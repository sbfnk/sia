library("readxl")
library("magrittr")
library("dplyr")
library("tidyr")
library("purrrlyr")
library("purrr")
library("cowplot")
library("lubridate")
library("readr")

## Function to calculate immunity profile for an SIA row
immunity_profile <- function(row, shift = 0, mcv_coverage,
                             schedule, sias, max_age) {
  ## Work out routine coverage
  country_schedule <- schedule |>
    filter(iso3 == row$iso3) ## look up country schedule
  age_mcv <- c(country_schedule$age_mcv1, country_schedule$age_mcv2)
  profile_year <- row$year + shift
  relevant_years <- seq(profile_year, profile_year - max_age + 1)

  coverage <- list()

  for (dose in which(!is.na(age_mcv))) {
    ## indicator of age groups that benefit from MCV1/2
    dose_type <- paste0("MCV", dose)
    min_dose_age <- floor(age_mcv[dose])

    if (max_age >= floor(age_mcv[dose])) {
      country_dose <- mcv_coverage[[dose]] |>
        filter(iso3 == row$iso3) |>
        select(iso3, year, coverage)

      coverage[[dose_type]] <- ## build coverage table
        tibble(
          iso3 = row$iso3, ## country
          year = relevant_years
        ) |>
        full_join(country_dose |> filter(year <= row$year),
          by = c("iso3", "year")
        ) |> ## bring in coverage
        mutate(
          age = seq(0, n() - 1), ## current age
          age.at.time = age_mcv[dose], ## age at time of vaccination
          fraction = if_else(age == min_dose_age,
            1 - (age_mcv[dose] - min_dose_age), 1
          )
        ) |>
        mutate(early.na = cumsum(!is.na(coverage))) |>
        mutate(coverage = if_else(early.na == 0,
          dplyr::first(coverage[early.na > 0]), coverage
        )) |>
        select(-early.na) |>
        filter(age < max_age) |>
        mutate(coverage = if_else(age < age.at.time, NA_real_, coverage)) |>
        mutate(
          coverage = coverage / 100,
          type = dose_type
        ) ## coverage in decimals
    }
  }

  ## work out immunity from SIAs
  country_sias <- sias |>
    filter(
      iso3 == row$iso3,
      between(year, profile_year - max_age + 1, row$year)
    ) |> ## look SIAs in the last max_age years
    arrange(year) ## sort by ascending year

  if (shift > 0) {
    country_sias <- country_sias |>
      rbind(country_sias[nrow(country_sias), ] |> mutate(year = year + shift))
  }

  ## loop over rows and add to immunity
  for (row_nb in seq_len(nrow(country_sias))) {
    sia_row <- country_sias[row_nb, ]
    dose_type <- paste0("SIA", row_nb)
    ## shift upper and lower age by when SIA happened
    lower_age <- sia_row$lower_age + profile_year - sia_row$year
    upper_age <- min(max_age, sia_row$upper_age + profile_year - sia_row$year)

    if (lower_age < max_age) {
      coverage[[dose_type]] <- ## build coverage table
        tibble(
          iso3 = row$iso3,
          year = sia_row$year,
          coverage = sia_row$coverage,
          age = seq(floor(lower_age), upper_age - 1), ## current age
          age.at.time = age - profile_year + sia_row$year
        ) |> ## efficacy of upper age by default
        ## year in which to look up vaccination coverage
        mutate(
          age.at.time = if_else(
            age.at.time == 0, sia_row$lower_age, as.numeric(age.at.time)
          ),
          fraction = if_else(
            age == floor(lower_age), 1 - (lower_age - floor(lower_age)), 1
          ),
          type = dose_type
        ) ## type of dose
    }
  }

  ## put it all together, for each age group
  collated_coverage <- coverage |>
    bind_rows() |> ## combine all coverages into a single table
    mutate(
      ## efficacy depends on age at vaccination
      efficacy = mcv_efficacy(age.at.time),
      adjusted_coverage = coverage * fraction
    ) |> ## coverage adjusted by fractional age group
    ## sort by age.at.time so vaccination is processed in correct order
    arrange(iso3, age, age.at.time) |>
    ## set adjusted coverage of NA to zero
    replace_na(list(adjusted_coverage = 0)) |>
    group_by(iso3, age) |>
    nest() |>
    mutate(immunity = map(data, collate_immunity)) |>
    unnest(cols = immunity) |>
    group_by(iso3, age) |>
    summarise(
      immune = sum(immune),
      vaccinated_susceptible = sum(vaccinated_susceptible),
      .groups = "drop"
    ) |>
    mutate(immune = immune + if_else(age == 0, 0.5, 0)) |> ## maternal immunity
    select(age, immune, vaccinated_susceptible)

  return(collated_coverage)
}

