options(echo = TRUE)
library("here")

source(here::here("inst", "snippets", "load_data.r"))
source(here::here("inst", "snippets", "prepare_surveys.r"))

library("docopt")

doc <-
  "Script for evaluating the reproduction number for different immunity level
 scenarios

 Usage: sia.r [-m <age>] [-i <immunity>] [-n <number>] [-o <filename>] [-p] [-h]

 Options:
   -n --nsamples=<number>            number of samples [default: 1]
   -o --output=<filename>            output file name
   -i --immunity=<immunity>          immunity in older age groups [default: 1]
   -m --maxage=<age>                 maximum age [default: 10]
   -p --polymod                      whether to use polymod for everything
   -e --empirical                    use empirical contact data
   -c --country                      country to use as iso3 code [default: all]
   -h --help                         show this message"

opts <- docopt(doc)

if (opts[["help"]]) {
  print(opts)
  exit()
}

older_immunity <- as.numeric(opts[["immunity"]])
max_age <- as.integer(opts[["maxage"]])
only_polymod <- opts[["polymod"]]
empirical <- opts[["empirical"]]
country <- opts[["empirical"]]

if (country != "all") {
  sias <- sias |>
    filter(iso3 == {{ country }})
  schedule <- schedule |>
    filter(iso3 == {{ country }})
  mcv_coverage <- lapply(mcv_coverage, function(x) {
    x |>
      filter(iso3 == {{ country }})
  })
}

res_dir <- here::here("res")
dropbox_dir <- here::here("data", "dropbox")

nsamples <- as.integer(opts[["nsamples"]])
output_file <- opts[["output"]]

age_limits <- seq(0, max_age)

datasets <- list(
  polymod, uganda_survey, smili_survey, zimbabwe_survey, peru_survey
)

if (empirical) {
  surveys <- list()
  for (id in seq_along(datasets)) {
    surveys[[id]] <- tibble(
      country = unique(datasets[[id]]$participants$country),
      dataset_id = id
    )
  }
  surveys <- surveys |>
    bind_rows() |>
    mutate(country = countrycode(country, "country.name.en", "country.name.en"))
}

## build up immunity profiles, line by line for each SIA after 2011
all_immunity_profiles <- sias |>
  mutate(id = seq_len(n())) |>
  nest(.by = id) |>
  mutate(
    res = map(
      data, immunity_profile, mcv_coverage = mcv_coverage,
      schedule = schedule, sias = sias, max_age = max_age
    )
  ) |>
  unnest(data) |>
  unnest(res) |>
  pivot_longer(c(immune, vaccinated_susceptible)) |>
  mutate(name = paste(name, age, sep = "_")) |>
  select(-age) |>
  pivot_wider() |>
  arrange(country, -year)

immunity_table <- all_immunity_profiles |>
  select(
    region, iso3, country, year, lower_age, upper_age, coverage,
    starts_with("immune")
  ) |>
  pivot_longer(
    starts_with("immune"), names_to = "var", values_to = "immunity"
  ) |>
  mutate(
    var = sub("^immune_", "", var),
    immunity = signif(immunity, 2)
  ) |>
  pivot_wider(names_from = "var", values_from = "immunity") |>
  arrange(iso3, -year)

immunity_profiles <- all_immunity_profiles |>
  mutate(country = countrycode(iso3, "iso3c", "country.name.en"))

if (only_polymod) {
  immunity_profiles <- immunity_profiles |>
    mutate(survey = country)
  country_matrices <- immunity_profiles |>
    filter(!duplicated(country)) |>
    rowwise() |>
    mutate(
      matrix = replicate(
        n = nsamples,
        contact_matrix(
          symmetric = TRUE, weigh.dayofweek = TRUE,
          sample.all.age.groups = TRUE, estimated.participant.age = "sample",
          estimated.contact.age = "sample", survey = polymod, split = TRUE,
          age.limits = age_limits, sample.participants = TRUE
        )
      )["matrix", ]
    ) |>
    select(survey, matrix)
} else {
  immunity_profiles <- immunity_profiles |>
    mutate(
      survey = if_else(country %in% surveys$country, country, NA_character_)
    ) |>
    left_join(africa_countries, by = "country") |>
    mutate(
      survey = if_else(region == "AFR" & subregion == "east", "Uganda", survey)
    ) |>
    mutate(
      survey = if_else(
        region == "AFR" & subregion == "south",
        "Zimbabwe",
        survey
      )
    ) |>
    mutate(survey = if_else(country %in% south_america, "Peru", survey)) |>
    filter(!is.na(survey)) |>
    mutate(id = seq_len(n())) |>
    left_join(surveys |> rename(survey = country), by = "survey")
  country_matrices <- surveys |>
    filter(country %in% immunity_profiles$survey) |>
    rowwise() |>
    mutate(
      matrix = list(replicate(
        n = nsamples,
        contact_matrix(
          symmetric = TRUE, weigh.dayofweek = TRUE,
          sample.all.age.groups = TRUE, estimated.participant.age = "sample",
          estimated.contact.age = "sample", survey = datasets[[dataset_id]],
          countries = country, split = TRUE, age.limits = age_limits,
          sample.participants = TRUE
        )
      )["matrix", ])
    ) |>
    rename(survey = country) |>
    select(survey, matrix)
}

immunity_profiles <- immunity_profiles |>
  left_join(country_matrices, by = "survey")

if (length(output_file) == 0) {
  output_file <- file.path(
    res_dir,
    paste0(
      "adjImm_sia_",
      if_else(only_polymod, "polymod_", ""),
      sub("\\.", "_", sprintf("%.2f", older_immunity)),
      ".rds"
    )
  )
} else if (length(output_file) > 0 && !grepl("\\.rds$", output_file)) {
  output_file <- paste0(output_file, ".rds")
}

adj_imm <- immunity_profiles |>
  unnest(matrix) |>
  mutate(scenario = seq_len(n())) |>
  nest(data = -scenario) |>
  mutate(
    immunity = map(
      data, test_sia_intervals, mcv_coverage = mcv_coverage,
      schedule = schedule, sias = sias, max_age = max_age
    )
  ) |>
  select(-data, -scenario) |>
  unnest(cols = immunity)

intervals <- adj_imm |>
  group_by(region, iso3, country, year, interval) |>
  summarise(adjImm = mean(adjImm), .groups = "drop") |>
  ungroup() |>
  filter(adjImm < 0.93 | interval == 10) |>
  group_by(iso3, year) |>
  filter(interval == min(interval)) |>
  ungroup() |>
  mutate(
    interval = if_else(adjImm >= 0.93, NA_integer_, interval - 1L),
    chr_interval = if_else(is.na(interval), "10+", as.character(interval))
  ) |>
  select(iso3, year, interval) |>
  arrange(iso3, -year)

res_filename <- paste0("funk_", gsub("-", "", as.character(today())), ".csv")
interval_file <- sub("\\.rds$", ".csv", output_file)

write_csv(intervals, interval_file)
