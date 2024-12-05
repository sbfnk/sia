library("readxl")
library("tidyr")
library("dplyr")
library("readr")

external_dir <- here::here("data", "external")

mcv_coverage <- list() ## list of two elements, corresponding to the two doses
## 1) MCV1 coverage
mcv_coverage[[1]] <-
  read_xlsx(
    file.path(
      external_dir,
      "wuenic2023rev_web-update.xlsx"
    ),
    sheet = "MCV1"
  ) |> ## read file
  rename_with(tolower) |> ## lower case column names
  select(-starts_with("x__")) |> ## remove obsolete columns
  ## convert to long table
  pivot_longer(matches("^[1-2]"), names_to = "year", values_to = "coverage") |>
  mutate(year = as.integer(year)) |> ## convert year to integer
  replace_na(list(coverage = 0)) |> ## set missing coverage to 0
  arrange(country, -year) ## sort by country and (reverse) year

## 2) MCV2 coverage
mcv_coverage[[2]] <-
  read_xlsx(
    file.path(
      external_dir,
      "wuenic2023rev_web-update.xlsx"
    ),
    sheet = "MCV2"
  ) |> ## read file
  rename_with(tolower) |> ## lower case column names
  select(-starts_with("x__")) |> ## remove obsolete columns
  ## convert to long table
  pivot_longer(matches("^[1-2]"), names_to = "year", values_to = "coverage") |>
  mutate(year = as.integer(year)) |> ## convert year to integer
  replace_na(list(coverage = 0)) |> ## set missing coverage to 0
  arrange(country, -year) ## sort by country and (reverse) year

## 3) Vaccination schedules
schedule <-
  read_xlsx(
    file.path(external_dir, "CountryInfo2020.xlsx"), sheet = "COUNTRY20"
  ) |>
  rename_with(tolower) |> ## lower case column names
  rename(
    country = cname, iso3 = iso, age_mcv1 = `age@m1`, age_mcv2 = `age@m2`
  ) |>
  select(iso3, country, age_mcv1, age_mcv2) ## select relevant columns

## 4) SIAs
sias <- read_csv(
  file.path(external_dir, "DATA_SIA_ALL_COMPLETE-2024.csv"),
  show_col_types = FALSE
) |>
  rename_with(tolower) |> ## lower case column names
  rename(
    iso3 = iso_code, coverage = coverage_adj,
    lower_age = `lower age`, upper_age = `upper age`
  ) |> ## rename column name
  select(region, iso3, country, year, lower_age, upper_age, coverage) |>
  ## only use countries for which we have schedule information
  filter(iso3 %in% schedule$iso3) |>
  distinct() |> ## remove duplicate rows
  arrange(country, -year) ## sort by country and (reverse) year
