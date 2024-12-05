library("here")
library("readr")
library("socialmixr")
library("dplyr")
library("tidyr")

download_dir <- here::here("data-raw", "downloads")
dir.create(download_dir, showWarnings = FALSE)

## Uganda survey

url <- paste0(
  "https://static-content.springer.com/esm/",
  "art%3A10.1186%2Fs12879-018-3073-1/MediaObjects/",
  "12879_2018_3073_MOESM4_ESM.csv"
)
filename <- file.path(download_dir, "uganda_survey.csv")
download.file(url, filename, method = "auto")

uganda_survey <- read_csv(filename, show_col_type = FALSE)

clean_age <- function(df, from_column, to_column) {
  min_column <- paste(to_column, "est_min", sep = "_")
  max_column <- paste(to_column, "est_max", sep = "_")
  df |>
    mutate(
      "{{ from_column }}" := sub(
        "y olds$", "", {{ from_column }}, ignore.case = TRUE
      ),
      "{{ from_column }}" := sub(
        "^<", "0-", {{ from_column }}
      ),
      "{{ from_column }}" := sub(
        "\\+$", "-", {{ from_column }}
      )
    ) |>
    separate(
      {{ from_column }},
      into = paste(to_column, c("est_min", "est_max"), sep = "_"),
      sep = "-"
    ) |>
    mutate(
      "{ min_column }" := as.integer(!!sym(min_column)),
      "{ max_column }" := as.integer(!!sym(max_column))
    )
}

participants <- uganda_survey |>
  select(
    participant_id, day_surveyed, agecat_participant, Rural
  ) |>
  distinct() |>
  mutate(
    setting = case_match(
      Rural,
      "No" ~ "urban",
      "Yes" ~ "rural"
    ),
    dayofweek = case_match(
      day_surveyed,
      "Monday" ~ 1L,
      "Tuesday" ~ 2L,
      "Wednesday" ~ 3L,
      "Thursday" ~ 4L,
      "Friday" ~ 5L,
      "Saturday" ~ 6L,
      "Sunday" ~ 7L
    )
  ) |>
  clean_age(agecat_participant, "part_age") |>
  select(-Rural, -day_surveyed) |>
  mutate(country = "Uganda")

contacts <- uganda_survey |>
  select(-day_surveyed, -agecat_participant, -Rural) |>
  clean_age(agecat_contact, "cnt_age")

citation <- RefManageR::GetBibEntryWithDOI("10.1186/s12879-018-3073-1")

survey_data <- list(
  participants = participants,
  contacts = contacts,
  reference = citation
)

uganda_survey <- as_contact_survey(
  survey_data,
  participant_id = "participant_id",
  year = NULL
)

usethis::use_data(uganda_survey, overwrite = TRUE)
