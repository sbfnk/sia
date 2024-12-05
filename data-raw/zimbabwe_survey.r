library("socialmixr")

zimbabwe_survey <-
  get_survey("https://doi.org/10.5281/zenodo.3886638")
zimbabwe_survey$participants[, country := "Zimbabwe"]

usethis::use_data(zimbabwe_survey, overwrite = TRUE)
