library("socialmixr")

peru_survey <- get_survey("https://doi.org/10.5281/zenodo.3874805")

usethis::use_data(peru_survey, overwrite = TRUE)
