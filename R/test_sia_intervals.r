##' Test SIA intervals
##'
##' .. content for \details{} ..
##' @title
##' @param row
##' @return
##' @importFrom epimixr adjust_immunity
##' @importFrom socialmixr wpp_age
##' @importFrom dplyr select starts_with bind_rows
##' @importFrom tibble tibble
##' @author Sebastian Funk
test_sia_intervals <- function(row, ...) {
  cat(row$country, " ", row$year, "\n")
  immunity <- row |>
    select(starts_with("immune_")) |>
    unlist()
  vaccinated_susceptible <- row |>
    select(starts_with("vaccinated_susceptible_")) |>
    unlist()

  matrix <- row$matrix[[1]]

  age_dist <- wpp_age(row$country, row$year)

  if (nrow(age_dist) > 0) {
    age.limits <-
      as.integer(sub(
        "^[^0-9]*([0-9]+)[,+].*$", "\\1",
        rownames(matrix)
      ))
    adjImm <- lapply(seq_len(max_age), function(shift) {
      immune <- c(
        immunity_profile(row, shift = shift, ...)$immune, older_immunity
      )
      imm <- tibble(
        region = row$region,
        iso3 = row$iso3,
        country = row$country,
        year = row$year,
        interval = shift,
        adjImm = adjust_immunity(matrix, immunity = immune)
      )
    })
    return(bind_rows(adjImm))
  } else {
    return(tibble(
      region = character(0),
      iso3 = character(0),
      country = character(0),
      year = integer(0),
      interval = integer(0),
      adjImm = numeric(0)
    ))
  }
}
