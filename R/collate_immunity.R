## collate immunity for one age group
collate_immunity <- function(df) {
  vaccinated_susceptible <- 0 ## proportion vaccinated but still susceptible
  immune <- 0 ## proportion immune
  ## at any time:
  ## vaccinated is: vaccinated_susceptible + immune
  ## unvaccinated_susceptible is: 1 - vaccinated_susceptible - immune

  added_immunity <- c()
  added_vacc_susceptibility <- c()

  for (row_nb in seq_len(nrow(df))) {
    adj_coverage <- unname(unlist(df[row_nb, "adjusted_coverage"]))
    efficacy <- unname(unlist(df[row_nb, "efficacy"]))

    ## doses going to people already vaccinated
    to_vaccinated <- min(adj_coverage, vaccinated_susceptible + immune)
    ## doses going to unvaccinated
    to_unvaccinated <- adj_coverage - to_vaccinated

    new_immune <-
      efficacy * (
        ## newly immunised and had been vaccinated before
        to_vaccinated * vaccinated_susceptible +
        ## newly immunised and had not been vaccinated before
        to_unvaccinated
      )

    vaccinated_susceptible <- vaccinated_susceptible +
      to_unvaccinated - new_immune
    immune <- immune + new_immune

    added_immunity <- c(added_immunity, new_immune)
    added_vacc_susceptibility <- c(
      added_vacc_susceptibility, to_unvaccinated - new_immune
    )
  }

  return(data.frame(
    immune = added_immunity,
    vaccinated_susceptible = added_vacc_susceptibility,
    type = df$type,
    stringsAsFactors = FALSE
  ))
}
