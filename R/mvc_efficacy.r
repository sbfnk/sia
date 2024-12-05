##' MVC efficacy
##'
## If age < 6 it's 0, if 6-9 months it's 0.75, if 9-12 months it's 0.85, if
## older it's 0.95
##' @param age age of individual (in years)
##' @importFrom dplyr case_when
##' @return efficacy of MCV
mcv_efficacy <- function(age) {
  eff <- case_when(
    age < 0.5 ~ 0,
    age < 0.75 ~ 0.75,
    age < 1 ~ 0.85,
    .default = 0.95
  )
  return(eff)
}
