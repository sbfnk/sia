africa_geographies <-
  list(
    south = c(
      "Angola", "Botswana", "Lesotho", "Malawi", "Mozambique", "Namibia",
      "South Africa", "Swaziland", "Zambia", "Zimbabwe"
    ),
    east = c(
      "Tanzania", "Kenya", "Uganda", "Rwanda", "Burundi", "South Sudan",
      "Djibouti", "Eritrea", "Ethiopia", "Somalia"
    )
  )

africa_countries <-
  bind_rows(lapply(names(africa_geographies), function(x) {
    tibble(country = africa_geographies[[x]], subregion = x)
  }))

south_america <-
  c(
    "Argentina", "Belize", "Bolivia", "Brazil", "Chile", "Colombia", "Costa",
    "Rica", "Ecuador", "El Salvador", "Guatemala", "Guyana", "Haiti",
    "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
    "Suriname", "Uruguay"
  )
