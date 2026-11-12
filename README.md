# sia

An R package and analysis for assessing the need for measles Strategic Immunisation Activities (SIAs). It combines immunity profiles, built from vaccination coverage and SIA history, with contact matrices to estimate contact-adjusted immunity, and uses this to assess where SIAs are needed.

- `R/`: functions for immunity profiles, measles vaccine efficacy by age, collating immunity and testing SIA intervals.
- `data/` and `data-raw/`: contact survey data (including surveys from Peru, Uganda and Zimbabwe) and the scripts that prepare it.
- `inst/scripts/sia.r`: command-line script that evaluates the reproduction number under different immunity scenarios, with its options listed in the usage text at the top. It expects input data, such as WHO/UNICEF vaccination coverage estimates, under `data/external/` and `data/dropbox/`, which are not in the repository.
- `inst/snippets/`: data loading and survey preparation code sourced by the script.
- `res/`: results written by the script.

The package depends on epimixr, socialmixr, dplyr and tibble.
