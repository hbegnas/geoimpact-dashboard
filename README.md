# GEO IMPACT MPC Quarterly Dashboard

R Shiny dashboard for monthly aggregate MPC data, fed back quarterly.

## Run locally
1. Open `geoimpact-dashboard.Rproj` in RStudio
2. `renv::restore()` (first time only)
3. `source("make_mock_data.R")` (creates MOCK data in `data/`)
4. `shiny::runApp()`

## Data
`data/*.csv` are long-format tables (see `make_mock_data.R` for columns).
Mock data only until the real Yale aggregate reports are mapped and approved for use.

## Workflow
Never commit to `main` directly: branch, open a pull request, have a teammate review, merge.
