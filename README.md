# INFLASCOPE INDIA

**An offline-first R package and Shiny dashboard for Indian rural–urban inflation analysis.**

## What works

- Official-sourced, dated CPI snapshot (selected 2026 reference periods, base 2024=100) with rural, urban and combined observations.
- HCES 2023–24 rural and urban MPCE and food/non-food shares.
- CPI validation, rural–urban comparisons, personal fixed-basket estimates, budget scenarios.
- Forecasting of *CPI index levels* with ARIMA and seasonal naive, only when >=24 consecutive months of compatible observations are supplied.
- Offline-capable Shiny dashboard with data tables, plots, downloads and a computed HTML report.
- Transactional local CSV import and cache fallback. No unverified automatic MoSPI API.

## Install (R 4.2+)

```r
install.packages(c("remotes", "devtools", "roxygen2"))
# From this folder:
devtools::document()
devtools::install()
library(inflascope)
launch_app()
```

After pushing the repository, use `remotes::install_github("YOUR_USERNAME/inflascope")`.

## R API example

```r
calculate_inflation(150, 159)
compare_rural_urban()
personal_inflation(c(Food=.47, `Non-food`=.53), c(Food=8, `Non-food`=4))
simulate_budget(25000, c(Food=.47, `Non-food`=.53), c(Food=8, `Non-food`=4), years=2, budget=30000)
get_data_metadata()
```

## Data caveats

The bundled CPI observations are **selected, attributable reference months**, not a complete monthly time series. Missing months remain missing; the app refuses to forecast using this short discontinuous sample. Older CPI base 2012=100 indices are not appended to 2024=100 index levels. CPI is an index of prices, whereas HCES MPCE describes expenditure: neither alone establishes causal changes in demand.

For forecasting or updated data, download an official MoSPI dataset and convert it to the [canonical CSV schema](docs/data-dictionary.md), retaining URLs and source metadata; import using `import_cpi_csv("path.csv")` or the dashboard upload control. Only validated inputs are persisted.

## Official data sources

- [MoSPI e-Sankhyiki CPI](https://esankhyiki.mospi.gov.in/macroindicators?product=cpi)
- [September 2026 CPI press release (contains August final)](https://www.pib.gov.in/PressReleasePage.aspx?PRID=2322244&lang=1&reg=3)
- [August 2026 release (contains July final)](https://www.pib.gov.in/PressReleasePage.aspx?PRID=2310058&lang=1&reg=19)
- [July 2026 release (contains June final)](https://www.pib.gov.in/PressReleasePage.aspx?PRID=2298247&lang=2&reg=48)
- [HCES 2023–24 fact sheet](https://mospi.gov.in/sites/default/files/publication_reports/HCES%20FactSheet%202023-24.pdf)

## Tests and package checks

```r
devtools::document()
devtools::test()
devtools::check()
```

**State of verification:** The source tree was produced in a build environment without an installed R interpreter. Runtime R package checks, Shiny startup and external deployment must still be performed; no successful GitHub push or live app is claimed.

## License

MIT. See `LICENSE.md`. Government-source attribution is preserved alongside observations.
