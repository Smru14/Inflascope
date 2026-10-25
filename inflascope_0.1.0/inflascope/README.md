# INFLASCOPE INDIA

**An offline-first R package and Shiny dashboard for Indian rural–urban inflation analysis.**

## What works

- Official MoSPI CPI data (base 2024=100), January 2025 to September 2026: All India for 13 divisions plus the general index (`get_static_data("cpi")`), and all 36 states and UTs (`get_cpi_detail()`). The exact source file is kept as `datasets/cpi_1153.csv`.
- HCES 2023–24 rural and urban MPCE and food/non-food shares.
- CPI validation, rural–urban comparisons, personal fixed-basket estimates, budget scenarios.
- Forecasting of *CPI index levels* with ARIMA and seasonal naive, when at least 18 consecutive months of compatible observations are available (the bundled series has 21).
- A bundled set of 10,000 synthetic household inflation scenarios (`get_scenario_data()`), regenerable with `generate_scenarios()`.
- Offline-capable Shiny dashboard with nine tabs: Overview, Historical Explorer, Rural & Urban Consumption, Personal Inflation Calculator, Stress Test & Budget Resilience, Scenario Explorer, Forecasting Studio, Research & Data, and Export Report.
- Transactional local CSV import and cache fallback. No unverified automatic MoSPI API.

## Install (R 4.2+)

```r
install.packages(c("shiny", "bslib", "ggplot2", "plotly", "DT", "forecast"))
install.packages("inflascope_0.1.0.tar.gz", repos = NULL, type = "source")
library(inflascope)
launch_app()
```

Or from the source folder: `devtools::install()`.

After pushing the repository, use `remotes::install_github("YOUR_USERNAME/inflascope")`.

## R API example

```r
calculate_inflation(150, 159)
compare_rural_urban()
personal_inflation(c(Food=.47, `Non-food`=.53), c(Food=8, `Non-food`=4))
simulate_budget(25000, c(Food=.47, `Non-food`=.53), c(Food=8, `Non-food`=4), years=2, budget=30000)
get_data_metadata()
summarise_scenarios(get_scenario_data(), by = c("profile", "years"))
```

## Data caveats

The bundled CPI series is continuous for 21 months (January 2025 to September 2026). Year-on-year rates are published from January 2026 only, because the 2024 base starts in January 2025. Older CPI base 2012=100 indices are not appended to 2024=100 index levels. Forecasts on a 21-month series are indicative. CPI is an index of prices, whereas HCES MPCE describes expenditure: neither alone establishes causal changes in demand.

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

**State of verification (version 0.1.0):** `R CMD check` passes with 0 errors, 0 warnings and 0 notes on R 4.3.3; 88 test expectations pass, including a server test of every dashboard output. GitHub publishing and Shiny deployment have not been done yet.

## License

MIT. See `LICENSE.md`. Government-source attribution is preserved alongside observations.
