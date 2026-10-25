# Data dictionary and safe import

`inst/extdata/cpi_snapshot.csv`: `date` first day of month (`YYYY-MM-01`); `sector` Rural/Urban/Combined; `category` General; `index` CPI level (positive); `inflation_yoy` officially published YoY percentage (NA accepted for unpublished); `base_year` literal `2024=100`; `release_date` ISO date; `source_url` official HTTPS page; `status` final/provisional; `dataset_version` release identifier.

Each row must be attributable to an official release. Values may change from provisional to final. Selected snapshot reference months are not a contiguous historical series. Provide a **complete, same-base, consecutive monthly** CPI CSV for reliable time-series forecasting.

`inst/extdata/hces_snapshot.csv`: `period`, `sector`, `mpce_inr` monthly *per capita* expenditure, `category` Food/Non-food, `share` fraction, `imputation` excluded, `geography` All India, `source_url`, `dataset_version`.

Importing a canonical CSV validates structural integrity and prevents mixing base years. It does not independently establish that a supplied file is authentically from MoSPI. The user must verify official provenance before import.
