# Run from any R working directory after adjusting folder_path.
folder_path <- "C:/Users/YOUR_USERNAME/Downloads"
scenario_path <- file.path(folder_path, "inflascope_10000_household_scenarios.csv")
cpi_path <- file.path(folder_path, "inflascope_cpi_snapshot.csv")
scenarios <- read.csv(scenario_path, stringsAsFactors = FALSE)
stopifnot(nrow(scenarios) == 10000L,
          !anyDuplicated(scenarios$scenario_id),
          all(scenarios$data_type == "HYPOTHETICAL_FIXED_BASKET_SCENARIO"))
# Scenario file is NOT a CPI dataset and must NOT be passed to import_cpi_csv().
print(aggregate(additional_monthly_cost_inr ~ sector, scenarios, mean))
print(head(scenarios))
library(inflascope)
observations <- import_cpi_csv(cpi_path)
stopifnot(isTRUE(validate_cpi_data(observations)))
print(get_latest_cpi(observations))
# This short CPI snapshot is not sufficient to fit a defensible ARIMA model.
