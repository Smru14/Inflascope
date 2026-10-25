## ----setup, message = FALSE---------------------------------------------------
library(inflascope)

## -----------------------------------------------------------------------------
cpi <- get_cpi_data("static")
get_latest_cpi(cpi)[, c("date", "sector", "index", "inflation_yoy")]
cmp <- compare_rural_urban(data = cpi)
generate_insights(cmp)
compare_rural_urban(data = cpi, category = "Food and beverages")$difference
head(get_cpi_detail(state = "Tamil Nadu", sector = "Rural", category = "General"))

## -----------------------------------------------------------------------------
personal_inflation(c(Food = .47, `Non-food` = .53), c(Food = 8, `Non-food` = 4))$inflation_pct
s <- simulate_budget(25000, c(Food = .47, `Non-food` = .53), c(Food = 8, `Non-food` = 4),
                     years = 2, budget = 28000)
s$projected
s$budget_gap

## -----------------------------------------------------------------------------
scen <- get_scenario_data()
nrow(scen)
summarise_scenarios(scen, by = c("profile", "years"))

