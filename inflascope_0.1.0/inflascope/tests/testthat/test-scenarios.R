test_that("bundled scenario dataset has 10,000 unique valid rows", {
  s <- get_scenario_data()
  expect_equal(nrow(s), 10000)
  expect_true(validate_scenario_data(s))
  key <- s[c("profile", "monthly_spend", "food_inflation", "nonfood_inflation", "years")]
  expect_equal(anyDuplicated(key), 0)
  expect_setequal(unique(s$profile), c("Rural", "Urban"))
})

test_that("scenario rows agree with simulate_budget", {
  s <- get_scenario_data()
  r <- s[s$scenario_id == 1234, ]
  sim <- simulate_budget(r$monthly_spend, c(Food = r$food_share, `Non-food` = 1 - r$food_share),
                         c(Food = r$food_inflation, `Non-food` = r$nonfood_inflation), r$years, r$budget)
  expect_equal(r$projected_cost, round(sim$projected, 2))
  expect_equal(r$budget_gap, round(sim$budget_gap, 2))
})

test_that("generator, validator and summary behave", {
  g <- generate_scenarios(spend = 20000, food_rates = c(5, 10), nonfood_rates = 4, years = 1:2)
  expect_equal(nrow(g), 2 * 2 * 2)
  expect_equal(g$food_share[g$profile == "Urban"][1], 0.3968)
  expect_error(validate_scenario_data(g[, -2]), "Missing scenario columns")
  bad <- g; bad$profile[1] <- "Metro"; expect_error(validate_scenario_data(bad), "Rural or Urban")
  sm <- summarise_scenarios(g, "profile")
  expect_equal(sm$scenarios, c(4, 4))
  expect_true(all(sm$shortfall_pct >= 0 & sm$shortfall_pct <= 100))
})
