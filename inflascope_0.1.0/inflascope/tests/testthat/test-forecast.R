test_that("forecasting works on a continuous monthly series", {
  d <- synthetic_cpi(36)
  f <- forecast_inflation(d, "Rural", horizon = 6)
  expect_equal(nrow(f), 6)
  expect_equal(f$date[1], as.Date("2027-01-01"))
  expect_true(all(f$lower95 <= f$cpi & f$cpi <= f$upper95))
  sn <- forecast_inflation(d, "Urban", horizon = 3, model = "snaive")
  expect_equal(nrow(sn), 3)
  e <- evaluate_forecast(d, "Rural", holdout = 6)
  expect_equal(e$model, c("ARIMA", "Seasonal naive"))
  expect_true(all(e$rmse >= e$mae))
})

test_that("forecasting refuses short or broken series", {
  expect_error(forecast_inflation(synthetic_cpi(12)), "18 consecutive")
  d <- synthetic_cpi(36); d <- d[-5, ]
  expect_error(forecast_inflation(d, "Rural"), "missing months")
  expect_error(forecast_inflation(synthetic_cpi(36), horizon = 0), "Horizon")
  expect_error(evaluate_forecast(synthetic_cpi(15), holdout = 6), "training")
})
