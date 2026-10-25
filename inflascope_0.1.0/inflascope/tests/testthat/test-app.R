test_that("Shiny dashboard renders every tab's outputs", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- shiny::shinyAppDir(system.file("shiny", package = "inflascope"))
  shiny::testServer(app, {
    session$setInputs(data_mode = "static", state = "All India", category = "General", state_month = "2026-09-01", sector = c("Rural", "Urban"), measure = "inflation_yoy",
      range = c("2026-01-01", "2026-09-01"), profile = "Rural", monthly = 25000, food_pct = 47, food_rate = 6,
      other_rate = 4, stress_profile = "Rural", stress_spend = 25000, available = 28000, horizon = 3,
      scenario = "Moderate", custom_food = 8, custom_other = 5, sc_profile = c("Rural", "Urban"),
      sc_spend = c(10000, 55000), sc_food = c(2, 20), sc_nonfood = c(1, 10), sc_years = as.character(1:5),
      forecast_sector = "Rural", forecast_horizon = 6, forecast_model = "arima")
    expect_equal(output$rural_kpi, "6.08%")
    expect_equal(output$gap_kpi, "+1.20 pp")
    expect_equal(output$sc_n, "10,000")
    for (o in c("trend", "historic_plot", "hces_share", "contrib_plot", "burden_plot", "stress_plot",
                "sc_heat", "sc_years", "sc_spend", "sc_table", "all_data", "provenance", "stress_table", "sc_summary",
                "trend_index", "division_plot", "state_plot", "forecast_plot", "historic_table"))
      expect_false(is.null(output[[o]]), info = o)
    expect_false(inherits(forecast_result(), "error"))
    expect_equal(nrow(forecast_result()), 6)
  })
})
