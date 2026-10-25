#' Generate the synthetic household inflation scenario grid
#'
#' Builds a fully crossed grid of hypothetical household profiles and assumed
#' inflation rates, then projects each fixed basket with [simulate_budget()].
#' The default grid has 2 profiles x 10 spending levels x 10 food rates x
#' 10 non-food rates x 5 horizons = 10,000 unique scenarios. Food shares are
#' the HCES 2023-24 benchmarks for each profile.
#'
#' These rows are calculated, synthetic scenarios. They are not observed
#' household records and not official CPI data.
#'
#' @param spend Monthly household spending levels in INR.
#' @param food_rates Assumed annual food inflation rates (percent).
#' @param nonfood_rates Assumed annual non-food inflation rates (percent).
#' @param years Projection horizons in years.
#' @param budget_ratio Available monthly budget as a multiple of current
#'   spending (1 means income stays flat).
#' @return A data frame with one row per scenario.
#' @examples
#' s <- generate_scenarios(spend = c(10000, 20000), food_rates = c(4, 8),
#'                         nonfood_rates = 4, years = 1:2)
#' head(s)
#' @export
generate_scenarios <- function(spend = seq(10000, 55000, by = 5000),
                               food_rates = seq(2, 20, by = 2),
                               nonfood_rates = seq(1, 10, by = 1),
                               years = 1:5, budget_ratio = 1.10) {
  if (!is.numeric(budget_ratio) || length(budget_ratio) != 1 || !is.finite(budget_ratio) || budget_ratio < 0)
    stop("budget_ratio must be a non-negative number")
  h <- get_static_data("hces")
  grid <- expand.grid(years = years, nonfood_inflation = nonfood_rates, food_inflation = food_rates,
                      monthly_spend = spend, profile = c("Rural", "Urban"),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid <- grid[, rev(names(grid))]
  food_share <- vapply(grid$profile, function(p) h$share[h$sector == p & h$category == "Food"], numeric(1))
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    w <- c(Food = food_share[[i]], `Non-food` = 1 - food_share[[i]])
    r <- c(Food = grid$food_inflation[i], `Non-food` = grid$nonfood_inflation[i])
    budget <- grid$monthly_spend[i] * budget_ratio
    s <- simulate_budget(grid$monthly_spend[i], w, r, grid$years[i], budget)
    c(food_share = w[["Food"]], basket_inflation = sum(w * r), projected_cost = s$projected,
      extra_cost = s$extra, cost_change_pct = s$change_pct, budget = budget, budget_gap = s$budget_gap)
  })
  out <- cbind(scenario_id = seq_len(nrow(grid)), grid, as.data.frame(do.call(rbind, rows)))
  out$shortfall <- out$budget_gap < 0
  out$data_type <- "Synthetic scenario"
  num <- c("projected_cost", "extra_cost", "budget", "budget_gap")
  out[num] <- lapply(out[num], round, 2)
  out[c("basket_inflation", "cost_change_pct")] <- lapply(out[c("basket_inflation", "cost_change_pct")], round, 4)
  rownames(out) <- NULL
  out
}

#' Load the bundled 10,000-row household scenario dataset
#'
#' @param path Optional path to a scenario CSV with the same columns. When
#'   `NULL`, the bundled file is used.
#' @return A validated data frame of synthetic scenarios.
#' @examples
#' s <- get_scenario_data()
#' nrow(s)
#' @export
get_scenario_data <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "inflascope_10000_household_scenarios.csv", package = "inflascope")
    if (!nzchar(path)) stop("Bundled scenario dataset missing from installed package")
  }
  if (!file.exists(path)) stop("Scenario CSV does not exist")
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  validate_scenario_data(x)
  x$shortfall <- as.logical(x$shortfall)
  x
}

#' Validate a household scenario table
#'
#' @param data Data frame with the scenario columns described in
#'   `docs/data-dictionary.md`.
#' @return `TRUE`, or an error describing the problem.
#' @examples
#' validate_scenario_data(get_scenario_data())
#' @export
validate_scenario_data <- function(data) {
  cols <- c("scenario_id", "profile", "monthly_spend", "food_inflation", "nonfood_inflation", "years",
            "food_share", "basket_inflation", "projected_cost", "extra_cost", "budget", "budget_gap")
  if (!is.data.frame(data)) stop("Scenario data must be a data frame")
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop("Missing scenario columns: ", paste(miss, collapse = ", "))
  if (!nrow(data)) stop("Scenario data must not be empty")
  if (!all(data$profile %in% c("Rural", "Urban"))) stop("Scenario profile must be Rural or Urban")
  num <- setdiff(cols, "profile")
  bad <- num[!vapply(data[num], function(v) is.numeric(v) && all(is.finite(v)), logical(1))]
  if (length(bad)) stop("Non-numeric or missing values in: ", paste(bad, collapse = ", "))
  if (anyDuplicated(data$scenario_id)) stop("Duplicate scenario_id values")
  if (any(data$monthly_spend < 0) || any(data$years < 0)) stop("Spending and years must be non-negative")
  if (any(data$food_share < 0 | data$food_share > 1)) stop("food_share must be between 0 and 1")
  TRUE
}

#' Summarise household scenarios by group
#'
#' @param data Scenario data, by default [get_scenario_data()].
#' @param by Grouping columns.
#' @return Data frame with scenario counts, mean extra cost, mean cost change
#'   and the share of scenarios with a budget shortfall.
#' @examples
#' summarise_scenarios(by = c("profile", "years"))
#' @export
summarise_scenarios <- function(data = get_scenario_data(), by = c("profile", "years")) {
  validate_scenario_data(data)
  if (!all(by %in% names(data))) stop("Unknown grouping column")
  key <- interaction(data[by], drop = TRUE, lex.order = TRUE)
  parts <- split(data, key)
  out <- do.call(rbind, lapply(parts, function(d) {
    data.frame(d[1, by, drop = FALSE], scenarios = nrow(d),
               mean_basket_inflation = mean(d$basket_inflation),
               mean_extra_cost = mean(d$extra_cost),
               median_budget_gap = stats::median(d$budget_gap),
               shortfall_pct = 100 * mean(d$budget_gap < 0), check.names = FALSE)
  }))
  rownames(out) <- NULL
  out
}
