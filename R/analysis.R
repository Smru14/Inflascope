#' Calculate percentage change in an index
#' @param previous Positive earlier index level.
#' @param current Positive later index level.
#' @param period Text label; caller is responsible for appropriate lag.
#' @return Numeric percentage change.
#' @export
calculate_inflation <- function(previous, current, period = "specified period") {
  if (!is.numeric(previous) || !is.numeric(current) || length(previous)!=1L || length(current)!=1L ||
      !is.finite(previous) || !is.finite(current) || previous <= 0 || current <= 0) stop("Indices must be finite positive scalars")
  (current / previous - 1) * 100
}

#' Calculate fixed-basket inflation and contributions
#' @param weights Named non-negative weights summing to 1.
#' @param rates Named category-specific percentage changes for one consistent period.
#' @return A list with rate and per-category percentage-point contributions.
#' @export
personal_inflation <- function(weights, rates) {
  if (!is.numeric(weights) || !is.numeric(rates) || is.null(names(weights)) || is.null(names(rates)) ||
      anyDuplicated(names(weights)) || anyDuplicated(names(rates)) || any(!nzchar(names(weights))) ||
      any(!nzchar(names(rates))) || !setequal(names(weights), names(rates)) ||
      any(!is.finite(weights)) || any(!is.finite(rates)) || any(weights < 0) ||
      abs(sum(weights)-1) > 1e-8) stop("Require named, matching finite categories, with non-negative weights summing to 1")
  rates <- rates[names(weights)]
  contributions <- weights * rates
  list(inflation_pct = sum(contributions), contributions = contributions,
       weights = weights, rates = rates, type = "Fixed-weight hypothetical basket")
}

#' Simulate monthly cost of maintaining a fixed consumption basket
#' @param expenditure Current monthly spending in INR.
#' @param weights Named category shares summing to one.
#' @param rates Named assumed annual category inflation rates in percentage units.
#' @param years Simulation horizon, in years.
#' @param budget Optional monthly available budget in INR.
#' @return List of projected category and total costs and budget gap.
#' @export
simulate_budget <- function(expenditure, weights, rates, years = 1, budget = NULL) {
  if (!is.numeric(expenditure) || length(expenditure)!=1 || !is.finite(expenditure) || expenditure < 0) stop("Expenditure must be a non-negative number")
  if (!is.numeric(years) || length(years)!=1 || !is.finite(years) || years < 0) stop("Years must be non-negative")
  if (!is.null(budget) && (!is.numeric(budget) || length(budget)!=1 || !is.finite(budget) || budget < 0)) stop("Budget must be non-negative")
  b <- personal_inflation(weights,rates)
  if (any(b$rates <= -100)) stop("Annual category inflation cannot be -100% or lower")
  amounts <- expenditure * b$weights
  future <- amounts * (1 + b$rates/100)^years
  total <- sum(future)
  list(original = expenditure, projected = total, extra = total-expenditure,
       change_pct = if (expenditure==0) NA_real_ else 100*(total/expenditure-1),
       budget_gap = if (is.null(budget)) NA_real_ else budget-total,
       category = data.frame(category=names(amounts), current=as.numeric(amounts),
                             projected=as.numeric(future)),
       assumptions = "Annual category price changes compounded; quantities and weights fixed; hypothetical")
}

#' Compare compatible rural and urban CPI observations
#' @param date Optional first-of-month Date; defaults to most recent paired period.
#' @param data CPI dataset.
#' @param category CPI category, by default General.
#' @param measure Either published year-on-year inflation or index level.
#' @return List with both values, differences and provenance.
#' @export
compare_rural_urban <- function(date=NULL, data=get_cpi_data(), category="General", measure=c("inflation_yoy","index")) {
  measure <- match.arg(measure); validate_cpi_data(data)
  x <- data[data$category == category & data$sector %in% c("Rural","Urban"),,drop=FALSE]
  if (is.null(date)) {
    paired <- intersect(x$date[x$sector == "Rural"],x$date[x$sector == "Urban"])
    if (!length(paired)) stop("No paired rural-urban period")
    date <- max(paired)
  }
  x <- x[x$date == as.Date(date),,drop=FALSE]
  if (nrow(x)!=2 || !setequal(x$sector,c("Rural","Urban")) || length(unique(x$base_year))!=1 || anyNA(x[[measure]])) stop("Missing or incompatible observations")
  rural <- x[[measure]][x$sector=="Rural"]; urban <- x[[measure]][x$sector=="Urban"]
  list(date = as.Date(date), measure = measure, rural = rural, urban = urban,
       difference = rural-urban, relative_pct = if (urban==0) NA_real_ else 100*(rural/urban-1),
       source_url = unique(x$source_url), base_year = unique(x$base_year))
}

#' Compare hypothetical fixed-basket burdens to published food expenditure weights
#' @param rates Named Food and Non-food rates in percentage units.
#' @param weights Optional personal Food and Non-food shares.
#' @return Personal and rural/urban modeled inflation, with contributions.
#' @export
inflation_burden <- function(rates, weights = c("Food"=.45,"Non-food"=.55)) {
  if (!setequal(names(rates),c("Food","Non-food"))) stop("Use Food and Non-food rates")
  h <- get_static_data("hces")
  calc <- function(s) personal_inflation(stats::setNames(h$share[h$sector==s],h$category[h$sector==s]), rates)
  a <- personal_inflation(weights,rates); rural <- calc("Rural"); urban <- calc("Urban")
  list(personal=a, rural=rural, urban=urban,
       personal_minus_rural=a$inflation_pct-rural$inflation_pct,
       personal_minus_urban=a$inflation_pct-urban$inflation_pct,
       note="Illustrative fixed-basket comparison, not an official inflation index or welfare estimate")
}

#' Generate traceable comparisons from calculated results
#' @param comparison Output from compare_rural_urban().
#' @return Concise descriptive statement.
#' @export
generate_insights <- function(comparison = compare_rural_urban()) {
  who <- if (comparison$difference > 0) "Rural" else if (comparison$difference < 0) "Urban" else "Neither"
  if (who == "Neither") return(sprintf("Rural and urban values were equal in %s.", comparison$date))
  sprintf("%s %s was higher by %.2f %s in %s (same CPI base).",
          who, if (comparison$measure=="inflation_yoy") "year-on-year inflation" else "CPI index",
          abs(comparison$difference), if (comparison$measure=="inflation_yoy") "percentage points" else "index points", comparison$date)
}
