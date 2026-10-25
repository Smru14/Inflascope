# The 2024-base series starts in January 2025, so the minimum is set to 18
# months. Seasonal ARIMA terms are only considered with 36 or more months.
.min_forecast_months <- 18L
.min_training_months <- 12L

.cpi_ts <- function(data, sector) {
  validate_cpi_data(data)
  x <- data[data$sector == sector & data$category == "General",,drop=FALSE]
  x <- x[order(x$date),,drop=FALSE]
  if (anyDuplicated(x$date)) stop("Duplicate months")
  if (nrow(x)<2 || any(!is.finite(x$index))) stop("Missing CPI indices")
  months <- as.integer(format(x$date,"%Y"))*12L+as.integer(format(x$date,"%m"))
  if (any(diff(months)!=1L)) stop("Series has missing months: cannot fit regular monthly model")
  stats::ts(x$index, start=c(as.integer(format(x$date[1],"%Y")),as.integer(format(x$date[1],"%m"))),frequency=12)
}

#' Forecast future CPI index levels, never annual inflation rates
#' @param data Validated, continuous monthly CPI data frame (at least 18 months).
#' @param sector Rural or Urban.
#' @param horizon Number of months ahead.
#' @param model One of `arima` or `snaive`.
#' @return Data frame of CPI point estimates and 80/95 percent intervals.
#' @examples
#' forecast_inflation(get_static_data("cpi"), "Rural", horizon = 3)
#' # A synthetic series works too:
#' cpi <- data.frame(date = seq(as.Date("2024-01-01"), by = "month", length.out = 30),
#'   sector = "Rural", category = "General", index = 100 + 0.4 * (1:30),
#'   inflation_yoy = NA_real_, base_year = "2024=100", release_date = "2026-10-01",
#'   source_url = "https://example.org/synthetic")
#' forecast_inflation(cpi, "Rural", horizon = 3)
#' @export
forecast_inflation <- function(data=get_cpi_data(),sector=c("Rural","Urban"),horizon=6L,model=c("arima","snaive")) {
  sector <- match.arg(sector); model <- match.arg(model)
  if (length(horizon)!=1 || !is.finite(horizon) || horizon<1 || horizon!=as.integer(horizon) || horizon>36) stop("Horizon must be 1 to 36 months")
  y <- .cpi_ts(data,sector)
  if (length(y)<.min_forecast_months) stop(sprintf("Need at least %d consecutive monthly observations to forecast; import a longer compatible official series", .min_forecast_months))
  fit <- if(model=="arima") forecast::auto.arima(y,seasonal=length(y)>=36) else forecast::snaive(y,h=horizon)
  fc <- if(model=="arima") forecast::forecast(fit,h=horizon,level=c(80,95)) else forecast::forecast(fit,level=c(80,95))
  last <- max(as.Date(data$date[data$sector==sector & data$category=="General"]))
  dates <- seq(as.Date(format(last,"%Y-%m-01")), by="month", length.out=horizon+1L)[-1]
  data.frame(date=dates,sector=sector,model=model,cpi=as.numeric(fc$mean),
             lower80=as.numeric(fc$lower[,1]),upper80=as.numeric(fc$upper[,1]),
             lower95=as.numeric(fc$lower[,2]),upper95=as.numeric(fc$upper[,2]))
}

#' Evaluate CPI index forecasts on an untouched chronological holdout
#' @param data Validated continuous CPI data.
#' @param sector Rural or Urban.
#' @param holdout Length of test interval in months (default 6).
#' @return Performance table for ARIMA and seasonal naive benchmark.
#' @examples
#' cpi <- data.frame(date = seq(as.Date("2024-01-01"), by = "month", length.out = 30),
#'   sector = "Rural", category = "General", index = 100 + 0.4 * (1:30),
#'   inflation_yoy = NA_real_, base_year = "2024=100", release_date = "2026-10-01",
#'   source_url = "https://example.org/synthetic")
#' evaluate_forecast(cpi, "Rural", holdout = 6)
#' @export
evaluate_forecast <- function(data=get_cpi_data(),sector=c("Rural","Urban"),holdout=6L) {
  sector <- match.arg(sector)
  y <- .cpi_ts(data,sector)
  if (length(holdout)!=1 || !is.finite(holdout) || holdout<1 || holdout!=as.integer(holdout)) stop("Invalid holdout")
  if(length(y)-holdout<.min_training_months) stop(sprintf("Need >=%d training months plus a separate holdout", .min_training_months))
  n <- length(y); train <- stats::window(y,end=stats::time(y)[n-holdout]); actual <- utils::tail(as.numeric(y),holdout)
  ar <- as.numeric(forecast::forecast(forecast::auto.arima(train,seasonal=length(train)>=36),h=holdout)$mean)
  sn <- as.numeric(forecast::snaive(train,h=holdout)$mean)
  metrics <- function(pred,model) data.frame(model=model,mae=mean(abs(actual-pred)),rmse=sqrt(mean((actual-pred)^2)),
    train_months=length(train),test_months=holdout,forecast_horizon=holdout)
  rbind(metrics(ar,"ARIMA"),metrics(sn,"Seasonal naive"))
}
