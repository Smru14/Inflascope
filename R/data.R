#' Load the bundled, attributable official data snapshot
#' @param type One of `cpi` or `hces`.
#' @return A data.frame with original source metadata.
#' @export
get_static_data <- function(type = c("cpi", "hces")) {
  type <- match.arg(type)
  path <- system.file("extdata", paste0(type, "_snapshot.csv"), package = "inflascope")
  if (!nzchar(path)) stop("Bundled dataset missing from installed package")
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (type == "cpi") x$date <- as.Date(x$date)
  x
}

#' Validate imported CPI index rows before accepting an update
#' @param data Data frame matching the snapshot's columns.
#' @param expected_base Expected index base string; defaults to `2024=100`.
#' @return TRUE; otherwise raises a descriptive error.
#' @export
validate_cpi_data <- function(data, expected_base = "2024=100") {
  cols <- c("date", "sector", "category", "index", "inflation_yoy", "base_year", "release_date", "source_url")
  if (!is.data.frame(data) || !all(cols %in% names(data))) stop("Missing mandatory CPI columns")
  if (!nrow(data)) stop("CPI dataset must not be empty")
  d <- as.Date(data$date)
  if (anyNA(d) || any(format(d, "%d") != "01")) stop("CPI dates must be valid first-of-month dates")
  if (anyNA(data$sector) || !all(data$sector %in% c("Rural", "Urban", "Combined"))) stop("Unknown CPI sector")
  if (anyNA(data$category) || any(!nzchar(data$category))) stop("Missing CPI category")
  if (anyNA(data$base_year) || !all(data$base_year == expected_base)) stop("Incompatible CPI base year")
  if (!is.numeric(data$index) || anyNA(data$index) || any(!is.finite(data$index)) || any(data$index <= 0)) stop("CPI index must be positive and finite")
  if (!is.numeric(data$inflation_yoy) || any(!is.finite(data$inflation_yoy[!is.na(data$inflation_yoy)]))) stop("Invalid inflation rates")
  if (anyNA(as.Date(data$release_date))) stop("Missing or invalid release dates")
  if (anyNA(data$source_url) || any(!grepl("^https://", data$source_url))) stop("Missing HTTPS source URLs")
  key <- paste(d, data$sector, data$category, sep = "|")
  if (anyDuplicated(key)) stop("Duplicate date-sector-category observations")
  # Missing months are permissible in a partial verified snapshot; forecasting checks continuity separately.
  TRUE
}

.cache_path <- function() {
  path <- tools::R_user_dir("inflascope", which = "cache")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  file.path(path, "cpi_verified.rds")
}

#' Load the best available compatible CPI data
#' @param source One of `auto`, `static`, or `cache`.
#' @return CPI data frame, with source attribute.
#' @export
get_cpi_data <- function(source = c("auto", "static", "cache")) {
  source <- match.arg(source)
  snap <- get_static_data("cpi")
  if (source == "static") { attr(snap,"data_source") <- "Bundled official snapshot"; return(snap) }
  file <- .cache_path()
  if (file.exists(file)) {
    cached <- tryCatch(readRDS(file), error = function(e) NULL)
    valid <- tryCatch(validate_cpi_data(cached), error = function(e) FALSE)
    if (isTRUE(valid) && max(as.Date(cached$date)) >= max(snap$date)) {
      attr(cached, "data_source") <- "Validated local cache"; return(cached)
    }
  }
  if (source == "cache") stop("No compatible validated cache available")
  attr(snap, "data_source") <- "Bundled official snapshot"
  snap
}

#' Retrieve latest official general CPI observations by sector
#' @param data Validated CPI dataset.
#' @return Data frame of latest records, one per sector.
#' @export
get_latest_cpi <- function(data = get_cpi_data()) {
  validate_cpi_data(data)
  x <- data[data$category == "General", , drop = FALSE]
  if (!nrow(x)) stop("No general CPI rows")
  x <- x[order(x$date, decreasing = TRUE),,drop=FALSE]
  x[!duplicated(x$sector),,drop=FALSE]
}

#' Inspect active data provenance and currency
#' @param data CPI data frame.
#' @return Named list with snapshot or cache provenance.
#' @export
get_data_metadata <- function(data = get_cpi_data()) {
  list(source = if (is.null(attr(data,"data_source"))) "User-supplied" else attr(data,"data_source"),
       latest_period = max(as.Date(data$date)), base_year = unique(data$base_year),
       latest_release = max(as.Date(data$release_date)),
       retrieval_time = attr(data,"retrieval_time"),
       dataset_version = unique(data$dataset_version))
}

#' Import an official CPI file with transactional validation
#' @param path Local CSV path with the canonical CPI schema.
#' @return A validated CPI data frame written into the user's cache.
#' @export
import_cpi_csv <- function(path) {
  if (!file.exists(path)) stop("CSV file does not exist")
  incoming <- utils::read.csv(path, stringsAsFactors = FALSE)
  incoming$date <- as.Date(incoming$date)
  incoming$index <- as.numeric(incoming$index)
  incoming$inflation_yoy <- as.numeric(incoming$inflation_yoy)
  validate_cpi_data(incoming)
  if (max(incoming$date) < max(get_static_data("cpi")$date)) stop("Imported snapshot is older than the bundle")
  attr(incoming, "retrieval_time") <- Sys.time()
  target <- .cache_path(); temp <- tempfile(tmpdir = dirname(target))
  on.exit(unlink(temp), add = TRUE)
  saveRDS(incoming, temp)
  if (!file.copy(temp, target, overwrite = TRUE)) stop("Unable to commit validated cache")
  incoming
}

#' Determine whether a manual official-data update is required
#' @return List with reference period and guidance.
#' @export
check_data_updates <- function() {
  d <- get_cpi_data()
  list(latest_period = max(d$date), automatic_download = FALSE,
       message = "MoSPI downloads have not been verified as a stable machine-readable endpoint. Import a canonical official CSV using import_cpi_csv().")
}

#' Refresh the validated cache from a supplied official CSV
#' @param path Required path to locally downloaded official-format canonical CSV.
#' @return Imported data frame. Existing cache is preserved on error.
#' @export
refresh_cpi_data <- function(path = NULL) {
  if (is.null(path)) stop("No verified automated endpoint configured; provide path to an official canonical CSV")
  import_cpi_csv(path)
}
