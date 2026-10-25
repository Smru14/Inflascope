synthetic_cpi <- function(n = 36) {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = n)
  data.frame(date = rep(dates, 2), sector = rep(c("Rural", "Urban"), each = n), category = "General",
             index = c(100 + 0.4 * seq_len(n) + sin(seq_len(n)), 100 + 0.3 * seq_len(n) + cos(seq_len(n))),
             inflation_yoy = NA_real_, base_year = "2024=100", release_date = "2026-10-01",
             source_url = "https://example.org/synthetic", stringsAsFactors = FALSE)
}
