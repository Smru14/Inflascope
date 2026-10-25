# Execute: Rscript scripts/refresh_data.R /path/to/verified-official-canonical.csv
if (length(commandArgs(trailingOnly=TRUE))!=1) stop("Supply canonical CPI CSV file path")
library(inflascope)
refresh_cpi_data(commandArgs(trailingOnly=TRUE)[1]); print(get_data_metadata(get_cpi_data()))
