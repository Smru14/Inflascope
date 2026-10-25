# Optional: convert verified snapshot CSVs to R package .rda datasets using R.
# Execute inside the package root after audit of sources.
cpi_reference <- read.csv("inst/extdata/cpi_snapshot.csv", stringsAsFactors=FALSE)
hces_summary <- read.csv("inst/extdata/hces_snapshot.csv", stringsAsFactors=FALSE)
dir.create("data",showWarnings=FALSE)
save(cpi_reference,file="data/cpi_reference.rda",compress="xz")
save(hces_summary,file="data/hces_summary.rda",compress="xz")
