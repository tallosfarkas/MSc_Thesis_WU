# ==============================================================================
# pipeline/R/02p_export_sentences.R
#
# Export one year of Stage-1 sentences (out/corpus/sentences_YYYY.rds) to parquet
# so the Python sklearn pipeline (pipeline/python/02b_klr_discovery.py) can read
# the raw sentence TEXT. CountVectorizer must tokenise text itself — it cannot
# consume the R-built DFM.
#
# Output columns: year, Id, sentence_id, text  (one row per sentence).
#
# Run as a per-year SLURM array task (slurm_export_parquet.sh), year index via
# SLURM_ARRAY_TASK_ID, OR locally:  Rscript pipeline/R/02p_export_sentences.R 2015
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root.")
}
ROOT <- .find_root()

# Resolve which year to export -------------------------------------------------
args   <- commandArgs(trailingOnly = TRUE)
years  <- 2002:2025
ai     <- Sys.getenv("SLURM_ARRAY_TASK_ID")
if (length(args) >= 1) {
  year <- as.integer(args[1])
} else if (nzchar(ai)) {
  year <- years[as.integer(ai)]
} else {
  stop("Pass a year arg or set SLURM_ARRAY_TASK_ID (1-24).")
}

in_rds  <- file.path(ROOT, "out", "corpus", sprintf("sentences_%d.rds", year))
out_dir <- file.path(ROOT, "out", "corpus_parquet")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_pq  <- file.path(out_dir, sprintf("sentences_%d.parquet", year))

if (!file.exists(in_rds)) stop("Missing input: ", in_rds)

cat(sprintf("[%s] exporting %d ...\n", format(Sys.time(), "%H:%M:%S"), year))
x <- readRDS(in_rds)
setDT(x)
# Keep only the columns Python needs; tag the year for traceability.
keep <- intersect(c("Id", "sentence_id", "text"), names(x))
x <- x[, ..keep]
x[, year := year]
setcolorder(x, c("year", "Id", "sentence_id", "text"))

write_parquet(x, out_pq, compression = "zstd")
cat(sprintf("[%s] wrote %s  (%s rows)\n",
            format(Sys.time(), "%H:%M:%S"), out_pq,
            format(nrow(x), big.mark = ",")))
