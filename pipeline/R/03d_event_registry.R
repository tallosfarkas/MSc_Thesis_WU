# ==============================================================================
# pipeline/R/03d_event_registry.R   (Stage 3.5 — event registry, CLUSTER)
#
# Builds one row per call Id from the per-year corpus, recording the EVENT-TIME
# ticker (= companyTicker from the EARLIEST source-file year, closest to the call)
# and every ticker variant seen across versions. Run on the cluster where
# out/corpus/calls_*.rds live; the slim output is pulled local for 03e.
#
# Why the file year (filename), not `year`: cross-year re-includes share the same
# eventYear, so only the source file distinguishes the original from the rewrite.
#
#   Rscript pipeline/R/03d_event_registry.R   ->  out/exposure/event_registry.rds
# ==============================================================================
suppressPackageStartupMessages({ library(data.table) })
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d) }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
CORP <- file.path(ROOT, "out", "corpus")
EXP  <- file.path(ROOT, "out", "exposure"); if (!dir.exists(EXP)) dir.create(EXP, recursive = TRUE)

files <- sort(list.files(CORP, pattern = "^calls_\\d{4}\\.rds$", full.names = TRUE))
if (!length(files)) stop("No calls_YYYY.rds in ", CORP)

reg <- rbindlist(lapply(files, function(f) {
  yr <- as.integer(gsub("\\D", "", basename(f)))
  d  <- as.data.table(readRDS(f))
  d[, .(Id = as.character(Id), ticker = as.character(ticker),
        eventYear = year, eventQuarter = quarter, date, file_year = yr)]
}), use.names = TRUE)
cat(sprintf("[registry] %d calls-rows across %d files | unique Id=%s\n",
            nrow(reg), length(files), format(uniqueN(reg$Id), big.mark = ",")))

setorder(reg, Id, file_year)
registry <- reg[, .(
  event_time_ticker = ticker[1],                               # earliest file year
  all_tickers       = paste(unique(ticker[!is.na(ticker)]), collapse = ";"),
  n_versions        = uniqueN(file_year),
  first_file_year   = file_year[1],
  eventYear         = eventYear[1], eventQuarter = eventQuarter[1], date = date[1]
), by = Id]

stopifnot(nrow(registry) == uniqueN(reg$Id))                    # one row per Id
n_multi <- registry[n_versions > 1, .N]
cat(sprintf("[registry] one row per Id=%s | appearing in >1 file=%s (%.1f%%)\n",
            format(nrow(registry), big.mark = ","), format(n_multi, big.mark = ","),
            100 * n_multi / nrow(registry)))
saveRDS(registry, file.path(EXP, "event_registry.rds"))
cat("[registry] wrote out/exposure/event_registry.rds\n")
