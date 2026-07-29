# ==============================================================================
# pipeline/R/00_inspect_parsed.R
#
# Purpose: Audit data/parsed/TParsed_YYYY.RData. Pure inspection — NO NLP, NO
# cleaning, NO writes to data/. Outputs go to out/.
#
# Stage 00 of the pipeline: the audit that runs before the corpus build.
#
# Per-year row in out/parsed_audit.csv:
#   year, file_size_mb, rows, unique_calls, unique_tickers,
#   date_min, date_max, mean_speech_chars, median_speech_chars,
#   n_na_ticker, n_na_speech, n_empty_speech, schema_signature
#
# For 3 spot-check years (2002, 2012, 2024), also writes
#   out/sample_text_YYYY.txt — first 2000 chars of one reconstructed call
#   (reproducible: seed = 42).
#
# Memory pattern: sequential year load + rm() + gc() (legacy pattern at
#   scripts/02_nlp/01_keyword_discovery.R:120). Avoids dense-matrix blowup.
#
# Run from project root:  Rscript pipeline/R/00_inspect_parsed.R
# ==============================================================================

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

PARSED_DIR <- "data/parsed"
OUT_DIR    <- "out"
SAMPLE_YEARS <- c(2002, 2012, 2024)
SEED <- 42

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
if (!dir.exists(PARSED_DIR)) stop("Parsed directory not found at: ", PARSED_DIR)

files <- list.files(PARSED_DIR, pattern = "^TParsed_\\d{4}\\.RData$",
                    full.names = TRUE)
if (length(files) == 0) stop("No TParsed_YYYY.RData files in ", PARSED_DIR)
files <- sort(files)

cat(sprintf("Found %d parsed files in %s\n", length(files), PARSED_DIR))
cat(strrep("=", 70), "\n", sep = "")

audit_rows <- vector("list", length(files))

for (i in seq_along(files)) {
  f <- files[i]
  year <- as.integer(str_extract(basename(f), "\\d{4}"))
  size_mb <- round(file.info(f)$size / 1024^2, 2)

  cat(sprintf("[%2d/%d] %s  (%.1f MB) ... ", i, length(files), basename(f), size_mb))

  # Load — populates `meta.parsed` (per scripts/02_nlp/05_score_tparsed_2025.R:59)
  load(f)
  if (!exists("meta.parsed")) {
    stop("Loaded ", basename(f), " but object `meta.parsed` is not present")
  }
  dt <- as.data.table(meta.parsed)
  rm(meta.parsed)

  schema_sig <- paste(sort(names(dt)), collapse = ",")
  n_rows <- nrow(dt)

  # Tolerate column-name variation gracefully — only count what we can find.
  has_id     <- "Id" %in% names(dt)
  has_ticker <- "companyTicker" %in% names(dt)
  has_speech <- "speech" %in% names(dt)
  has_date   <- "lastUpdate" %in% names(dt)

  unique_calls   <- if (has_id)     uniqueN(dt$Id)            else NA_integer_
  unique_tickers <- if (has_ticker) uniqueN(dt$companyTicker) else NA_integer_

  if (has_date) {
    raw <- dt$lastUpdate
    if (is.numeric(raw) || is.integer(raw)) {
      d <- as.Date(as.POSIXct(raw, origin = "1970-01-01", tz = "UTC"))
    } else {
      d <- suppressWarnings(as.Date(as.character(raw)))
    }
    date_min <- as.character(suppressWarnings(min(d, na.rm = TRUE)))
    date_max <- as.character(suppressWarnings(max(d, na.rm = TRUE)))
  } else {
    date_min <- date_max <- NA_character_
  }

  if (has_speech) {
    sp <- as.character(dt$speech)
    chars <- nchar(sp)
    mean_chars   <- round(mean(chars, na.rm = TRUE), 1)
    median_chars <- median(chars, na.rm = TRUE)
    n_na_speech    <- sum(is.na(sp))
    n_empty_speech <- sum(!is.na(sp) & nchar(sp) == 0)
  } else {
    mean_chars <- median_chars <- NA_real_
    n_na_speech <- n_empty_speech <- NA_integer_
  }

  n_na_ticker <- if (has_ticker) sum(is.na(dt$companyTicker) |
                                       nchar(as.character(dt$companyTicker)) == 0)
                 else NA_integer_

  # Spot-check sample (reproducible by call ID, not row, so it stays
  # stable even if rows get reordered).
  if (year %in% SAMPLE_YEARS && has_id && has_speech) {
    set.seed(SEED)
    ids <- unique(dt$Id[!is.na(dt$Id)])
    if (length(ids) > 0) {
      pick <- sample(ids, 1)
      reconstructed <- paste(dt$speech[dt$Id == pick], collapse = " ")
      sample_text <- substr(reconstructed, 1, 2000)
      out_path <- file.path(OUT_DIR, sprintf("sample_text_%d.txt", year))
      writeLines(c(
        sprintf("# Year: %d", year),
        sprintf("# Call Id: %s", as.character(pick)),
        sprintf("# Reconstructed length (chars): %d", nchar(reconstructed)),
        sprintf("# Below: first 2000 chars."),
        "",
        sample_text
      ), out_path)
      cat(sprintf("[sample -> %s] ", out_path))
    }
  }

  audit_rows[[i]] <- data.table(
    year                 = year,
    file_size_mb         = size_mb,
    rows                 = n_rows,
    unique_calls         = unique_calls,
    unique_tickers       = unique_tickers,
    date_min             = date_min,
    date_max             = date_max,
    mean_speech_chars    = mean_chars,
    median_speech_chars  = as.numeric(median_chars),
    n_na_ticker          = n_na_ticker,
    n_na_speech          = n_na_speech,
    n_empty_speech       = n_empty_speech,
    schema_signature     = schema_sig
  )

  cat("done\n")
  rm(dt); gc(verbose = FALSE)
}

audit <- rbindlist(audit_rows)
out_csv <- file.path(OUT_DIR, "parsed_audit.csv")
fwrite(audit, out_csv)

cat(strrep("=", 70), "\n", sep = "")
cat(sprintf("Audit written: %s (%d rows)\n", out_csv, nrow(audit)))
print(audit[, .(year, rows, unique_calls, unique_tickers,
                date_min, date_max, mean_speech_chars)])

# Quick sanity: schema should be identical across years
sigs <- unique(audit$schema_signature)
if (length(sigs) == 1L) {
  cat(sprintf("\nSchema: STABLE across all %d years -> %s\n",
              nrow(audit), sigs))
} else {
  cat(sprintf("\nSchema DRIFT across years (%d distinct signatures):\n",
              length(sigs)))
  for (s in sigs) {
    yrs <- audit$year[audit$schema_signature == s]
    cat(sprintf("  [%s]: %s\n",
                paste(yrs, collapse = ","), s))
  }
}
