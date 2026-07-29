# ==============================================================================
# pipeline/R/01_build_corpus.R
#
# STAGE 1 — Corpus build for ONE year. Designed to run as an SLURM array job,
# one task per year (2002-2025). Local runs work too — pass year as arg.
#
# Pipeline (per call):
#   1. Drop speakerGroup == "Operator" rows (lifted from ra_project/03_corpus.R:228)
#   2. remove_boilerplate() on each surviving speaker turn
#      (lifted from ra_project/03_corpus.R:197-221)
#   3. Concatenate cleaned turns -> one document per call
#   4. cld2::detect_language() -> keep only English (Sautner JF p. 1456)
#   5. Sentence-split for Stage 2 KLR (Sautner OA p. 2 works at sentence level)
#
# Outputs (under out/corpus/):
#   calls_YYYY.rds       one row per English-language call
#                        cols: Id, ticker, year, quarter, date, text, n_chars, n_sentences
#   sentences_YYYY.rds   one row per sentence (Stage-2 KLR input)
#                        cols: Id, sentence_id, text
#   audit_YYYY.csv       one row per year — filter funnel + summary stats
#
# Usage:
#   Rscript pipeline/R/01_build_corpus.R 2002
#   sbatch  pipeline/cluster/slurm_corpus.sh           # array over all years
#
# Memory pattern: load -> filter -> write -> rm() + gc(). Designed to fit in
# a 32 GB SLURM task; largest year (2024, ~825 MB) peaks at ~12 GB.
# ==============================================================================

options(stringsAsFactors = FALSE)

# ---- Parse year arg ----------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args) >= 1) as.integer(args[1]) else {
  # SLURM array fallback
  sa <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  if (nzchar(sa)) 2001L + as.integer(sa) else
    stop("Pass year as arg or set SLURM_ARRAY_TASK_ID (1=2002, ..., 24=2025).")
}
stopifnot(year >= 2002L, year <= 2025L)

# ---- Setup -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(stringi)
  library(cld2)
  library(quanteda)
})

# Locate project root and source helpers
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
source(file.path(ROOT, "pipeline", "R", "lib", "utils.R"))
source(file.path(ROOT, "pipeline", "R", "lib", "boilerplate.R"))

CORPUS_OUT <- file.path(ROOT, "out", "corpus")
if (!dir.exists(CORPUS_OUT)) dir.create(CORPUS_OUT, recursive = TRUE)

PARSED <- parsed_file(year)
if (!file.exists(PARSED)) stop("Missing input: ", PARSED)

log_step("=== STAGE 1 — Build corpus for year %d ===", year)
log_step("Input: %s (%.1f MB)", PARSED,
         file.info(PARSED)$size / 1024^2)

# ============================================================================
# 1. Load
# ============================================================================
log_step("[1/7] Loading TParsed_%d.RData ...", year)
load(PARSED)
stopifnot(exists("meta.parsed"))
dt <- as.data.table(meta.parsed)
rm(meta.parsed); gc(verbose = FALSE)

audit <- list(year = year, n_rows_raw = nrow(dt),
              n_calls_raw = uniqueN(dt$Id))
log_step("    rows=%s  calls=%s  tickers=%s",
         format(nrow(dt), big.mark = ","),
         format(uniqueN(dt$Id), big.mark = ","),
         format(uniqueN(dt$companyTicker), big.mark = ","))

# Keep only the columns we need (drop the rest — saves a lot of memory)
keep_cols <- c("Id", "companyTicker", "eventYear", "eventQuarter",
               "expirationDate", "sec", "secIdx", "speakerGroup", "speech")
dt <- dt[, intersect(keep_cols, names(dt)), with = FALSE]

# ============================================================================
# 2. Drop Operator turns (RA pattern, ra_project/R/03_corpus.R:228)
# ============================================================================
log_step("[2/7] Dropping speakerGroup == 'Operator' ...")
n_before <- nrow(dt)
dt <- dt[is.na(speakerGroup) | speakerGroup != "Operator"]
audit$n_rows_after_operator_drop <- nrow(dt)
log_step("    rows: %s -> %s (%d operator turns dropped)",
         format(n_before, big.mark = ","),
         format(nrow(dt), big.mark = ","),
         n_before - nrow(dt))

# Drop NA / empty speech
dt <- dt[!is.na(speech) & nchar(as.character(speech)) > 0]
dt <- dt[!is.na(Id)]
audit$n_rows_after_na_drop <- nrow(dt)

# ============================================================================
# 3. Cleaning levers (env-toggled) — per-turn safe-harbour boilerplate + markers
#    The v2 minimal-cleaning HEADLINE build uses SW_BP=off, SW_MARKER=on,
#    SW_SENTSH=none: one tokeniser and nothing aggressive, faithful to Sautner
#    (2023) and Hassan (2025, "don't over-clean"). Boilerplate removal was an
#    addition to Sautner and the source of the stale-DFM alignment artifact, so
#    v2 drops it. To reproduce the frozen v1.1 configuration instead, set
#    SW_BP=iter SW_SENTSH=on (see docs/REPRODUCE.md).
# ============================================================================
SW_BP     <- tolower(Sys.getenv("SW_BP",     "off"))    # off (v2) | once | iter (v1.1)
SW_MARKER <- tolower(Sys.getenv("SW_MARKER", "on"))     # on (default) | off
SW_SENTSH <- tolower(Sys.getenv("SW_SENTSH", "none"))   # none (v2) | on (v1.1)
log_step("[3/7] Cleaning levers: SW_BP=%s SW_MARKER=%s SW_SENTSH=%s", SW_BP, SW_MARKER, SW_SENTSH)
chars_before <- sum(nchar(dt$speech))
if (SW_BP == "iter") {
  dt[, speech := vapply(speech, remove_boilerplate, character(1))]        # iterative (<=20x) v1.1
} else if (SW_BP == "once") {
  dt[, speech := vapply(speech, remove_boilerplate_once, character(1))]   # single pass (RA-original)
}                                                                         # off (v2 default): no removal
# Strip transcript stage-direction / interpreter markers ("[Foreign Language]",
# "[Interpreted]", "[Inaudible]", ...) — pure metadata that otherwise becomes
# target-discriminative bigrams. Kept ON in v2. See boilerplate.R strip_transcript_markers.
if (SW_MARKER != "off") {
  dt[, speech := vapply(speech, strip_transcript_markers, character(1))]
}
chars_after <- sum(nchar(dt$speech))
audit$chars_stripped <- chars_before - chars_after
audit$chars_stripped_pct <- round(100 * (chars_before - chars_after) / chars_before, 2)
log_step("    chars: %s -> %s (%.2f%% removed)",
         format(chars_before, big.mark = ","),
         format(chars_after, big.mark = ","),
         audit$chars_stripped_pct)

# ============================================================================
# 4. Reconstruct one document per call (preserving order via secIdx if present)
# ============================================================================
log_step("[4/7] Reconstructing one document per call ...")
if ("secIdx" %in% names(dt)) setorder(dt, Id, secIdx)
calls <- dt[, .(
  ticker     = first(companyTicker),
  year       = if ("eventYear" %in% names(dt)) first(eventYear) else year,
  quarter    = if ("eventQuarter" %in% names(dt)) first(eventQuarter) else NA_integer_,
  date       = if ("expirationDate" %in% names(dt)) first(expirationDate) else NA,
  text       = paste(speech, collapse = " ")
), by = Id]
rm(dt); gc(verbose = FALSE)

# Coerce date to Date
if (!inherits(calls$date, "Date")) {
  raw <- calls$date
  if (is.numeric(raw) || is.integer(raw)) {
    calls[, date := as.Date(as.POSIXct(raw, origin = "1970-01-01", tz = "UTC"))]
  } else {
    calls[, date := suppressWarnings(as.Date(as.character(raw)))]
  }
}
calls[, year := as.integer(year)]
calls[, quarter := as.integer(quarter)]

audit$n_calls_reconstructed <- nrow(calls)
log_step("    calls: %s", format(nrow(calls), big.mark = ","))

# ============================================================================
# 5. Language filter (cld2)
# ============================================================================
log_step("[5/7] Detecting language (cld2) ...")
calls[, lang := detect_language(substr(text, 1, 5000))]
lang_tab <- calls[, .N, by = lang][order(-N)]
print(lang_tab)
audit$n_calls_english <- sum(calls$lang == "en", na.rm = TRUE)
audit$n_calls_dropped_nonenglish <- nrow(calls) - audit$n_calls_english
calls <- calls[!is.na(lang) & lang == "en"]
calls[, lang := NULL]
log_step("    kept: %s English calls (%d non-English dropped)",
         format(nrow(calls), big.mark = ","),
         audit$n_calls_dropped_nonenglish)

# ============================================================================
# 6. Sentence-split (for Stage 2 KLR)
# ============================================================================
log_step("[6/7] Sentence-splitting for Stage 2 ...")
corp <- corpus(calls$text, docnames = as.character(calls$Id))
corp_sent <- corpus_reshape(corp, to = "sentences")
sent_dt <- data.table(
  Id          = stri_replace_last_regex(docnames(corp_sent), "\\.\\d+$", ""),
  sentence_id = as.integer(stri_extract_last_regex(docnames(corp_sent), "\\d+$")),
  text        = as.character(corp_sent)
)
rm(corp, corp_sent)
audit$n_sentences <- nrow(sent_dt)
audit$mean_sentences_per_call <- round(nrow(sent_dt) / nrow(calls), 2)
log_step("    sentences: %s  (mean %.1f per call)",
         format(nrow(sent_dt), big.mark = ","),
         audit$mean_sentences_per_call)

# Drop sentences with < 5 chars (after sentence-split, lots of "Yes." "OK." noise)
n_sent_before <- nrow(sent_dt)
sent_dt <- sent_dt[nchar(text) >= 5]
audit$n_sentences_after_minlen <- nrow(sent_dt)
log_step("    after min-length=5 filter: %s (%d short dropped)",
         format(nrow(sent_dt), big.mark = ","),
         n_sent_before - nrow(sent_dt))

# Sentence-level safe-harbour filter (v1.1 only) — drops residual legal-disclaimer
# sentences that the per-turn remover couldn't peel off. Skipped under the v2
# minimal-cleaning default (SW_SENTSH=none); set SW_SENTSH=on for the v1.1 config.
if (SW_SENTSH != "none") {
  n_sent_before <- nrow(sent_dt)
  sent_dt <- sent_dt[!is_safeharbor_sentence(text)]
  audit$n_sentences_after_safeharbor_filter <- nrow(sent_dt)
  audit$n_sentences_dropped_safeharbor <- n_sent_before - nrow(sent_dt)
  log_step("    after safe-harbour sentence filter: %s (%d sentences dropped)",
           format(nrow(sent_dt), big.mark = ","),
           audit$n_sentences_dropped_safeharbor)
} else {
  audit$n_sentences_after_safeharbor_filter <- nrow(sent_dt)
  audit$n_sentences_dropped_safeharbor <- 0L
  log_step("    safe-harbour sentence filter: SKIPPED (SW_SENTSH=none, v2 minimal)")
}

# Per-call summary additions
n_sent_per_call <- sent_dt[, .N, by = Id]
calls <- merge(calls, n_sent_per_call, by = "Id", all.x = TRUE)
setnames(calls, "N", "n_sentences")
calls[, n_chars := nchar(text)]

# ============================================================================
# 7. Save
# ============================================================================
log_step("[7/7] Writing outputs ...")
calls_out <- file.path(CORPUS_OUT, sprintf("calls_%d.rds", year))
sent_out  <- file.path(CORPUS_OUT, sprintf("sentences_%d.rds", year))
audit_out <- file.path(CORPUS_OUT, sprintf("audit_%d.csv", year))

saveRDS(calls,   calls_out,   compress = "gzip")
saveRDS(sent_dt, sent_out,    compress = "gzip")
fwrite(as.data.table(audit), audit_out)

log_step("    %s (%.1f MB)", calls_out, file.info(calls_out)$size / 1024^2)
log_step("    %s (%.1f MB)", sent_out,  file.info(sent_out)$size  / 1024^2)
log_step("    %s", audit_out)

log_step("=== DONE — year %d ===", year)
log_step("Funnel: %s rows -> %s calls -> %s English -> %s sentences",
         format(audit$n_rows_raw, big.mark = ","),
         format(audit$n_calls_reconstructed, big.mark = ","),
         format(audit$n_calls_english, big.mark = ","),
         format(audit$n_sentences_after_minlen, big.mark = ","))
