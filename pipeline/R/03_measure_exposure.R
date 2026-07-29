# ==============================================================================
# pipeline/R/03_measure_exposure.R
#
# STAGE 3 (per-year array) — Sautner exposure measures (Eqs 1-4) for one year.
# Reference: Sautner, van Lent, Vilkov, Zhang (2023) JF pp. 1461-1462.
#
# All measures scaled by 1/B_it (total bigrams in the transcript). Eq.2/Eq.3
# condition at the SENTENCE level (not a +/-word window). For each call:
#   Eq.1 GeoExposure      = (1/B) Σ_b 1[b∈C]
#   Eq.3 GeoRisk          = (1/B) Σ_b 1[b∈C]·1[risk word in same sentence]
#   Eq.2 GeoSentiment     = pos - neg; pos/neg = (1/B) Σ_b 1[b∈C]·1[L&M pos/neg word in sentence]
#   Eq.4 GeoExposureTFIDF = (1/B) Σ_b 1[b∈C]·count_b·log(N_T/f_b)   <-- GLOBAL f_b, finished in 03b
# Computed for BOTH the PRIMARY (unpruned, headline) and PRUNED (robustness) dicts.
#
# Inputs (per year, on cluster): out/dict/dfm_YYYY.rds (sentence bigram DFM),
# out/dict/meta_YYYY.rds (year,Id,sentence_id aligned to DFM rows),
# out/corpus/sentences_YYYY.rds (Id,sentence_id,text), out/corpus/calls_YYYY.rds.
#
# Run:  Rscript pipeline/R/03_measure_exposure.R 2015      (or via SLURM_ARRAY_TASK_ID)
# ==============================================================================
options(stringsAsFactors = FALSE)

N_CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", parallel::detectCores()))
Sys.setenv(OMP_NUM_THREADS = as.character(N_CORES),
           OPENBLAS_NUM_THREADS = as.character(N_CORES),
           RCPP_PARALLEL_NUM_THREADS = as.character(N_CORES))

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(stringi)
  library(quanteda)
  library(jsonlite)
})
quanteda_options(threads = N_CORES)

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
source(file.path(ROOT, "pipeline", "R", "lib", "sklearn_tokenizer.R"))

# ---- Resolve year ------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
years <- 2002:2025
ai    <- Sys.getenv("SLURM_ARRAY_TASK_ID")
if (length(args) >= 1) {
  year <- as.integer(args[1])
} else if (nzchar(ai)) {
  year <- years[as.integer(ai)]
} else {
  stop("Pass a year arg or set SLURM_ARRAY_TASK_ID (1-24).")
}

DICT_OUT <- v2_dir(file.path(ROOT, "out", "dict"))       # GEOV2=1 -> out/dict_v2
CORP     <- file.path(ROOT, "out", "corpus")
EXP_OUT  <- out_dir(exp_dirname())   # v1/v2 unchanged; GEO_EXPO=rwclean -> exposure_rwclean

log_step("STAGE 3 — exposure measurement | year %d | cores=%d%s", year, N_CORES,
         if (is_v2()) " | V2 (exact tokenizer + reconciled risk words)" else "")

# ---- Normalise a dictionary's bigrams to the tokeniser's space ---------------
norm_bigrams <- function(bg) tolower(stri_trans_general(bg, "Latin-ASCII"))

dict_primary <- norm_bigrams(load_dictionary("primary"))
dict_pruned  <- norm_bigrams(load_dictionary("pruned"))
risk_words   <- norm_bigrams(load_risk_words())            # unigrams
lm           <- load_lm_words()                            # NULL -> skip Eq.2
lm_pos <- if (!is.null(lm)) norm_bigrams(lm$positive) else character(0)
lm_neg <- if (!is.null(lm)) norm_bigrams(lm$negative) else character(0)
do_sent <- !is.null(lm)
if (!do_sent) log_step("  WARNING: no LM dictionary -> Eq.2 (sentiment) skipped this run")
log_step("  dict primary=%d, pruned=%d, risk=%d, LM pos/neg=%d/%d",
         length(dict_primary), length(dict_pruned), length(risk_words),
         length(lm_pos), length(lm_neg))

# ---- Load inputs -------------------------------------------------------------
dfm  <- readRDS(file.path(DICT_OUT, sprintf("dfm_%d.rds",  year)))
meta <- as.data.table(readRDS(file.path(DICT_OUT, sprintf("meta_%d.rds", year))))
sent <- as.data.table(readRDS(file.path(CORP,     sprintf("sentences_%d.rds", year))))
calls<- as.data.table(readRDS(file.path(CORP,     sprintf("calls_%d.rds", year))))
stopifnot(nrow(meta) == ndoc(dfm))
log_step("  loaded: dfm %s x %s, meta %s, sentences %s, calls %s",
         format(ndoc(dfm), big.mark=","), format(nfeat(dfm), big.mark=","),
         format(nrow(meta), big.mark=","), format(nrow(sent), big.mark=","),
         format(nrow(calls), big.mark=","))

# ---- Dict-bigram sub-DFM (FIXED column set via dfm_match) --------------------
present_primary <- intersect(dict_primary, featnames(dfm))
dfm_dict_p  <- dfm_match(dfm, features = present_primary)     # sentences x present_primary
cols_pruned <- intersect(dict_pruned, colnames(dfm_dict_p))
log_step("  dict bigrams present: primary %d/%d, pruned %d/%d",
         length(present_primary), length(dict_primary),
         length(cols_pruned), length(dict_pruned))

# ---- Per-sentence UNIGRAM flags (risk / L&M tone) ----------------------------
# Align sentence text to DFM rows by KEYED join (never positional).
setkey(sent, Id, sentence_id)
meta_keyed <- sent[meta[, .(Id, sentence_id)], on = .(Id, sentence_id)]  # 1 row per dfm row, in meta order
stopifnot(nrow(meta_keyed) == ndoc(dfm))
flag_vocab <- unique(c(risk_words, lm_pos, lm_neg))
toks_uni   <- tokenize_unigrams(ifelse(is.na(meta_keyed$text), "", meta_keyed$text))
dfm_flag   <- dfm(tokens_keep(toks_uni, pattern = flag_vocab, valuetype = "fixed"))
has_in <- function(words) {
  cols <- intersect(words, colnames(dfm_flag))
  if (length(cols) == 0L) return(rep(FALSE, ndoc(dfm_flag)))
  as.numeric(rowSums(dfm_flag[, cols, drop = FALSE]) > 0)
}
has_risk <- has_in(risk_words)
has_pos  <- if (do_sent) has_in(lm_pos) else rep(0, ndoc(dfm))
has_neg  <- if (do_sent) has_in(lm_neg) else rep(0, ndoc(dfm))

# ---- Per-sentence quantities -------------------------------------------------
B_sent      <- as.numeric(ntoken(dfm))            # total bigrams per sentence (denominator base)
hits_p_sent <- as.numeric(rowSums(dfm_dict_p))    # dict hits per sentence (primary)
hits_pr_sent<- if (length(cols_pruned)) as.numeric(rowSums(dfm_dict_p[, cols_pruned, drop=FALSE])) else rep(0, nrow(meta))

# ---- Aggregate sentences -> calls (Eq.1/2/3) ---------------------------------
A <- data.table(Id = meta$Id, B = B_sent,
                hp = hits_p_sent, hpr = hits_pr_sent,
                risk = has_risk, pos = has_pos, neg = has_neg)
agg <- A[, .(
  B   = sum(B),
  h_p = sum(hp),  h_pr = sum(hpr),
  hr_p= sum(hp*risk), hr_pr = sum(hpr*risk),
  hpos_p = sum(hp*pos), hpos_pr = sum(hpr*pos),
  hneg_p = sum(hp*neg), hneg_pr = sum(hpr*neg)
), by = Id]
safe_div <- function(num, den) ifelse(den > 0, num/den, NA_real_)
agg[, `:=`(
  GeoExposure        = safe_div(h_p,  B),  GeoExposure_pr   = safe_div(h_pr,  B),
  GeoRisk            = safe_div(hr_p, B),  GeoRisk_pr       = safe_div(hr_pr, B),
  GeoSentimentPos    = safe_div(hpos_p,B), GeoSentimentPos_pr = safe_div(hpos_pr,B),
  GeoSentimentNeg    = safe_div(hneg_p,B), GeoSentimentNeg_pr = safe_div(hneg_pr,B)
)]
agg[, `:=`(GeoSentiment    = GeoSentimentPos - GeoSentimentNeg,
           GeoSentiment_pr = GeoSentimentPos_pr - GeoSentimentNeg_pr)]
# Eq.4 columns are placeholders here; 03b fills them with the GLOBAL TF-IDF weights.
agg[, `:=`(GeoExposureTFIDF = NA_real_, GeoExposureTFIDF_pr = NA_real_)]

# Attach firm-quarter key
panel <- calls[, .(Id, ticker, year, quarter, date)][agg, on = "Id"]
panel[, n_dict_hits := h_p]

# ---- Eq.4 raw materials (per-call dict-bigram counts + per-year docfreq) ------
C_call_p <- dfm_group(dfm_dict_p, groups = meta$Id)          # calls x present_primary (counts)
B_call   <- A[, .(B = sum(B)), by = Id]                       # total bigrams per call
B_call   <- setNames(B_call$B, B_call$Id)[docnames(C_call_p)] # align to C_call rows
Cp <- as(C_call_p, "CsparseMatrix")
docfreq_p <- Matrix::colSums(Cp > 0)                          # # calls (this year) containing each bigram
N_T_year  <- nrow(Cp)
tfidf_parts <- list(
  C_call_primary = Cp,
  cols_pruned    = cols_pruned,
  B_call         = B_call,
  docfreq_primary= docfreq_p,
  N_T_year       = N_T_year,
  call_ids       = docnames(C_call_p),
  year           = year
)

# ---- Write outputs -----------------------------------------------------------
# GEO_EXP_TAG (e.g. "rt") namespaces vintage/real-time scoring so it does not
# clobber the main exposure panel.
.etag <- Sys.getenv("GEO_EXP_TAG"); .et <- if (nzchar(.etag)) paste0("_", .etag) else ""
saveRDS(panel,       file.path(EXP_OUT, sprintf("exposure%s_%d.rds", .et, year)))
saveRDS(tfidf_parts, file.path(EXP_OUT, sprintf("tfidf_parts%s_%d.rds", .et, year)))

audit <- list(
  year = year, n_calls = nrow(panel), n_sentences = nrow(meta),
  n_present_primary = length(present_primary), n_present_pruned = length(cols_pruned),
  pct_calls_with_hit = round(100*mean(panel$h_p > 0, na.rm=TRUE), 2),
  pct_calls_B0 = round(100*mean(panel$B == 0, na.rm=TRUE), 3),
  mean_GeoExposure = mean(panel$GeoExposure, na.rm=TRUE),
  median_GeoExposure = median(panel$GeoExposure, na.rm=TRUE),
  sentiment_computed = do_sent,
  alignment_ok = TRUE,
  ended_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
write_json(audit, file.path(EXP_OUT, sprintf("audit_%d.json", year)),
           pretty = TRUE, auto_unbox = TRUE)

log_step("  DONE year %d: %s calls | mean GeoExposure %.3e | %% with hit %.1f",
         year, format(nrow(panel), big.mark=","), audit$mean_GeoExposure, audit$pct_calls_with_hit)
