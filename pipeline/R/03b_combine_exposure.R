# ==============================================================================
# pipeline/R/03b_combine_exposure.R
#
# STAGE 3 combine — finalize Eq.4 (global TF-IDF), bind the call panel, and
# aggregate to a firm-quarter panel. Runs once, after the per-year array.
#
# Eq.4 needs GLOBAL document frequency: f_b = # transcripts (calls) across ALL
# years containing bigram b, N_T = total calls. The per-year tasks emitted the
# raw materials (sparse per-call dict counts + per-year docfreq); here we sum
# them, build w_b = log(N_T/f_b), and apply it per call. w_b is a property of the
# bigram, so the SAME weights serve both the primary and pruned dictionaries.
#
# Run:  Rscript pipeline/R/03b_combine_exposure.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(data.table); library(Matrix); library(jsonlite); library(arrow); library(stringi)
})
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d) }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
source(file.path(ROOT, "pipeline", "R", "lib", "utils.R"))
EXP <- exp_dir()   # GEOV2/GEO_EXPO=v2 -> out/exposure_v2; GEO_EXPO=rwclean -> exposure_rwclean
log_step("STAGE 3 combine — global TF-IDF + firm-quarter panel%s",
         if (is_v2()) " [V2]" else "")

# ---- 1. Bind per-year call panels -------------------------------------------
ef <- sort(list.files(EXP, pattern = "^exposure_\\d{4}\\.rds$", full.names = TRUE))
if (!length(ef)) stop("No exposure_YYYY.rds in ", EXP, " — run the array first.")
panel <- rbindlist(lapply(ef, readRDS), use.names = TRUE, fill = TRUE)
log_step("  bound %d per-year files -> %s calls", length(ef), format(nrow(panel), big.mark=","))

# ---- 2. Global TF-IDF weights (Eq.4) ----------------------------------------
tf <- sort(list.files(EXP, pattern = "^tfidf_parts_\\d{4}\\.rds$", full.names = TRUE))
parts <- lapply(tf, readRDS)
N_T <- sum(vapply(parts, function(p) p$N_T_year, numeric(1)))           # total transcripts
# f_b = sum of per-year call-docfreq, aligned by bigram name
fb <- Reduce(function(a, b) {
  u <- union(names(a), names(b)); a <- a[u]; b <- b[u]
  a[is.na(a)] <- 0; b[is.na(b)] <- 0; setNames(a + b, u)
}, lapply(parts, function(p) p$docfreq_primary))
w_b <- log(N_T / fb)                                                    # IDF weight per bigram
log_step("  N_T=%s transcripts | %s unique dict bigrams | w_b range [%.2f, %.2f]",
         format(N_T, big.mark=","), format(length(w_b), big.mark=","), min(w_b), max(w_b))

# Robustness dictionaries = column SUBSETS of the primary 9,650, so their Eq.1 +
# Eq.4 come straight from the saved per-call counts (no array re-run).
nrm <- function(b) tolower(stri_trans_general(b, "Latin-ASCII"))
cfg <- file.path(ROOT, "pipeline", "config")
exen_bg  <- nrm(fread(file.path(cfg, "dictionary_geoeconomic_exenergy.csv"))$bigram)
small_bg <- nrm(fread(file.path(cfg, "dictionary_geoeconomic_small.csv"))$bigram)
sub_exp <- function(C, B, cols) {        # Eq.1 + Eq.4 for a bigram-name subset
  cc <- intersect(cols, colnames(C))
  e <- if (length(cc)) as.numeric(Matrix::rowSums(C[, cc, drop=FALSE])) else rep(0, nrow(C))
  t <- if (length(cc)) as.numeric(C[, cc, drop=FALSE] %*% w_b[cc])       else rep(0, nrow(C))
  list(exp = ifelse(B > 0, e/B, NA_real_), tfidf = ifelse(B > 0, t/B, NA_real_))
}

# Per-call Eq.4 = (C_call %*% w_b)/B_call ; pruned/ex-energy/small restrict columns.
tfidf_rows <- rbindlist(lapply(parts, function(p) {
  C <- p$C_call_primary; B <- p$B_call[rownames(C)]
  num_p  <- as.numeric(C %*% w_b[colnames(C)])
  cpr    <- intersect(p$cols_pruned, colnames(C))
  num_pr <- if (length(cpr)) as.numeric(C[, cpr, drop=FALSE] %*% w_b[cpr]) else rep(0, nrow(C))
  ex <- sub_exp(C, B, exen_bg); sm <- sub_exp(C, B, small_bg)
  data.table(Id = p$call_ids,
             GeoExposureTFIDF    = ifelse(B > 0, num_p  / B, NA_real_),
             GeoExposureTFIDF_pr = ifelse(B > 0, num_pr / B, NA_real_),
             GeoExposure_exen = ex$exp,  GeoExposureTFIDF_exen = ex$tfidf,
             GeoExposure_small = sm$exp, GeoExposureTFIDF_small = sm$tfidf)
}), use.names = TRUE)

# hand-check one call (Eq.4 vectorised vs by-hand)
p1 <- parts[[length(parts)]]; C1 <- p1$C_call_primary
i  <- which.max(Matrix::rowSums(C1 > 0))
man <- sum(C1[i, ] * w_b[colnames(C1)]) / p1$B_call[rownames(C1)[i]]
vec <- tfidf_rows[Id == p1$call_ids[i], GeoExposureTFIDF][1]
stopifnot(abs(man - vec) < 1e-8)
log_step("  Eq.4 hand-check OK (call %s: %.3e)", p1$call_ids[i], vec)

panel[, c("GeoExposureTFIDF","GeoExposureTFIDF_pr") := NULL]
panel <- tfidf_rows[panel, on = "Id"]

# ---- 3. Firm-quarter panel ---------------------------------------------------
measure_cols <- c("GeoExposure","GeoRisk","GeoSentiment","GeoExposureTFIDF",
                  "GeoExposure_pr","GeoRisk_pr","GeoSentiment_pr","GeoExposureTFIDF_pr",
                  "GeoSentimentPos","GeoSentimentNeg",
                  "GeoExposure_exen","GeoExposureTFIDF_exen",      # ex-energy robustness
                  "GeoExposure_small","GeoExposureTFIDF_small")    # small ~300 curated robustness
fq <- panel[!is.na(ticker), c(
  lapply(.SD, mean, na.rm = TRUE), .(n_calls = .N)
), by = .(ticker, year, quarter), .SDcols = measure_cols]
log_step("  firm-quarter panel: %s rows (%s firms)", format(nrow(fq), big.mark=","),
         format(uniqueN(fq$ticker), big.mark=","))

# ---- 4. Write outputs --------------------------------------------------------
saveRDS(panel, file.path(EXP, "exposure_calls.rds"))
saveRDS(fq,    file.path(EXP, "exposure_firmquarter.rds"))
write_parquet(panel, file.path(EXP, "exposure_calls.parquet"))
write_parquet(fq,    file.path(EXP, "exposure_firmquarter.parquet"))

audit <- list(
  n_calls = nrow(panel), n_firmquarters = nrow(fq), n_firms = uniqueN(fq$ticker),
  N_T = N_T, n_dict_bigrams_seen = length(w_b),
  n_dead_bigrams = sum(fb == 0),
  measure_summary = lapply(measure_cols, function(c) {
    x <- panel[[c]]; list(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE),
                          p99 = as.numeric(quantile(x, .99, na.rm=TRUE))) }),
  ended_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
names(audit$measure_summary) <- measure_cols
write_json(audit, file.path(EXP, "exposure_audit.json"), pretty = TRUE, auto_unbox = TRUE)
log_step("=== STAGE 3 DONE === calls=%s firm-quarters=%s | mean GeoExposure %.3e",
         format(nrow(panel), big.mark=","), format(nrow(fq), big.mark=","),
         mean(panel$GeoExposure, na.rm=TRUE))
