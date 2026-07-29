# ==============================================================================
# pipeline/R/03e_map_identifiers.R   (Stage 3.5 — dedup + dual mapping, LOCAL)
#
# Runs locally (where ec_ccm_map_v8.rds + CLEAN_QUARTERLY_PRICING_v2.rds live).
# (1) dedup exposure_calls to one row per Id (cross-year re-includes are identical);
# (2) attach the event-time ticker from the registry; (3) GLOBAL map -> canonical
# RIC (clean_ric) joined to the LSEG price universe; (4) US map -> permno/gvkey/siccd
# via ec_ccm_map_v8 (Id == eventId, already one row/eventId, point-in-time). Emits
# two firm-quarter panels + a coverage audit. Keeps the locked dict + Stage-4
# validation untouched (panel-level fix only).
#
#   Rscript pipeline/R/03e_map_identifiers.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(arrow) })
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d) }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
source(file.path(ROOT, "pipeline", "R", "lib", "utils.R"))
source(file.path(ROOT, "pipeline", "R", "lib", "identifiers.R"))
EXP <- exp_dir()   # GEOV2/GEO_EXPO=v2 -> out/exposure_v2; GEO_EXPO=rwclean -> exposure_rwclean
# Transitioned 2026-06-13 to the v8.2 re-engineered mapping (293,861 matched events /
# 9,174 gvkeys, +7.3% vs the old v8; clean verify + 0-failure audit). Same schema/eventId
# key (drop-in). Old v8 map: ra_project/mapping/output/ec_ccm_map_v8.rds (pre-v8.2).
V8_PATH    <- file.path(ROOT, "ra_project/GEO_RA_mapping_v8.2/output/ec_ccm_map_v8.rds")
PRICE_PATH <- file.path(ROOT, "data/processed/CLEAN_QUARTERLY_PRICING_v2.rds")
say <- function(...) cat(sprintf(...), "\n")

# ---- 1. dedup exposure_calls to one row per Id ------------------------------
calls <- as.data.table(readRDS(file.path(EXP, "exposure_calls.rds")))
n_raw <- nrow(calls)
calls[, Id := as.character(Id)]
dedup <- unique(calls, by = "Id")                      # values identical across copies
say("[1] exposure_calls %s rows -> %s unique Id (dropped %s dup rows)",
    format(n_raw, big.mark=","), format(nrow(dedup), big.mark=","),
    format(n_raw - nrow(dedup), big.mark=","))

# ---- 2. attach event-time ticker -------------------------------------------
# The registry is exposure-value-independent (Ids/tickers/versions), so the V2
# track reuses the v1 registry rather than re-running 03d.
reg_path <- file.path(EXP, "event_registry.rds")
if (!file.exists(reg_path) && is_v2()) {
  reg_path <- file.path(ROOT, "out", "exposure", "event_registry.rds")
  say("[2] V2: reusing v1 event registry (%s)", reg_path)
}
reg <- as.data.table(readRDS(reg_path))
reg[, Id := as.character(Id)]
dedup[, ticker := NULL]
dedup <- reg[, .(Id, event_time_ticker, all_tickers, n_versions)][dedup, on = "Id"]

# Resolve to a PRICE-COMPATIBLE RIC. The LSEG price file was pulled from the call
# tickers, so the ~50% gap is a join-key FORMAT mismatch (the event-time ticker is
# often bare, e.g. MDP, while the price file keys on the suffixed RIC MDP.N), NOT
# missing firms. Two-step: (1) clean_ric (suffix kept) matches a price ticker
# directly; (2) base-ticker (suffix stripped) maps to a UNIQUE price ticker ->
# adopt it. Uniqueness IS the cross-listing guard (ambiguous bases stay unmatched).
price_tk <- unique(toupper(trimws(as.data.table(readRDS(PRICE_PATH))$Ticker)))
base_of  <- function(x) sub("\\..*$", "", clean_ric(x))          # strip exchange suffix + marker
suf_of   <- function(x) sub("^[^.]*", "", clean_ric(x))          # ".N"/".OQ" or "" if bare
uniq_base <- names(which(table(base_of(price_tk)) == 1L))        # bases mapping to exactly 1 price RIC
base2ptk  <- setNames(price_tk, base_of(price_tk))               # indexed only via uniq_base below
ck <- clean_ric(dedup$event_time_ticker); ric <- ck
bb <- base_of(dedup$event_time_ticker)
need <- !is.na(ck) & !(ck %in% price_tk) & !is.na(bb) & (bb %in% uniq_base)
cand <- base2ptk[bb[need]]
# ACCEPT the base-unique fallback ONLY when bare->suffixed or same exchange suffix;
# DROP cross-exchange (e.g. SU.PA -> SU.TO) -> the residual false-match guard.
cs <- suf_of(ck[need]); keep_fb <- (cs == "") | (cs == suf_of(cand))
rn <- ric[need]; rn[keep_fb] <- cand[keep_fb]; ric[need] <- rn
dedup[, ric := ric]
say("[2] price-compatible RIC: %s calls | direct=%s | base-fallback kept=%s (cross-exchange dropped=%s)",
    format(sum(!is.na(dedup$ric)), big.mark=","), format(sum(ck %in% price_tk), big.mark=","),
    format(sum(keep_fb), big.mark=","), format(sum(!keep_fb), big.mark=","))

# ---- 3. CRSP map (US robustness) via v8 on Id == eventId --------------------
v8 <- as.data.table(readRDS(V8_PATH))
v8[, eventId := as.character(eventId)]
v8cols <- intersect(c("eventId","permno","gvkey","siccd","us_can",
                      "ambiguity_flag","share_class_flag","matched"), names(v8))
dedup <- v8[, ..v8cols][dedup, on = c(eventId = "Id")]
setnames(dedup, "eventId", "Id")
say("[3] v8 join: calls matched to permno=%s (US/CA=%s)",
    format(sum(!is.na(dedup$permno)), big.mark=","),
    format(sum(dedup$us_can %in% TRUE), big.mark=","))

# ---- 4. RIC coverage vs the LSEG price universe ----------------------------
price_tk <- unique(as.data.table(readRDS(PRICE_PATH))$Ticker)
dedup[, ric_in_pricing := ric %in% price_tk]
# any-variant coverage (diagnostic: would a non-event-time variant match more?)
any_match <- function(s) { v <- clean_ric(strsplit(s, ";", fixed=TRUE)[[1]]); any(v %in% price_tk) }
cov_evt <- mean(dedup$ric_in_pricing, na.rm=TRUE)
cov_any <- mean(vapply(dedup$all_tickers, function(s) isTRUE(any_match(s)), logical(1)))
say("[4] RIC price-coverage: event-time=%.1f%% | any-variant=%.1f%%", 100*cov_evt, 100*cov_any)

saveRDS(dedup, file.path(EXP, "exposure_calls_dedup.rds"))
write_parquet(dedup, file.path(EXP, "exposure_calls_dedup.parquet"))

# ---- 5. firm-quarter panels -------------------------------------------------
measure_cols <- intersect(c("GeoExposure","GeoRisk","GeoSentiment","GeoExposureTFIDF",
  "GeoExposure_pr","GeoRisk_pr","GeoSentiment_pr","GeoExposureTFIDF_pr",
  "GeoSentimentPos","GeoSentimentNeg","GeoExposure_exen","GeoExposureTFIDF_exen",
  "GeoExposure_small","GeoExposureTFIDF_small"), names(dedup))

# RIC panel carries us_can/siccd/permno (returns are LSEG/RIC-keyed; the CRSP map
# supplies the US flag for the US-subsample + siccd for industry-adjust robustness).
fq_ric <- dedup[!is.na(ric), c(lapply(.SD, mean, na.rm=TRUE),
                .(n_calls = .N, us_can = any(us_can %in% TRUE),
                  permno = permno[!is.na(permno)][1], siccd = siccd[!is.na(siccd)][1])),
                by = .(ric, year, quarter), .SDcols = measure_cols]
fq_crsp <- dedup[!is.na(permno) & us_can %in% TRUE,
                 c(lapply(.SD, mean, na.rm=TRUE),
                   .(n_calls = .N, gvkey = gvkey[1], siccd = siccd[1])),
                 by = .(permno, year, quarter), .SDcols = measure_cols]
say("[5] firm-quarter: RIC=%s rows/%s firms | CRSP=%s rows/%s permno",
    format(nrow(fq_ric), big.mark=","), format(uniqueN(fq_ric$ric), big.mark=","),
    format(nrow(fq_crsp), big.mark=","), format(uniqueN(fq_crsp$permno), big.mark=","))

for (nm in c("fq_ric","fq_crsp")) {
  obj <- get(nm); suff <- sub("fq_", "", nm)
  saveRDS(obj, file.path(EXP, sprintf("exposure_firmquarter_%s.rds", suff)))
  write_parquet(obj, file.path(EXP, sprintf("exposure_firmquarter_%s.parquet", suff)))
}

# ---- 6. audit ---------------------------------------------------------------
old_fq <- if (file.exists(file.path(EXP, "exposure_firmquarter.rds")))
  as.data.table(readRDS(file.path(EXP, "exposure_firmquarter.rds"))) else NULL
audit <- list(
  n_calls_raw = n_raw, n_calls_dedup = nrow(dedup), dup_rows_removed = n_raw - nrow(dedup),
  n_cross_year = sum(dedup$n_versions > 1, na.rm=TRUE),
  ric_coverage_eventtime = round(cov_evt, 4), ric_coverage_anyvariant = round(cov_any, 4),
  crsp_matched = sum(!is.na(dedup$permno)), crsp_matched_usca = sum(dedup$us_can %in% TRUE),
  n_firms_ric = uniqueN(fq_ric$ric), n_firms_crsp = uniqueN(fq_crsp$permno),
  n_firms_old_ticker = if (!is.null(old_fq)) uniqueN(old_fq$ticker) else NA_integer_,
  ended_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
write_json(audit, file.path(EXP, "mapping_audit.json"), pretty = TRUE, auto_unbox = TRUE)
say("[6] DONE — RIC firms=%s (was %s raw tickers) | CRSP permno=%s | audit -> mapping_audit.json",
    format(audit$n_firms_ric, big.mark=","),
    if (!is.null(old_fq)) format(audit$n_firms_old_ticker, big.mark=",") else "NA",
    format(audit$n_firms_crsp, big.mark=","))
