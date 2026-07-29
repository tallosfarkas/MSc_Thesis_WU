# ==============================================================================
# pipeline/R/05_build_analysis_panel.R   (Stage 5 — analysis panel, LOCAL)
#
# Merges the DEDUPED exposure panel with returns + controls + beta into the
# firm-quarter analysis panel that feeds the return regressions. Headline uses the
# GLOBAL RIC panel (exposure_firmquarter_ric, joins LSEG returns); a US/CRSP variant
# is built from exposure_firmquarter_crsp for robustness.
#
# Timing (no look-ahead): exposure is measured in quarter t; the dependent variable
# is the NEXT quarter's return (Ret_lead), kept only when quarters are consecutive.
# Exposure is z-scored WITHIN quarter (cross-sectional standardisation, Stage-5).
#
#   Rscript pipeline/R/05_build_analysis_panel.R
# Output: out/analysis/panel_ric.rds (headline) + panel_crsp.rds (US)
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d) }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
# V2 correctness track (audit 2026-06-12): GEOV2=1 reads out/exposure_v2 and writes
# *_v2 panels; Ret_lead comes from the PRICING series (no call-in-t+1 selection).
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2  <- FIX
vS  <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
EXP <- file.path(ROOT, "out", if (EXV2) "exposure_v2" else "exposure")
ANA <- file.path(ROOT, "out", "analysis"); if (!dir.exists(ANA)) dir.create(ANA, recursive = TRUE)
PROC <- file.path(ROOT, "data", "processed")
say <- function(...) cat(sprintf(...), "\n")
if (V2) say("[V2] exposure dir=%s | outputs get _v2 suffix | pricing-based Ret_lead", EXP)

EXPO_MEAS <- c("GeoExposure","GeoRisk","GeoSentiment","GeoExposureTFIDF",
               "GeoExposure_pr","GeoExposureTFIDF_pr")          # z-scored within quarter

# prefer the daily-derived quarterly panel (post-topup, one source of truth) if present;
# else fall back to the legacy LSEG quarterly product.
.qd <- file.path(ANA, "quarterly_pricing_daily.rds")
if (file.exists(.qd)) {
  say("[pricing] using daily-derived quarterly: %s", .qd)
  pricing <- as.data.table(readRDS(.qd))
} else {
  pricing <- as.data.table(readRDS(file.path(PROC, "CLEAN_QUARTERLY_PRICING_v2.rds")))
}
controls <- as.data.table(readRDS(file.path(PROC, "CONTROLS_PANEL.rds")))
beta     <- as.data.table(readRDS(file.path(PROC, "BETA_PANEL.rds")))

# V2: forward return computed on the PRICING series itself, so quarter t is kept
# whenever the firm is still PRICED in t+1 — no conditioning on a t+1 call.
if (V2) {
  setorder(pricing, Ticker, Quarter)
  pricing[, Ret_lead_px := shift(QuarterlyRet_W, 1L, type = "lead"), by = Ticker]
  if (!"QuarterlyRet" %in% names(pricing)) pricing[, QuarterlyRet := QuarterlyRet_W]
  pricing[, Ret_lead_raw_px := shift(QuarterlyRet, 1L, type = "lead"), by = Ticker]
  pricing[, Qn := shift(Quarter, 1L, type = "lead"), by = Ticker]
  pricing[is.na(Qn) | as.integer(Qn - Quarter) > 95L,
          c("Ret_lead_px", "Ret_lead_raw_px") := .(NA_real_, NA_real_)]
  pricing[, Qn := NULL]
}

build_panel <- function(fq_file, idcol, label) {
  fq <- as.data.table(readRDS(file.path(EXP, fq_file)))
  setnames(fq, idcol, "Ticker")
  fq[, Quarter := as.Date(ISOdate(year, (quarter - 1L) * 3L + 1L, 1L))]   # quarter start

  pcols <- intersect(c("Ticker","Quarter","QuarterlyRet_W","QuarterlyRet","MCap_QEnd",
                       "Ret_lead_px","Ret_lead_raw_px"), names(pricing))
  p <- merge(fq, pricing[, ..pcols], by = c("Ticker", "Quarter"))
  p <- merge(p, controls[, .(Ticker, Quarter, Size, Momentum, BM, MCap_Q)],
             by = c("Ticker", "Quarter"), all.x = TRUE)
  p <- merge(p, beta[, .(Ticker, Quarter, Beta_CAPM, Beta_FF3)],
             by = c("Ticker", "Quarter"), all.x = TRUE)
  p[is.na(MCap_Q), MCap_Q := MCap_QEnd]

  # forward (t+1) return, consecutive-quarter only (<=95 days gap)
  setorder(p, Ticker, Quarter)
  if (V2) {
    # audit fix: pricing-based lead — no selection on a call existing in t+1
    p[, Ret_lead := Ret_lead_px]
    p[, Ret_lead_raw := Ret_lead_raw_px]
    p[, c("Ret_lead_px", "Ret_lead_raw_px") := NULL]
  } else {
    p[, Ret_lead := shift(QuarterlyRet_W, 1L, type = "lead"), by = Ticker]
    p[, Qnext := shift(Quarter, 1L, type = "lead"), by = Ticker]
    p[is.na(Qnext) | as.integer(Qnext - Quarter) > 95L, Ret_lead := NA_real_]
    p[, Qnext := NULL]
  }

  # within-quarter cross-sectional z-score of each exposure measure
  zc <- intersect(EXPO_MEAS, names(p))
  for (m in zc) p[, paste0(m, "_z") := {
    s <- sd(get(m), na.rm = TRUE); if (is.na(s) || s == 0) NA_real_ else (get(m) - mean(get(m), na.rm = TRUE)) / s
  }, by = Quarter]

  say("[%s] %s firm-quarters | %s firms | with Ret_lead=%s | yrs %d-%d",
      label, format(nrow(p), big.mark=","), format(uniqueN(p$Ticker), big.mark=","),
      format(sum(!is.na(p$Ret_lead)), big.mark=","), min(p$year), max(p$year))
  p
}

# Single returns panel: GLOBAL RIC (LSEG returns). us_can flag + siccd ride along
# from the CRSP map for the US-subsample + industry-adjust robustness (returns stay
# LSEG/RIC-keyed; no CRSP return series in the repo). panel_ric[us_can==TRUE] = US.
panel_ric <- build_panel("exposure_firmquarter_ric.rds", "ric", "RIC/global")
say("[US subset] %s firm-quarters | %s firms (us_can==TRUE)",
    format(panel_ric[us_can == TRUE, .N], big.mark=","),
    format(panel_ric[us_can == TRUE, uniqueN(Ticker)], big.mark=","))

saveRDS(panel_ric, vS(file.path(ANA, "panel_ric.rds"))); write_parquet(panel_ric, vS(file.path(ANA, "panel_ric.parquet")))
say("=== Stage 5 panel written -> %s (us_can flags US subsample) ===", vS("out/analysis/panel_ric.rds"))
