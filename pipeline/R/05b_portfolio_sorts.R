# ==============================================================================
# pipeline/R/05b_portfolio_sorts.R   (Stage 5 — quintile L/S + FF5 spanning)
#
# Headline cross-sectional test: sort firms into quintiles on GeoExposure each
# quarter, hold next quarter, and test the high-minus-low (Q5-Q1) spread for alpha
# after FF5. Equal- AND value-weighted; Newey-West SEs. Mirrors the legacy
# scripts/03_analysis/04_portfolio_sorts_q3.R methodology on the new deduped panel.
#
#   Rscript pipeline/R/05b_portfolio_sorts.R [ric|crsp]   (default ric)
# Output: out/analysis/portfolio_sorts_<panel>.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE) && requireNamespace("lmtest", quietly = TRUE)
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d) }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
ANA  <- file.path(ROOT, "out", "analysis")
args <- commandArgs(TRUE)
which_panel <- if (length(args) >= 1) args[1] else "ric"
us_only     <- length(args) >= 2 && args[2] == "us"
N_QUINT <- 5L; NW_LAGS <- 4L
MEASURES <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment",
              "GeoExposure_pr","GeoExposureTFIDF_pr")     # all four Sautner variants + pruned
say <- function(...) cat(sprintf(...), "\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"


panel <- as.data.table(readRDS(vS(file.path(ANA, sprintf("panel_%s.rds", which_panel)))))
if (us_only) panel <- panel[us_can == TRUE]
say("[panel=%s%s] %s firm-quarters | %s with Ret_lead",
    which_panel, if (us_only) " US-only" else "", format(nrow(panel), big.mark=","),
    format(sum(!is.na(panel$Ret_lead)), big.mark=","))

# FF5 monthly -> quarterly (compound). Audit fix 2026-06-12: the L/S series is keyed by the
# FORMATION quarter t but holds Ret_lead (the t+1 return), so the factors must be the t+1
# realisations — shift the factor key back one quarter so merge-by-Quarter pairs them right.
ff5 <- fread(file.path(ROOT, "data", "inputs", "ff5_factors_monthly.csv"))
ff5[, Date := as.Date(Date)]; ff5[, Quarter := floor_date(Date, "quarter")]
fcols <- intersect(names(ff5), c("MktRF","SMB","HML","RMW","CMA","RF"))
ff5q <- ff5[, lapply(.SD, function(x) prod(1 + x) - 1), .SDcols = fcols, by = Quarter]
ff5q[, Quarter := Quarter %m-% months(3)]   # factor quarter t+1 keyed to formation quarter t

span <- function(d, col) {
  d <- d[!is.na(get(col)) & !is.na(MktRF)]
  m <- lm(as.formula(sprintf("%s ~ MktRF+SMB+HML+RMW+CMA", col)), data = d)
  se <- if (have_nw) sqrt(diag(sandwich::NeweyWest(m, lag = NW_LAGS, prewhite = FALSE, adjust = TRUE)))
        else sqrt(diag(vcov(m)))
  a <- coef(m)["(Intercept)"]
  list(alpha_q = unname(a), nw_t = unname(a / se["(Intercept)"]), r2 = summary(m)$r.squared, n = nrow(d))
}
ls_series <- function(d) { w <- dcast(d, Quarter ~ Quintile, value.var="Ret")
  w[, LS := get(as.character(N_QUINT)) - get("1")]; w }

run_measure <- function(meas) {
  if (!meas %in% names(panel)) return(NULL)
  d <- panel[!is.na(get(meas)) & !is.na(Ret_lead)]
  if (RAW && "Ret_lead_raw" %in% names(d)) d[, Ret_lead := Ret_lead_raw]   # V2: raw-return portfolios
  # rank-based quintiles (robust to the many ties/zeros in Risk & Sentiment, where
  # quantile breaks would collapse); always yields N_QUINT non-empty bins per quarter
  d[, Quintile := as.integer(ceiling(frank(get(meas), ties.method=TIEM) / .N * N_QUINT)),
    by = Quarter]
  ew <- d[!is.na(Quintile), .(Ret = mean(Ret_lead, na.rm=TRUE)), by=.(Quarter, Quintile)]
  vw <- d[!is.na(Quintile) & MCap_Q > 0, .(Ret = weighted.mean(Ret_lead, MCap_Q, na.rm=TRUE)), by=.(Quarter, Quintile)]
  ew_w <- merge(ls_series(ew), ff5q, by="Quarter"); vw_w <- merge(ls_series(vw), ff5q, by="Quarter")
  list(measure = meas, n_obs = nrow(d),
       ew_ls_mean_q = mean(ew_w$LS, na.rm=TRUE), vw_ls_mean_q = mean(vw_w$LS, na.rm=TRUE),
       ew_ls_ff5 = span(ew_w[!is.na(LS)], "LS"), vw_ls_ff5 = span(vw_w[!is.na(LS)], "LS"))
}

res <- Filter(Negate(is.null), lapply(MEASURES, run_measure))
say("\n  %-20s %10s %10s %12s %10s %12s", "measure", "EW raw/q", "EW a/q", "EW NW-t", "VW a/q", "VW NW-t")
for (r in res)
  say("  %-20s %9.3f%% %9.3f%% %12.2f %9.3f%% %12.2f", r$measure,
      100*r$ew_ls_mean_q, 100*r$ew_ls_ff5$alpha_q, r$ew_ls_ff5$nw_t,
      100*r$vw_ls_ff5$alpha_q, r$vw_ls_ff5$nw_t)
suff <- paste0(which_panel, if (us_only) "_us" else "")
write_json(list(panel = suff, n_quintiles = N_QUINT, nw = have_nw, results = res),
           vS(file.path(ANA, sprintf("portfolio_sorts_%s.json", suff))), pretty=TRUE, auto_unbox=TRUE)
say("\n  wrote out/analysis/portfolio_sorts_%s.json", suff)
