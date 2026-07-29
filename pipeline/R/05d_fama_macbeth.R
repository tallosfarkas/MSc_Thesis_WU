# ==============================================================================
# pipeline/R/05d_fama_macbeth.R   (Stage 5 — Q2: is GeoExposure priced?)
#
# Fama-MacBeth: each quarter run a cross-sectional OLS of next-quarter return on
# WITHIN-QUARTER-STANDARDISED exposure (+ controls), collect the slope lambda_t,
# then Newey-West (4 lags) on the lambda time series. lambda = expected return per
# 1-SD of exposure. Mirrors the legacy scripts/03_analysis/03_fama_macbeth_q2.R but
# on the deduped panel and across ALL exposure measures. This is the higher-power,
# controls-adjusted test (vs the univariate portfolio L/S in 05b).
#
#   Rscript pipeline/R/05d_fama_macbeth.R [ric|crsp] [us]
# Output: out/analysis/fama_macbeth_<panel>.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d) }
  stop("Could not locate project root.")
}
ROOT <- .find_root(); ANA <- file.path(ROOT, "out", "analysis")
args <- commandArgs(TRUE)
which_panel <- if (length(args) >= 1) args[1] else "ric"
us_only     <- length(args) >= 2 && args[2] == "us"
MIN_FIRMS <- 30L; MIN_Q <- 8L; NW_LAGS <- 4L
MEASURES <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment",
              "GeoExposure_pr","GeoExposureTFIDF_pr")
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


p <- as.data.table(readRDS(vS(file.path(ANA, sprintf("panel_%s.rds", which_panel)))))
if (us_only) p <- p[us_can == TRUE]
p <- p[!is.na(Ret_lead)]
if (V2) { setorder(p, Ticker, Quarter); p[, N_Obs := seq_len(.N), by = Ticker]   # V2: past-only expanding count
} else  { p[, N_Obs := .N, by = Ticker] }
p <- p[N_Obs >= MIN_Q]            # min quarters per firm
csz <- function(x) { s <- sd(x, na.rm=TRUE); if (is.na(s)||s==0) rep(NA_real_, length(x)) else (x-mean(x,na.rm=TRUE))/s }
for (v in c("Size","Momentum","BM","Beta_CAPM")) p[, paste0(v,"_std") := csz(get(v)), by = Quarter]
say("[panel=%s%s] %s firm-quarters | %s firms | quarters %d",
    which_panel, if (us_only) " US" else "", format(nrow(p), big.mark=","),
    format(uniqueN(p$Ticker), big.mark=","), uniqueN(p$Quarter))

nw_t <- function(lam) {                                          # NW t on the lambda series
  lam <- lam[is.finite(lam)]; m <- lm(lam ~ 1)
  se <- if (have_nw) sqrt(sandwich::NeweyWest(m, lag = NW_LAGS, prewhite = FALSE, adjust = TRUE)[1,1])
        else summary(m)$coef[1,2]
  c(mean = mean(lam), t = mean(lam)/se, nq = length(lam))
}
fm_one <- function(meas, ctrl) {
  p[, mstd := csz(get(meas)), by = Quarter]
  rhsv <- if (ctrl) c("mstd","Size_std","Momentum_std","BM_std","Beta_CAPM_std") else "mstd"
  cols <- c("Ret_lead", rhsv)
  lam <- p[, {
    d <- .SD[complete.cases(.SD)]                         # all RHS + y finite
    if (nrow(d) < MIN_FIRMS) NA_real_
    else coef(lm(as.formula(paste("Ret_lead ~", paste(rhsv, collapse="+"))), data = d))["mstd"]
  }, by = Quarter, .SDcols = cols]$V1
  s <- nw_t(lam); list(lambda_q = unname(s["mean"]), nw_t = unname(s["t"]), n_quarters = unname(s["nq"]))
}

res <- lapply(MEASURES, function(m) if (m %in% names(p))
  list(measure = m, univariate = fm_one(m, FALSE), with_controls = fm_one(m, TRUE)) else NULL)
res <- Filter(Negate(is.null), res)

say("\n  %-20s %12s %8s   %12s %8s", "measure", "lambda(uni)", "t", "lambda(ctrl)", "t")
for (r in res)
  say("  %-20s %11.4f%% %8.2f   %11.4f%% %8.2f", r$measure,
      100*r$univariate$lambda_q, r$univariate$nw_t,
      100*r$with_controls$lambda_q, r$with_controls$nw_t)
say("\n  (lambda = mean quarterly return per +1 SD of exposure; NW(%d) t; |t|>1.96 ~ 5%%)", NW_LAGS)
suff <- paste0(which_panel, if (us_only) "_us" else "")
write_json(list(panel = suff, min_firms = MIN_FIRMS, min_quarters = MIN_Q, results = res),
           vS(file.path(ANA, sprintf("fama_macbeth_%s.json", suff))), pretty=TRUE, auto_unbox=TRUE)
say("  wrote out/analysis/fama_macbeth_%s.json", suff)
