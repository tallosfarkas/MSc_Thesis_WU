# ==============================================================================
# pipeline/R/05e_volatility_q1.R   (Stage 5 — Q1: exposure & idiosyncratic vol)
#
# Does geoeconomic exposure relate to firm risk? IVOL_{i,t} = sd of residuals from
# a daily FF3 regression within each firm-quarter; then
#   IVOL ~ GeoExposure_std (+ controls) | Firm FE + Quarter FE  (SE clustered by firm).
# Mirrors legacy scripts/03_analysis/01_volatility_q1.R on the deduped RIC panel.
# Daily IVOL is cached (out/analysis/ivol_panel.rds) so re-runs skip the heavy step.
#
#   Rscript pipeline/R/05e_volatility_q1.R
# Output: out/analysis/ivol_panel.rds + volatility_q1.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(jsonlite); library(lubridate) })
.find_root <- function() { d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") { if (file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d <- dirname(d) }
  stop("no root") }
ROOT <- .find_root(); ANA <- file.path(ROOT, "out", "analysis")
MIN_DAYS <- 40L
MEASURES <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment","GeoExposure_pr","GeoExposureTFIDF_pr")
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


panel <- as.data.table(readRDS(vS(file.path(ANA, "panel_ric.rds"))))
ivol_path <- file.path(ANA, "ivol_panel.rds")

ivol <- if (file.exists(ivol_path)) as.data.table(readRDS(ivol_path)) else NULL
if (!is.null(ivol) && "TVOL" %in% names(ivol)) {
  say("[ivol] loaded cache (%s firm-quarters)", format(nrow(ivol), big.mark=","))
} else {
  if (!is.null(ivol)) say("[ivol] cache lacks TVOL -> recomputing (heavy) ...")
  else                say("[ivol] computing daily FF3 residual vol + total vol (heavy) ...")
  rics <- unique(panel$Ticker)
  d <- as.data.table(read_parquet(file.path(ROOT, "data/processed/LSEG_Final_Panel.parquet"),
                                  col_select = c("Date","Ticker","Ret")))
  d <- d[Ticker %in% rics & is.finite(Ret)]
  d[, Date := as.Date(Date)]
  ff3 <- fread(file.path(ROOT, "data/inputs/ff3_factors_daily.csv")); ff3[, Date := as.Date(Date)]
  d <- merge(d, ff3[, .(Date, MktRF, SMB, HML, RF)], by = "Date")
  d[, ExcessRet := Ret - RF]; d[, Quarter := floor_date(Date, "quarter")]
  d <- d[is.finite(ExcessRet) & is.finite(MktRF) & is.finite(SMB) & is.finite(HML)]
  say("[ivol] daily rows after merge: %s | tickers %s", format(nrow(d), big.mark=","), format(uniqueN(d$Ticker), big.mark=","))
  # IVOL = sd of daily FF3 residual (idiosyncratic); TVOL = sd of daily excess return
  # (total = systematic + idiosyncratic). Randl Q&A: could geopolitical risk show up
  # in SYSTEMATIC risk? -> report both, same firm-quarter window.
  ivol <- d[, {
    if (.N < MIN_DAYS) .(IVOL = NA_real_, TVOL = NA_real_, n_days = .N)
    else { fit <- .lm.fit(cbind(1, MktRF, SMB, HML), ExcessRet)
           .(IVOL = sd(fit$residuals), TVOL = sd(ExcessRet), n_days = .N) }
  }, by = .(Ticker, Quarter)][!is.na(IVOL)]
  saveRDS(ivol, ivol_path)
  say("[ivol] computed + cached: %s firm-quarters", format(nrow(ivol), big.mark=","))
}

# merge IVOL + TVOL to exposure panel (ric==Ticker, Quarter)
m <- merge(panel, ivol[, .(Ticker, Quarter, IVOL, TVOL)], by = c("Ticker","Quarter"))
m[, IVOL_w := pmin(IVOL, quantile(IVOL, 0.99, na.rm=TRUE))]            # light winsor
m[, TVOL_w := pmin(TVOL, quantile(TVOL, 0.99, na.rm=TRUE))]
csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}
say("[ivol] merged analysis rows: %s | firms %s", format(nrow(m), big.mark=","), format(uniqueN(m$Ticker), big.mark=","))

run <- function(meas, yv = "IVOL_w") {
  if (!meas %in% names(m)) return(NULL)
  m[, mstd := csz(get(meas)), by = Quarter]; d <- m[is.finite(mstd) & is.finite(get(yv))]
  f <- stats::as.formula(sprintf("%s ~ mstd + Size + Momentum + BM | Ticker + Quarter", yv))
  mod <- tryCatch(feols(f, data = d, vcov = ~Ticker), error=function(e) NULL)
  if (is.null(mod)) return(list(measure=meas, coef=NA, t=NA, n=NA))
  list(measure = meas, coef = unname(coef(mod)["mstd"]), t = unname(coef(mod)["mstd"]/se(mod)["mstd"]), n = mod$nobs)
}
res  <- Filter(Negate(is.null), lapply(MEASURES, run, yv = "IVOL_w"))   # idiosyncratic
rest <- Filter(Negate(is.null), lapply(MEASURES, run, yv = "TVOL_w"))   # total
say("\n  %-20s %12s %7s %12s %7s", "measure", "IVOL coef", "t", "TVOL coef", "t")
for (i in seq_along(res)) say("  %-20s %12.5f %7.2f %12.5f %7.2f",
    res[[i]]$measure, res[[i]]$coef, res[[i]]$t, rest[[i]]$coef, rest[[i]]$t)
say("\n  (coef = change in quarterly vol per +1 SD exposure | firm+quarter FE, cluster firm)")
write_json(list(panel="ric", min_days=MIN_DAYS, results=res, tvol=rest),
           vS(file.path(ANA, "volatility_q1.json")), pretty=TRUE, auto_unbox=TRUE, digits=8)
say("  wrote out/analysis/volatility_q1.json")
