# ==============================================================================
# pipeline/R/05f_panel_fe.R   (Stage 5 — Q2 complement: panel fixed-effects)
#
# Ret_{t+1} ~ GeoExposure_std (+ controls) | Firm FE + Quarter FE, SE clustered by
# firm. The within-firm, within-quarter pricing test (complements the cross-sectional
# Fama-MacBeth in 05d). Mirrors legacy scripts/03_analysis/02_panel_fe_q2.R across
# all exposure measures. Exposure standardised within quarter -> coef = return per 1 SD.
#
#   Rscript pipeline/R/05f_panel_fe.R [ric|crsp] [us]
# Output: out/analysis/panel_fe_<panel>.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(jsonlite) })
.find_root <- function() { d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") { if (file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d <- dirname(d) }
  stop("no root") }
ROOT <- .find_root(); ANA <- file.path(ROOT, "out", "analysis")
args <- commandArgs(TRUE); which_panel <- if (length(args)>=1) args[1] else "ric"; us_only <- length(args)>=2 && args[2]=="us"
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


p <- as.data.table(readRDS(vS(file.path(ANA, sprintf("panel_%s.rds", which_panel)))))
if (us_only) p <- p[us_can == TRUE]
p <- p[!is.na(Ret_lead)]
csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}
say("[panel=%s%s] %s firm-quarters | %s firms", which_panel, if(us_only)" US" else "",
    format(nrow(p), big.mark=","), format(uniqueN(p$Ticker), big.mark=","))

run <- function(meas) {
  if (!meas %in% names(p)) return(NULL)
  p[, mstd := csz(get(meas)), by = Quarter]
  d <- p[is.finite(mstd)]
  # M3 firm+time FE (baseline) and M4 + controls
  m3 <- tryCatch(feols(Ret_lead ~ mstd | Ticker + Quarter, data = d, vcov = if (V2) ~Ticker + Quarter else ~Ticker), error=function(e) NULL)
  m4 <- tryCatch(feols(Ret_lead ~ mstd + Size + Momentum + BM | Ticker + Quarter, data = d, vcov = if (V2) ~Ticker + Quarter else ~Ticker), error=function(e) NULL)
  g <- function(m) if (is.null(m)) list(coef=NA,t=NA,n=NA) else
    list(coef = unname(coef(m)["mstd"]), t = unname(coef(m)["mstd"]/se(m)["mstd"]), n = m$nobs)
  list(measure = meas, fe_firm_time = g(m3), fe_plus_controls = g(m4))
}
res <- Filter(Negate(is.null), lapply(MEASURES, run))
say("\n  %-20s %12s %8s   %12s %8s", "measure","coef(FE)","t","coef(+ctrl)","t")
for (r in res) say("  %-20s %11.4f%% %8.2f   %11.4f%% %8.2f", r$measure,
    100*r$fe_firm_time$coef, r$fe_firm_time$t, 100*r$fe_plus_controls$coef, r$fe_plus_controls$t)
say("\n  (coef = next-qtr return per +1 SD exposure | firm+quarter FE, SE clustered by firm)")
suff <- paste0(which_panel, if(us_only)"_us" else "")
write_json(list(panel=suff, results=res), vS(file.path(ANA, sprintf("panel_fe_%s.json", suff))), pretty=TRUE, auto_unbox=TRUE)
say("  wrote out/analysis/panel_fe_%s.json", suff)
