# ==============================================================================
# pipeline/R/05m_realization_q1.R   (Stage 5 — Q1: REALIZATION of geoeconomic risk)
#
# Q1 of the life-cycle framework (Realize -> Price -> Trade). When a firm's
# geoeconomic discussion RISES (DGeoExpo = within-firm change), is there a negative
# CONTEMPORANEOUS return and a spike in idiosyncratic volatility — i.e. is the risk
# realized/priced AT the call? (Distinct from Q2, which tests the forward premium on
# the ex-ante LEVEL.) Two within-firm panel regressions, firm+quarter FE, cluster firm:
#   A: QuarterlyRet_t   ~ DGeoExpo_t (+ controls)   [expect NEGATIVE]
#   B: IVOL_t           ~ DGeoExpo_t (+ controls)   [expect POSITIVE / spike]
# DGeoExpo z-scored within quarter -> coef = effect per +1 SD increase in exposure.
#
#   Rscript pipeline/R/05m_realization_q1.R
# Output: out/analysis/realization_q1.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(jsonlite) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); say <- function(...) cat(sprintf(...),"\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment"); csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

p <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds"))))
setorder(p, Ticker, year, quarter)
# within-firm CHANGE in exposure (consecutive quarters only)
p[, qidx := year*4 + quarter]
for (m in MEAS) p[, paste0("d_",m) := { dv <- get(m) - shift(get(m)); dq <- qidx - shift(qidx)
                                        ifelse(dq == 1L, dv, NA_real_) }, by = Ticker]
# Audit fix 2026-06-12: Size = log(quarter-END MCap) and BM = BE/quarter-END MCap embed the
# quarter-t return that regression A explains (bad controls). Use the PRIOR quarter's values
# (consecutive quarters only); Momentum is built from past months and stays as-is.
for (v in c("Size","BM")) p[, paste0(v,"_l") := { lv <- shift(get(v)); dq <- qidx - shift(qidx)
                                                  ifelse(dq == 1L, lv, NA_real_) }, by = Ticker]
# IVOL (contemporaneous), from the daily-derived cache if present
iv <- file.path(ANA,"ivol_panel.rds")
if (file.exists(iv)) { ivp <- as.data.table(readRDS(iv)); p <- merge(p, ivp[,.(Ticker,Quarter,IVOL)], by=c("Ticker","Quarter"), all.x=TRUE) }
has_ivol <- "IVOL" %in% names(p)
say("[Q1 realization] %s firm-quarters | IVOL available: %s", format(nrow(p),big.mark=","), has_ivol)

run <- function(meas){
  dcol <- paste0("d_",meas); p[, dz := csz(get(dcol)), by = Quarter]
  d <- p[is.finite(dz)]
  g <- function(m) if(is.null(m)) list(coef=NA,t=NA,n=NA) else list(coef=unname(coef(m)["dz"]), t=unname(coef(m)["dz"]/se(m)["dz"]), n=m$nobs)
  # A: contemporaneous return on DGeoExpo — LAGGED Size/BM (audit fix; headline spec)
  rA  <- tryCatch(feols(QuarterlyRet_W ~ dz + Size_l + Momentum + BM_l | Ticker + Quarter, d, vcov=if (V2) ~Ticker + Quarter else ~Ticker), error=function(e)NULL)
  # comparisons: no controls, and the OLD endogenous-control spec (quarter-END Size/BM)
  rA0 <- tryCatch(feols(QuarterlyRet_W ~ dz | Ticker + Quarter, d, vcov=if (V2) ~Ticker + Quarter else ~Ticker), error=function(e)NULL)
  rAe <- tryCatch(feols(QuarterlyRet_W ~ dz + Size + Momentum + BM | Ticker + Quarter, d, vcov=if (V2) ~Ticker + Quarter else ~Ticker), error=function(e)NULL)
  # B: IVOL on DGeoExpo — lagged controls too
  rB  <- if (has_ivol) tryCatch(feols(IVOL ~ dz + Size_l + Momentum + BM_l | Ticker + Quarter, d[is.finite(IVOL)], vcov=if (V2) ~Ticker + Quarter else ~Ticker), error=function(e)NULL) else NULL
  list(measure=meas, ret_contemp=g(rA), ret_nocontrols=g(rA0), ret_endog_controls=g(rAe), ivol=g(rB))
}
res <- lapply(MEAS, run)
say("\n=== Q1 REALIZATION: effect of +1 SD rise in exposure (DGeoExpo), firm+quarter FE ===")
say("  %-18s %14s %8s   %14s %8s", "measure", "contemp Ret", "t", "IVOL", "t")
for(r in res) say("  %-18s %13.4f%% %8.2f   %14.6f %8.2f   [no-ctrl t=%.2f | old endog-ctrl t=%.2f]", r$measure,
   100*ifelse(is.na(r$ret_contemp$coef),NA,r$ret_contemp$coef), r$ret_contemp$t,
   ifelse(is.na(r$ivol$coef),NA,r$ivol$coef), r$ivol$t, r$ret_nocontrols$t, r$ret_endog_controls$t)
say("\n  (contemp Ret expected NEGATIVE if risk realized at the call; IVOL expected POSITIVE/spike)")
write_json(list(results=res, n=nrow(p), has_ivol=has_ivol), vS(file.path(ANA,"realization_q1.json")), pretty=TRUE, auto_unbox=TRUE)
say("  wrote out/analysis/realization_q1.json")
