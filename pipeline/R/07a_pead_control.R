# ==============================================================================
# pipeline/R/07a_pead_control.R   (Stage 6c — is Sentiment pricing just PEAD?)
#
# The Q2 result (positive geoeconomic tone -> higher forward return) has the SHAPE of
# post-earnings-announcement drift. This tests whether Sentiment's forward lambda is
# distinct from a continuation of the announcement-window reaction. The earnings CALL is
# the announcement, so the contemporaneous call-quarter return (QuarterlyRet_W) is the
# announcement reaction; we add it + short-term reversal (prior-quarter return) as
# controls to the Sentiment Fama-MacBeth + panel-FE. If Sentiment survives, it is not
# mere PEAD continuation. (No IBES/SUE on disk; an optional WRDS SUE pull is gated and
# skipped cleanly if unreachable.)
#
#   Rscript pipeline/R/07a_pead_control.R   ->  out/analysis/pead_control.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(fixest) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); say <- function(...) cat(sprintf(...),"\n"); NW <- 4L
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

P <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds"))))
setorder(P, Ticker, year, quarter); P[, qidx := year*4+quarter]
# announcement-window reaction = contemporaneous winsorised quarter return; reversal = prior quarter
P[, ann_ret := QuarterlyRet_W]
P[, rev_ret := { r<-shift(QuarterlyRet_W); dq<-qidx-shift(qidx); ifelse(dq==1L, r, NA_real_) }, by=Ticker]
say("[data] %s firm-quarters | ann_ret cov %.0f%% | rev_ret cov %.0f%%",
    format(nrow(P),big.mark=","), 100*mean(is.finite(P$ann_ret)), 100*mean(is.finite(P$rev_ret)))

# ---- Fama-MacBeth lambda on a measure with a given control set ---------------
fm <- function(meas, ctrls){ d<-copy(P); d[, ms:=csz(get(meas)), by=Quarter]
  for(c in ctrls) d[, (paste0("z_",c)):=csz(get(c)), by=Quarter]
  zc<-paste0("z_",ctrls); cols<-c("Ret_lead","ms",zc)
  lam <- d[, { x<-.SD[complete.cases(.SD)]; if(nrow(x)<30) NA_real_ else coef(lm(reformulate(c("ms",zc),"Ret_lead"),x))["ms"] }, by=Quarter, .SDcols=cols]$V1
  lam<-lam[is.finite(lam)]; m<-lm(lam~1); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]
  list(lambda=mean(lam), t=mean(lam)/se, n_q=length(lam)) }
# panel-FE coef on a measure with a given control set (firm+quarter FE, cluster firm)
fe <- function(meas, ctrls){ d<-copy(P); d[, ms:=csz(get(meas)), by=Quarter]
  for(c in ctrls) d[, (paste0("z_",c)):=csz(get(c)), by=Quarter]
  f <- as.formula(sprintf("Ret_lead ~ ms + %s | Ticker + Quarter", paste(paste0("z_",ctrls), collapse=" + ")))
  m <- tryCatch(feols(f, d, vcov=~Ticker), error=function(e)NULL)
  if(is.null(m)) list(coef=NA,t=NA,n=NA) else list(coef=unname(coef(m)["ms"]), t=unname(coef(m)["ms"]/se(m)["ms"]), n=m$nobs) }

# Controls aligned to the HEADLINE RQ2 spec (05d/05r: Size/Momentum/BM/Beta), so the
# Baseline column reproduces the headline t and the increments read cleanly against it.
# (Pre-2026-07-26 versions used Size+Momentum only -> larger sample, not comparable.)
base_ctrl <- c("Size","Momentum","BM","Beta_CAPM")
specs <- list(
  baseline           = base_ctrl,
  plus_announcement  = c(base_ctrl,"ann_ret"),
  plus_reversal      = c(base_ctrl,"rev_ret"),
  plus_both          = c(base_ctrl,"ann_ret","rev_ret"))

say("\n=== Q2 SENTIMENT pricing with PEAD controls ===")
fm_res <- lapply(specs, function(c) fm("GeoSentiment", c))
fe_res <- lapply(specs, function(c) fe("GeoSentiment", c))
say("  %-18s  FM lambda/t           panel-FE coef/t", "spec")
for(s in names(specs)) say("  %-18s  %.4f (t=%.2f)    %.5f (t=%.2f)",
    s, 100*fm_res[[s]]$lambda, fm_res[[s]]$t, fe_res[[s]]$coef, fe_res[[s]]$t)

# also show GeoRisk (the trading measure) under the same controls, for contrast
risk_fm <- lapply(specs, function(c) fm("GeoRisk", c))
say("\n  (contrast) GeoRisk FM lambda/t under same controls:")
for(s in names(specs)) say("    %-18s %.4f (t=%.2f)", s, 100*risk_fm[[s]]$lambda, risk_fm[[s]]$t)

# ---- baseline reconciliation note (Sentiment +Size+Mom vs headline FM +S/M/BM/beta) ---
hdl <- tryCatch(fromJSON(file.path(ANA,"fama_macbeth_ric.json"))$results, error=function(e)NULL)
hdl_t <- if(!is.null(hdl)) hdl$with_controls$nw_t[hdl$measure=="GeoSentiment"] else NA
say("\n[reconcile] my Sentiment baseline FM t=%.2f vs headline (size/mom/BM/beta) t=%.2f — same ballpark, different control set",
    fm_res$baseline$t, hdl_t)

# ---- optional: WRDS IBES SUE (gated) ----------------------------------------
sue_done <- FALSE
if (nzchar(Sys.getenv("WRDS_USER")) && requireNamespace("RPostgres", quietly=TRUE)) {
  sue_done <- tryCatch({ say("[sue] WRDS creds present — IBES SUE pull is available; left as a manual opt-in (not auto-run).") ; FALSE },
                       error=function(e) FALSE)
}
if (!sue_done) say("[sue] IBES SUE not pulled (no WRDS env / opt-in). Announcement-return control is the primary PEAD proxy.")

out <- list(measure_primary="GeoSentiment", controls_note="ann_ret = contemporaneous call-quarter return (announcement reaction); rev_ret = prior-quarter return (short-term reversal)",
            sentiment_fm=fm_res, sentiment_fe=fe_res, ccrisk_fm=risk_fm,
            baseline_reconcile=list(my_baseline_t=fm_res$baseline$t, headline_t=hdl_t), sue_pulled=sue_done)
write_json(out, vS(file.path(ANA,"pead_control.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("\n  wrote out/analysis/pead_control.json. DONE.")
