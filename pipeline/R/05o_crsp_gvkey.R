# ==============================================================================
# pipeline/R/05o_crsp_gvkey.R   (Stage 5 — US/CRSP robustness aggregated to the FIRM)
#
# 05i/05j key the CRSP layer on PERMNO (a security), so dual-class firms split into
# two return series and the v8 "which permno" choice is arbitrary (~2% of gvkeys have
# >1 permno). This re-runs Q2/Q3 on the CRSP layer aggregated to GVKEY (the firm):
# the permno pipeline is reproduced EXACTLY (hold exposure -> merge returns ->
# winsorise the security return per period) and only the FINAL step collapses to the
# firm — each gvkey counted once, share-class returns combined value-weighted (lagged
# ME). Single-permno firms are byte-identical to 05j, so any change is purely the
# dual-class collapse. (Confirms the GeoRisk premium is firm-level, not a share-class
# double-counting artifact.)
#
#   Rscript pipeline/R/05o_crsp_gvkey.R   ->  out/analysis/crsp_gvkey.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
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

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment","GeoExposure_pr","GeoExposureTFIDF_pr")
csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

fq <- as.data.table(readRDS(file.path(ROOT, if (EXV2) "out/exposure_v2" else "out/exposure", "exposure_firmquarter_crsp.rds")))
fq <- fq[!is.na(gvkey)]; fq[, permno := as.integer(permno)]
fq[, form_q := as.Date(ISOdate(year, (quarter-1L)*3L+1L, 1L))]
mcols <- intersect(MEAS, names(fq))
n_multi <- sum(fq[, uniqueN(permno), by=gvkey]$V1 > 1)
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv"))
nw_t <- function(v,lag){v<-v[is.finite(v)]; m<-lm(v~1); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=lag,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]; c(mean=mean(v),t=mean(v)/se)}

# collapse a winsorised PERMNO-level held panel to the firm (gvkey): VW share-class
# returns by lagged ME, sum ME, average exposure across classes, count firm ONCE.
# Audit fix 2026-06-12: also carry MCap_w = the PRE-RETURN firm cap (prior-month ME at
# monthly freq; formation-quarter-end ME at quarterly, where R is already the lead
# return) so the quintile VW weights and the size control are predetermined — the old
# MCap = sum(same-period-end me) embeds the monthly return (look-ahead).
to_gvkey <- function(p0, pcol, wcol = "me_lag"){
  p0[, w := get(wcol)]; p0[is.na(w) | w<=0, w := me]
  p0[, c(.(Ret = weighted.mean(rw, w), MCap = sum(me, na.rm=TRUE), MCap_w = sum(w, na.rm=TRUE)),
         lapply(.SD, mean, na.rm=TRUE)), by=c("gvkey", pcol), .SDcols=mcols]
}

run_freq <- function(freq){
  if (freq=="monthly"){
    ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[, permno:=as.integer(permno)]
    setnames(ret, c("RetM","MCap_MEnd"), c("R","me")); setorder(ret, permno, Month)
    ret[, me_lag := shift(me), by=permno]
    hm <- fq[rep(seq_len(.N), each=3L)]; hm[, kk := rep(0:2, times=nrow(fq))]
    hm[, per := form_q %m+% months(3L + kk)]; hm[, kk := NULL]
    ret[, per := Month]
    p0 <- merge(hm, ret[, c("permno","per","R","me","me_lag"), with=FALSE], by=c("permno","per"))
    p0 <- p0[is.finite(R)]
    p0[, rw := if (RAW) R else pmin(pmax(R, quantile(R,.005,na.rm=TRUE)), quantile(R,.995,na.rm=TRUE)), by=per]   # winsor at SECURITY level (= 05j)
    p <- to_gvkey(p0, "per", wcol = "me_lag"); lag <- 6L   # weights = PRIOR-month ME (R is the same-month return)
    ff <- ff5[, .(per=floor_date(as.Date(Date),"month"), MktRF,SMB,HML,RMW,CMA,RF)]
  } else {
    retq <- as.data.table(readRDS(file.path(ANA,"crsp_returns_quarterly.rds"))); retq[, permno:=as.integer(permno)]
    setnames(retq, c("RetQ","MCap_QEnd"), c("R","me")); setorder(retq, permno, Quarter)
    retq[, me_lag := shift(me), by=permno]
    # predictive: exposure at quarter q -> NEXT quarter return, at PERMNO level (= 05i), then collapse
    retq[, Rlead := shift(R, 1L, type="lead"), by=permno]
    retq[, qn := shift(Quarter,1L,type="lead"), by=permno]; retq[is.na(qn)|as.integer(qn-Quarter)>95L, Rlead := NA_real_]; retq[, qn := NULL]
    fqq <- copy(fq); setnames(fqq, "form_q", "per")
    retq[, per := Quarter]
    p0 <- merge(fqq, retq[, .(permno, per, R=Rlead, me, me_lag)], by=c("permno","per"))
    p0 <- p0[is.finite(R)]
    p0[, rw := if (RAW) R else pmin(pmax(R, quantile(R,.005,na.rm=TRUE)), quantile(R,.995,na.rm=TRUE)), by=per]
    p <- to_gvkey(p0, "per", wcol = "me"); lag <- 4L   # weights = formation-quarter-end ME (R is the LEAD return -> predetermined)
    ffm <- ff5[, .(Q=floor_date(as.Date(Date),"quarter"), MktRF,SMB,HML,RMW,CMA,RF)]
    ff <- ffm[, lapply(.SD, function(x) prod(1+x)-1), .SDcols=c("MktRF","SMB","HML","RMW","CMA","RF"), by=Q]; setnames(ff,"Q","per")
    ff[, per := per %m-% months(3)]   # audit fix: R is the LEAD return -> pair with t+1 factors
  }
  p[, Ret_w := Ret]                                  # already winsorised at the security level
  p[, size := log(pmax(MCap_w,1))]                   # PRE-RETURN firm cap (audit fix; was same-period MCap)
  say("[%s] firm-periods: %s | gvkeys: %s | %d periods (%s..%s) | multi-permno gvkeys: %d",
      freq, format(nrow(p),big.mark=","), format(uniqueN(p$gvkey),big.mark=","), uniqueN(p$per),
      as.character(min(p$per)), as.character(max(p$per)), n_multi)
  span <- function(ls){d<-merge(ls,ff,by="per"); d<-d[is.finite(LS)&is.finite(MktRF)]; m<-lm(LS~MktRF+SMB+HML+RMW+CMA,d)
    se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=lag,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]; a<-coef(m)[1]; c(alpha=unname(a),t=unname(a/se))}
  q2 <- lapply(MEAS, function(meas){ if(!meas%in%names(p)) return(NULL); p[, ms:=csz(get(meas)),by=per]; p[, sz:=csz(size),by=per]
    lam<-p[,{d<-.SD[complete.cases(.SD)]; if(nrow(d)<30) NA_real_ else coef(lm(Ret_w~ms+sz,d))["ms"]}, by=per, .SDcols=c("Ret_w","ms","sz")]$V1
    s<-nw_t(lam,lag); list(measure=meas, fm_lambda=unname(s["mean"]), fm_t=unname(s["t"])) })
  q2 <- Filter(Negate(is.null), q2)
  q3 <- lapply(MEAS, function(meas){ if(!meas%in%names(p)) return(NULL); d<-p[is.finite(get(meas))]
    d[, b:=as.integer(ceiling(frank(get(meas),ties.method=TIEM)/.N*5)),by=per]
    ew<-d[,.(Ret=mean(Ret_w,na.rm=TRUE)),by=.(per,b)]; vw<-d[is.finite(MCap_w)&MCap_w>0,.(Ret=weighted.mean(Ret_w,MCap_w,na.rm=TRUE)),by=.(per,b)]
    L<-function(x){w<-dcast(x,per~b,value.var="Ret"); w[,LS:=get("5")-get("1")]; w[,.(per,LS)]}
    ea<-span(L(ew)); va<-span(L(vw)); list(measure=meas, ew_alpha=unname(ea["alpha"]),ew_t=unname(ea["t"]),vw_alpha=unname(va["alpha"]),vw_t=unname(va["t"])) })
  q3 <- Filter(Negate(is.null), q3)
  u <- if(freq=="monthly") "%/m" else "%/q"
  say("  -- Q2 Fama-MacBeth (+size) --")
  for(r in q2) say("    %-20s lambda=%.4f%s t=%.2f", r$measure, 100*r$fm_lambda, u, r$fm_t)
  say("  -- Q3 quintile L/S FF5 alpha (EW | VW) --")
  for(r in q3) say("    %-20s EW %.3f%s t=%.2f | VW %.3f%s t=%.2f", r$measure, 100*r$ew_alpha,u,r$ew_t,100*r$vw_alpha,u,r$vw_t)
  list(q2=q2, q3=q3, n_obs=nrow(p), n_gvkey=uniqueN(p$gvkey), n_periods=uniqueN(p$per))
}

say("\n=== CRSP aggregated to GVKEY (firm) — share classes combined VW, each firm once ===")
out <- list(monthly = run_freq("monthly"), quarterly = run_freq("quarterly"), n_multi_permno_gvkeys = n_multi)
write_json(out, vS(file.path(ANA,"crsp_gvkey.json")), pretty=TRUE, auto_unbox=TRUE)
say("\n  wrote out/analysis/crsp_gvkey.json")
