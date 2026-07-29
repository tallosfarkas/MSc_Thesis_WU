# ==============================================================================
# pipeline/R/05j_crsp_monthly.R   (Stage 5 — US/CRSP robustness at MONTHLY freq)
#
# Monthly version of the US layer: CRSP monthly returns (05c) x the CRSP-mapped
# exposure panel, exposure held over the 3 following months. Q2 Fama-MacBeth + Q3
# quintile L/S + FF5 spanning, across measures. More power than the quarterly CRSP.
#
#   Rscript pipeline/R/05j_crsp_monthly.R   ->  out/analysis/crsp_monthly.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.find_root <- function() { d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") { if (file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d <- dirname(d) }
  stop("no root") }
ROOT <- .find_root(); ANA <- file.path(ROOT,"out","analysis"); say <- function(...) cat(sprintf(...),"\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
exp_dirname <- function() { .e <- Sys.getenv("GEO_EXPO"); if (nzchar(Sys.getenv("GEOV2"))||identical(.e,"v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_",.e) else "exposure" }  # self-contained (no utils.R source here)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment","GeoExposure_pr","GeoExposureTFIDF_pr")
NW <- 6L; csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

fq <- as.data.table(readRDS(file.path(ROOT, "out", exp_dirname(), "exposure_firmquarter_crsp.rds")))
fq[, permno := as.integer(permno)]; fq[, form_q := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
hm <- fq[rep(seq_len(.N), each=3L)]; hm[, k := rep(0:2, times=nrow(fq))]
hm[, Month := form_q %m+% months(3L + k)]; hm[, k := NULL]
ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[, permno := as.integer(permno)]
# PRIOR-month-end market cap (audit fix 2026-06-12): same-month-end ME embeds the month's
# return -> look-ahead in VW weights and endogeneity in the size control. Self-join so a
# gap month yields NA instead of a stale cap.
mel <- ret[is.finite(MCap_MEnd), .(permno, Month = Month %m+% months(1), MCap_lag = MCap_MEnd)]
p <- merge(hm, ret[, .(permno, Month, RetM, MCap_MEnd)], by=c("permno","Month"))
p <- merge(p, mel, by=c("permno","Month"), all.x=TRUE)
p <- p[is.finite(RetM)]
p[, RetM_w := pmin(pmax(RetM, quantile(RetM,.005,na.rm=TRUE)), quantile(RetM,.995,na.rm=TRUE)), by=Month]
p[, size := log(pmax(MCap_lag,1))]
say("[crsp-monthly] %s firm-months | %s permno | %d months (%s..%s)", format(nrow(p),big.mark=","),
    format(uniqueN(p$permno),big.mark=","), uniqueN(p$Month), as.character(min(p$Month)), as.character(max(p$Month)))

ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); ff5[, Month := floor_date(as.Date(Date),"month")]
fcols <- intersect(names(ff5), c("MktRF","SMB","HML","RMW","CMA","RF")); ff5 <- ff5[, c("Month",fcols), with=FALSE]
nw_t <- function(v){v<-v[is.finite(v)]; m<-lm(v~1); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]; c(mean=mean(v),t=mean(v)/se)}
span <- function(ls){d<-merge(ls,ff5,by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]; m<-lm(LS~MktRF+SMB+HML+RMW+CMA,d)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]; a<-coef(m)[1]; c(alpha=unname(a),t=unname(a/se))}

q2 <- lapply(MEAS, function(meas){ if(!meas%in%names(p)) return(NULL); p[, ms:=csz(get(meas)),by=Month]; p[, sz:=csz(size),by=Month]
  lam<-p[,{d<-.SD[complete.cases(.SD)]; if(nrow(d)<30) NA_real_ else coef(lm(RetM_w~ms+sz,d))["ms"]}, by=Month, .SDcols=c("RetM_w","ms","sz")]$V1
  s<-nw_t(lam); list(measure=meas, fm_lambda=unname(s["mean"]), fm_t=unname(s["t"])) })
q2 <- Filter(Negate(is.null), q2)
q3 <- lapply(MEAS, function(meas){ if(!meas%in%names(p)) return(NULL); d<-p[is.finite(get(meas))]
  d[, b:=as.integer(ceiling(frank(get(meas),ties.method=TIEM)/.N*5)),by=Month]
  ew<-d[,.(Ret=mean(get(RP),na.rm=TRUE)),by=.(Month,b)]; vw<-d[is.finite(MCap_lag)&MCap_lag>0,.(Ret=weighted.mean(get(RP),MCap_lag,na.rm=TRUE)),by=.(Month,b)]
  L<-function(x){w<-dcast(x,Month~b,value.var="Ret"); w[,LS:=get("5")-get("1")]; w[,.(Month,LS)]}
  ea<-span(L(ew)); va<-span(L(vw)); list(measure=meas, ew_alpha=unname(ea["alpha"]),ew_t=unname(ea["t"]),vw_alpha=unname(va["alpha"]),vw_t=unname(va["t"])) })
q3 <- Filter(Negate(is.null), q3)

say("\n=== CRSP MONTHLY Q2 Fama-MacBeth (+size) ===")
for(r in q2) say("  %-20s lambda=%.4f%%/m t=%.2f", r$measure, 100*r$fm_lambda, r$fm_t)
say("\n=== CRSP MONTHLY Q3 quintile L/S FF5 alpha === EW | VW")
for(r in q3) say("  %-20s EW %.3f%%/m t=%.2f | VW %.3f%%/m t=%.2f", r$measure, 100*r$ew_alpha,r$ew_t,100*r$vw_alpha,r$vw_t)
write_json(list(q2=q2,q3=q3,n_obs=nrow(p)), vS(file.path(ANA,"crsp_monthly.json")), pretty=TRUE, auto_unbox=TRUE)
say("\n  wrote out/analysis/crsp_monthly.json")
