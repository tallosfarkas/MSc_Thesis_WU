# ==============================================================================
# pipeline/R/05i_crsp_analysis.R   (Stage 5 — US robustness on genuine CRSP returns)
#
# The US/CRSP layer: join the CRSP-mapped exposure panel (permno) to CRSP quarterly
# returns (05c) and re-run Q2 (Fama-MacBeth) + Q3 (quintile L/S + FF5 spanning) across
# all measures. Confirms the LSEG-based findings on an independent, clean US return
# source. Size control + value weights use CRSP market cap.
#
#   Rscript pipeline/R/05i_crsp_analysis.R
# Output: out/analysis/crsp_q2.json + crsp_q3.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.find_root <- function() { d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") { if (file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d <- dirname(d) }
  stop("no root") }
ROOT <- .find_root(); ANA <- file.path(ROOT, "out", "analysis"); say <- function(...) cat(sprintf(...), "\n")
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
NW <- 4L; csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

fq  <- as.data.table(readRDS(file.path(ROOT, if (EXV2) "out/exposure_v2" else "out/exposure", "exposure_firmquarter_crsp.rds")))
fq[, Quarter := as.Date(ISOdate(year, (quarter-1L)*3L+1L, 1L))]
ret <- as.data.table(readRDS(file.path(ANA, "crsp_returns_quarterly.rds")))
ret[, permno := as.integer(permno)]; fq[, permno := as.integer(permno)]
p <- merge(fq, ret[, .(permno, Quarter, RetQ, MCap_QEnd)], by = c("permno","Quarter"))
setorder(p, permno, Quarter)
p[, Ret_lead := shift(RetQ, 1L, type="lead"), by = permno]
p[, Qn := shift(Quarter, 1L, type="lead"), by = permno]; p[is.na(Qn) | as.integer(Qn-Quarter) > 95L, Ret_lead := NA_real_]; p[, Qn := NULL]
p[, size := log(pmax(MCap_QEnd, 1))]
p <- p[!is.na(Ret_lead)]
say("[crsp] %s firm-quarters | %s permno | %d quarters (%s..%s)", format(nrow(p),big.mark=","),
    format(uniqueN(p$permno),big.mark=","), uniqueN(p$Quarter), as.character(min(p$Quarter)), as.character(max(p$Quarter)))

ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); ff5[, Q := floor_date(as.Date(Date),"quarter")]
fcols <- intersect(names(ff5), c("MktRF","SMB","HML","RMW","CMA","RF"))
ff5q <- ff5[, lapply(.SD, function(x) prod(1+x)-1), .SDcols=fcols, by=Q]; setnames(ff5q,"Q","Quarter")
# Audit fix 2026-06-12: the L/S is keyed by the FORMATION quarter t but holds Ret_lead (t+1),
# so pair it with the t+1 factor realisations by shifting the factor key back one quarter.
ff5q[, Quarter := Quarter %m-% months(3)]
nw_t <- function(v){v<-v[is.finite(v)]; m<-lm(v~1); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]; c(mean=mean(v),t=mean(v)/se)}
span <- function(ls){d<-merge(ls,ff5q,by="Quarter"); d<-d[is.finite(LS)&is.finite(MktRF)]; m<-lm(LS~MktRF+SMB+HML+RMW+CMA,d)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]; a<-coef(m)[1]; c(alpha=unname(a),t=unname(a/se))}

q2 <- lapply(MEAS, function(meas){ if(!meas %in% names(p)) return(NULL); p[, ms:=csz(get(meas)), by=Quarter]; p[, sz:=csz(size), by=Quarter]
  lam <- p[, { d<-.SD[complete.cases(.SD)]; if(nrow(d)<30) NA_real_ else coef(lm(Ret_lead~ms+sz,d))["ms"] }, by=Quarter, .SDcols=c("Ret_lead","ms","sz")]$V1
  s<-nw_t(lam); list(measure=meas, fm_lambda=unname(s["mean"]), fm_t=unname(s["t"])) })
q2 <- Filter(Negate(is.null), q2)

q3 <- lapply(MEAS, function(meas){ if(!meas %in% names(p)) return(NULL); d<-p[is.finite(get(meas))]
  d[, b:=as.integer(ceiling(frank(get(meas),ties.method=TIEM)/.N*5)), by=Quarter]
  ew<-d[, .(Ret=mean(Ret_lead,na.rm=TRUE)), by=.(Quarter,b)]; vw<-d[MCap_QEnd>0,.(Ret=weighted.mean(Ret_lead,MCap_QEnd,na.rm=TRUE)),by=.(Quarter,b)]
  lsq<-function(x){w<-dcast(x,Quarter~b,value.var="Ret"); w[,LS:=get("5")-get("1")]; w[,.(Quarter,LS)]}
  ea<-span(lsq(ew)); va<-span(lsq(vw))
  list(measure=meas, ew_alpha=unname(ea["alpha"]), ew_t=unname(ea["t"]), vw_alpha=unname(va["alpha"]), vw_t=unname(va["t"])) })
q3 <- Filter(Negate(is.null), q3)

say("\n=== CRSP Q2 Fama-MacBeth (+size) ===")
for(r in q2) say("  %-20s lambda=%.4f%%/q t=%.2f", r$measure, 100*r$fm_lambda, r$fm_t)
say("\n=== CRSP Q3 quintile L/S FF5 alpha ===  EW | VW")
for(r in q3) say("  %-20s EW %.3f%%/q t=%.2f | VW %.3f%%/q t=%.2f", r$measure, 100*r$ew_alpha, r$ew_t, 100*r$vw_alpha, r$vw_t)
write_json(list(q2=q2,q3=q3, n_obs=nrow(p)), vS(file.path(ANA,"crsp_analysis.json")), pretty=TRUE, auto_unbox=TRUE)
say("\n  wrote out/analysis/crsp_analysis.json")
