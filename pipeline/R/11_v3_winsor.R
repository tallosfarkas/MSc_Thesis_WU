# ==============================================================================
# pipeline/R/11_v3_winsor.R   (V3 = V2 corrections, but WINSORISED portfolio returns)
#
# V3 keeps every V2 fix (exact tokenizer, Hassan risk words, lagged ME, seeded
# ties, two-way clustering, factor alignment) EXCEPT it reverts the one change that
# drove most of the headline t drop: it builds the long-short from 0.5/99.5
# within-month-winsorised constituent returns (standard outlier control), not raw.
# Since winsorisation only affects PORTFOLIO construction, RQ1/RQ2 are identical to
# V2 -- this script recomputes only RQ3 (GeoRisk L/S), seed-averaged over the random
# tie-break, for CRSP monthly EW (headline), LSEG monthly EW, and walk-forward OOS.
# Reads the V2 measure (out/exposure_v2) + V2 panels.
#
#   Rscript pipeline/R/11_v3_winsor.R   -> out/analysis/v3_winsor.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly=TRUE); NW <- 6L; WARM <- 36L; NSEED <- 25L
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT,"out","exposure_v2"); say <- function(...) cat(sprintf(...),"\n")
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); ff5[,Month:=floor_date(as.Date(Date),"month")]
FC <- c("MktRF","SMB","HML","RMW","CMA"); ff5 <- ff5[,c("Month",FC),with=FALSE]
ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[,permno:=as.integer(permno)]

crsp <- {
  fq<-as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); fq[,permno:=as.integer(permno)]
  fq[,form_q:=as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
  hm<-fq[rep(seq_len(.N),each=3L)]; hm[,k:=rep(0:2,times=nrow(fq))]; hm[,Month:=form_q %m+% months(3L+k)]; hm[,k:=NULL]
  p<-merge(hm,ret[,.(permno,Month,RetM)],by=c("permno","Month")); p<-p[is.finite(RetM)]
  p[,RetM_w:=pmin(pmax(RetM,quantile(RetM,.005,na.rm=TRUE)),quantile(RetM,.995,na.rm=TRUE)),by=Month]; p }
lseg <- as.data.table(readRDS(file.path(ANA,"panel_ric_monthly_v2.rds"))); lseg[,Month:=as.Date(Month)]

ls_w <- function(p, seed){ d<-copy(p[is.finite(GeoRisk)]); set.seed(seed)
  d[,b:=as.integer(ceiling(frank(GeoRisk,ties.method="random")/.N*5)),by=Month]
  ew<-d[,.(R=mean(RetM_w,na.rm=TRUE)),by=.(Month,b)]; w<-dcast(ew,Month~b,value.var="R")
  w[,LS:=get("5")-get("1")]; w[is.finite(LS),.(Month,LS)][order(Month)] }
fa <- function(ls){ m<-merge(ls,ff5,by="Month"); m<-m[is.finite(LS)&is.finite(MktRF)]; fit<-lm(LS~MktRF+SMB+HML+RMW+CMA,m)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(fit,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(fit)))[1]
  c(alpha=unname(coef(fit)[1]), t=unname(coef(fit)[1]/se)) }
oos <- function(ls){ d<-merge(ls,ff5,by="Month")[order(Month)]; d<-d[is.finite(LS)&is.finite(MktRF)]; n<-nrow(d)
  ar<-rep(NA_real_,n); for(t in (WARM+1):n){ tr<-d[1:(t-1)]; b<-coef(lm(LS~MktRF+SMB+HML+RMW+CMA,tr))[-1]
    ar[t]<-d$LS[t]-sum(b*as.numeric(d[t,..FC])) }
  o<-ar[is.finite(ar)]; mo<-lm(o~1); se<-if(have_nw) sqrt(sandwich::NeweyWest(mo,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(mo)$coef[1,2]
  c(alpha=mean(o), t=mean(o)/se) }

avg <- function(p, lab){
  fs<-sapply(1:NSEED, function(s) fa(ls_w(p,s))); os<-sapply(1:NSEED, function(s) oos(ls_w(p,s)))
  r<-list(full_alpha=mean(fs["alpha",]), full_t=mean(fs["t",]), full_t_sd=sd(fs["t",]),
          oos_alpha=mean(os["alpha",]), oos_t=mean(os["t",]), oos_t_sd=sd(os["t",]))
  say("  [%s] WINSOR full alpha %.3f%%/m t=%.2f (sd %.2f) | walk-fwd OOS alpha %.3f%%/m t=%.2f (sd %.2f)",
      lab,100*r$full_alpha,r$full_t,r$full_t_sd,100*r$oos_alpha,r$oos_t,r$oos_t_sd); r }

say("=== V3 (V2 corrections + WINSORISED returns) — GeoRisk L/S, seed-averaged over %d seeds ===", NSEED)
res <- list(crsp = avg(crsp,"CRSP monthly EW"), lseg = avg(lseg,"LSEG monthly EW"))
write_json(list(variant="v3", note="V2 corrections but 0.5/99.5 within-month winsorised constituent returns (standard outlier control); seed-averaged over the random tie-break. RQ1/RQ2 identical to V2.",
                results=res), file.path(ANA,"v3_winsor.json"), pretty=TRUE, auto_unbox=TRUE, digits=6)
say("  wrote out/analysis/v3_winsor.json")
