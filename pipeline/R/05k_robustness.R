# ==============================================================================
# pipeline/R/05k_robustness.R   (Stage 5 — Q3 robustness battery, monthly)
#
# Stress-tests the long-short on the monthly RIC panel: decile sort, SIZE-NEUTRAL
# double sort (controls the small-cap tilt), pre/post-2018 split, and INDUSTRY-
# ADJUSTED exposure demeaned within (a) 2-digit SIC and (b) Hoberg-Phillips FIC300
# text-based industries. All EW quintile L/S, FF5 alpha, NW(6). Focus on the
# measures that carried signal (GeoRisk trading, GeoSentiment pricing) + GeoExposure ref.
#
#   Rscript pipeline/R/05k_robustness.R   ->  out/analysis/robustness_q3.json
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
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment"); NW <- 6L

p <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric_monthly.rds"))))[is.finite(RetM_w)]
# FIC300 (Hoberg-Phillips text-based industry) from the HP controls panel
hp <- as.data.table(readRDS(file.path(ROOT,"data/processed/CONTROLS_PANEL_HP.rds")))
p <- merge(p, hp[, .(Ticker, form_q = Quarter, FIC300)], by = c("Ticker","form_q"), all.x = TRUE)
p[, sic2 := floor(as.integer(siccd)/100)]
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); ff5[, Month := floor_date(as.Date(Date),"month")]
fcols <- intersect(names(ff5), c("MktRF","SMB","HML","RMW","CMA","RF")); ff5 <- ff5[, c("Month",fcols), with=FALSE]
say("[robust] %s firm-months | FIC300 cov %.0f%% | sic2 cov %.0f%%", format(nrow(p),big.mark=","),
    100*mean(!is.na(p$FIC300)), 100*mean(!is.na(p$sic2)))

alpha_of <- function(ls){ d<-merge(ls,ff5,by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]; if(nrow(d)<24) return(c(a=NA,t=NA,n=nrow(d)))
  m<-lm(LS~MktRF+SMB+HML+RMW+CMA,d); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
  c(a=unname(coef(m)[1]), t=unname(coef(m)[1]/se), n=nrow(d)) }
qbin <- function(x,n) as.integer(ceiling(frank(x,ties.method=TIEM)/length(x)*n))

ls_simple <- function(d, meas, nbin){ d<-d[is.finite(get(meas))]; d[, b:=qbin(get(meas),nbin), by=Month]
  r<-d[, .(Ret=mean(RetM_w,na.rm=TRUE)), by=.(Month,b)]; w<-dcast(r,Month~b,value.var="Ret"); w[,LS:=get(as.character(nbin))-get("1")]; w[,.(Month,LS)] }
ls_sizeneutral <- function(d, meas){ d<-d[is.finite(get(meas))&is.finite(MCap_Q)]
  d[, sq:=qbin(MCap_Q,5), by=Month]; d[, eb:=qbin(get(meas),5), by=.(Month,sq)]
  r<-d[, .(Ret=mean(RetM_w,na.rm=TRUE)), by=.(Month,sq,eb)]
  ls<-r[eb %in% c(1L,5L)][, .(LS=Ret[eb==5L]-Ret[eb==1L]), by=.(Month,sq)][, .(LS=mean(LS,na.rm=TRUE)), by=Month]; ls }
ls_indadj <- function(d, meas, indcol){ d<-d[is.finite(get(meas))&!is.na(get(indcol))]
  d[, adj := get(meas) - mean(get(meas),na.rm=TRUE), by=c("Month",indcol)]      # demean within industry x month
  ls_simple(d, "adj", 5L) }

res <- list()
for (meas in MEAS) {
  base <- alpha_of(ls_simple(p, meas, 5L)); dec <- alpha_of(ls_simple(p, meas, 10L))
  szn  <- alpha_of(ls_sizeneutral(p, meas))
  pre  <- alpha_of(ls_simple(p[Month <  as.Date("2018-01-01")], meas, 5L))
  post <- alpha_of(ls_simple(p[Month >= as.Date("2018-01-01")], meas, 5L))
  sicadj <- alpha_of(ls_indadj(p, meas, "sic2")); ficadj <- alpha_of(ls_indadj(p, meas, "FIC300"))
  res[[meas]] <- list(quintile=base, decile=dec, size_neutral=szn, pre2018=pre, post2018=post,
                      sic2_adj=sicadj, fic300_adj=ficadj)
  say("\n  %s (EW quintile L/S FF5 alpha %%/m, t):", meas)
  for (k in names(res[[meas]])) { v<-res[[meas]][[k]]; say("    %-14s %.3f%%  t=%.2f  (n=%d)", k, 100*v["a"], v["t"], v["n"]) }
}
write_json(res, vS(file.path(ANA,"robustness_q3.json")), pretty=TRUE, auto_unbox=TRUE)
say("\n  wrote out/analysis/robustness_q3.json")
