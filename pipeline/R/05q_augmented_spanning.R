# ==============================================================================
# pipeline/R/05q_augmented_spanning.R   (Stage 5 — AUGMENTED spanning of the L/S)
#
# Does FF5 (plus momentum and the geoeconomic/uncertainty indices) SPAN the GeoRisk
# long-short? Rebuilds the headline GeoRisk EW quintile L/S monthly series (CRSP, the
# strongest cell) and the LSEG-global one, then regresses each on:
#   M0  FF5                                  (the baseline spanning test = Q3 alpha)
#   M1  FF5 + UMD (momentum)
#   M2  FF5 + dGPR + dEPU                    (geopolitical-risk + policy-uncertainty shocks)
#   M3  FF5 + UMD + dGPR + dEPU              (the augmented model)
# plus univariate macro loadings (sign of GPR/GPRT/EPU on the L/S). Macro indices enter
# as monthly log-changes (innovations). Newey-West(6) SE. If the intercept (alpha)
# stays significant, none of these factors explain the geoeconomic premium.
# (VIX/oil/USD were in the legacy 8-factor set but FRED/Stooq are unreachable here;
# GPR + EPU are the geoeconomically relevant ones and are included.)
#
#   Rscript pipeline/R/05q_augmented_spanning.R   ->  out/analysis/augmented_spanning.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate); library(readxl) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); MAC <- file.path(ROOT,"data/inputs/macro")
say <- function(...) cat(sprintf(...),"\n"); NW <- 6L
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
exp_dirname <- function() { .e <- Sys.getenv("GEO_EXPO"); if (nzchar(Sys.getenv("GEOV2"))||identical(.e,"v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_",.e) else "exposure" }  # self-contained
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"


# ---- factors: FF5 + UMD + GPR + EPU (monthly), macro as log-changes ----------
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv"))[, .(Month=floor_date(as.Date(Date),"month"), MktRF,SMB,HML,RMW,CMA,RF)]
umd <- fread(file.path(MAC,"F-F_Momentum_Factor.csv"), skip=13, header=FALSE, col.names=c("ym","Mom"))
umd <- umd[grepl("^[0-9]{6}$", trimws(ym))]; umd[, Month := floor_date(as.Date(paste0(ym,"01"),"%Y%m%d"),"month")]; umd[, UMD := as.numeric(Mom)/100]
gpr <- as.data.table(read_excel(file.path(ROOT,"data/inputs/gpr_monthly.xls"), guess_max = 100000))
gpr[, Month := floor_date(as.Date(month),"month")]; gpr <- gpr[, .(Month, GPR=as.numeric(GPR), GPRT=as.numeric(GPRT))]
epu <- fread(file.path(MAC,"US_EPU.csv")); epu <- epu[Year>=1985 & Year<=2026]
epu[, Month := floor_date(as.Date(sprintf("%d-%02d-01", Year, Month)),"month")]; epu[, EPU := as.numeric(News_Based_Policy_Uncert_Index)]
# Yahoo (yfinance) market factors: VIX level, WTI oil, USD index -> dVIX, oil ret, usd ret
ymac <- function(f, col){ p<-file.path(MAC,f); if(!file.exists(p)) return(NULL)
  d<-fread(p); d[, Month:=floor_date(as.Date(Date),"month")]; setnames(d,"Close",col); d[,.(Month,get(col))][, setNames(.SD,c("Month",col))] }
vix <- tryCatch({d<-fread(file.path(MAC,"VIXCLS.csv")); d[,.(Month=floor_date(as.Date(Date),"month"), VIX=as.numeric(Close))]}, error=function(e)NULL)
oil <- tryCatch({d<-fread(file.path(MAC,"DCOILWTICO.csv")); d[,.(Month=floor_date(as.Date(Date),"month"), OIL=as.numeric(Close))]}, error=function(e)NULL)
usd <- tryCatch({d<-fread(file.path(MAC,"DTWEXBGS.csv")); d[,.(Month=floor_date(as.Date(Date),"month"), USD=as.numeric(Close))]}, error=function(e)NULL)
F <- Reduce(function(a,b) merge(a,b,by="Month",all.x=TRUE),
            Filter(Negate(is.null), list(ff5, umd[,.(Month,UMD)], gpr[,.(Month,GPR,GPRT)], epu[,.(Month,EPU)], vix, oil, usd)))
setorder(F, Month)
for (v in c("GPR","GPRT","EPU")) F[, (paste0("d",v)) := c(NA, diff(log(get(v))))]   # innovation = monthly log-change
if("VIX" %in% names(F)) F[, dVIX := c(NA, diff(VIX))]                                  # change in the VIX level
if("OIL" %in% names(F)) F[, oilret := c(NA, diff(log(OIL)))]                           # WTI monthly return
if("USD" %in% names(F)) F[, usdret := c(NA, diff(log(USD)))]                           # broad USD monthly return
MACX <- intersect(c("dGPR","dEPU","dVIX","oilret","usdret","UMD"), names(F))           # augmented add-factors
say("[factors] %d months %s..%s | UMD cov %.0f%% GPR %.0f%% EPU %.0f%%", nrow(F), as.character(min(F$Month)), as.character(max(F$Month)),
    100*mean(!is.na(F$UMD)), 100*mean(!is.na(F$dGPR)), 100*mean(!is.na(F$dEPU)))

# ---- rebuild an EW quintile L/S monthly series (default GeoRisk) --------------
ls_series <- function(source, meas = "GeoRisk"){
  if (source=="crsp"){
    fq <- as.data.table(readRDS(file.path(ROOT, "out", exp_dirname(), "exposure_firmquarter_crsp.rds"))); fq[, permno:=as.integer(permno)]
    fq[, form_q := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
    hm <- fq[rep(seq_len(.N),each=3L)]; hm[, kk:=rep(0:2,times=nrow(fq))]; hm[, Month:=form_q %m+% months(3L+kk)]
    ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[, permno:=as.integer(permno)]
    p <- merge(hm, ret[,.(permno,Month,R=RetM)], by=c("permno","Month"))
  } else {
    p <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric_monthly.rds"))))
    rc <- intersect(c("RetM_lead","RetM","Ret"), names(p))[1]; setnames(p, rc, "R"); p[, Month := as.Date(Month)]
  }
  p <- p[is.finite(R) & is.finite(get(meas))]
  if (!RAW) p[, R := pmin(pmax(R, quantile(R,.005,na.rm=TRUE)), quantile(R,.995,na.rm=TRUE)), by=Month]   # V2: raw returns
  p[, b := as.integer(ceiling(frank(get(meas),ties.method=TIEM)/.N*5)), by=Month]
  w <- dcast(p[, .(R=mean(R)), by=.(Month,b)], Month~b, value.var="R")
  w[, .(Month, LS = get("5") - get("1"))][is.finite(LS)]
}

spans <- function(LS, src){
  d <- merge(LS, F, by="Month"); d <- d[is.finite(LS) & is.finite(MktRF)]
  reg <- function(rhs){ m <- lm(reformulate(rhs,"LS"), d[complete.cases(d[, c("LS",rhs),with=FALSE])])
    se <- if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
    a <- coef(m)[1]; list(alpha=unname(a), t=unname(a/se), n=length(m$residuals)) }
  ff5v <- c("MktRF","SMB","HML","RMW","CMA")
  models <- list(M0_FF5=ff5v, M1_FF5_UMD=c(ff5v,"UMD"),
                 M2_FF5_GPR_EPU=c(ff5v,"dGPR","dEPU"),
                 M3_augmented=c(ff5v, intersect(MACX, names(d))))   # FF5 + momentum + GPR + EPU + dVIX + oil + USD
  res <- lapply(models, reg)
  # univariate macro loadings (beta + t) on the L/S
  uni <- function(f){ x <- d[[f]]; y <- d[["LS"]]; ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 12) return(list(beta=NA_real_, t=NA_real_))
    m <- lm(y[ok] ~ x[ok]); if (length(coef(m)) < 2 || is.na(coef(m)[2])) return(list(beta=NA_real_, t=NA_real_))
    V <- tryCatch(if(have_nw) sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE) else vcov(m), error=function(e) vcov(m))
    list(beta=unname(coef(m)[2]), t=unname(coef(m)[2]/sqrt(V[2,2]))) }
  lf <- intersect(c("dGPR","dGPRT","dEPU","dVIX","oilret","usdret","UMD"), names(d))
  loads <- setNames(lapply(lf, uni), lf)
  say("\n=== %s: EW L/S spanning (alpha %%/m, NW-t; %d months) ===", toupper(src), nrow(d))
  for (nm in names(res)) say("  %-16s alpha=%.4f%%/m (t=%.2f) ~ %.2f%%/yr", nm, 100*res[[nm]]$alpha, res[[nm]]$t, 100*((1+res[[nm]]$alpha)^12-1))
  say("  -- univariate macro loadings on the L/S (beta, t) --")
  for (nm in names(loads)) say("     %-6s beta=%.4f t=%.2f", nm, loads[[nm]]$beta, loads[[nm]]$t)
  list(models=res, loadings=loads, n_months=nrow(d))
}

F[, dGPRT := c(NA, diff(log(GPRT)))]
out <- list()
out[["crsp"]] <- spans(ls_series("crsp"), "crsp")
out[["lseg"]] <- tryCatch(spans(ls_series("lseg"), "lseg"), error=function(e){message("skip lseg: ",conditionMessage(e)); NULL})
# GeoSentiment spanning (CRSP) under an extra key, so the GeoRisk shape above stays stable
out[["GeoSentiment"]] <- list(crsp = spans(ls_series("crsp", "GeoSentiment"), "crsp GeoSentiment"))
write_json(out, vS(file.path(ANA,"augmented_spanning.json")), pretty=TRUE, auto_unbox=TRUE, na="null")
say("\n  wrote out/analysis/augmented_spanning.json")
