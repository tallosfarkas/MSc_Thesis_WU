# ==============================================================================
# pipeline/R/05p_hp_controls.R   (Stage 5 — Hoberg-Phillips ADVANCED CONTROLS)
#
# Closes the proposal's promise to control for the three named Hoberg-Phillips
# product-market variables (all firm-year, gvkey-keyed, US, 1989-2023):
#   - TNIC3 HHI      (tnic3hhi)    : product-market concentration (less competition)
#   - TNIC3 Total Sim(tnic3tsimm) : crowdedness / product differentiation
#   - Product Mkt Fluidity (prodmktfluid) : competitive threat / product-space change
#   - Vertical Integration (vertinteg)    : supply-chain relatedness (Fresard-Hoberg-Phillips)
# These attach to the US/CRSP layer (which carries gvkey). We re-run the Q2
# Fama-MacBeth pricing test ADDING the HP controls on top of size, to check the
# geoeconomic premium (GeoSentiment pricing, GeoRisk) is not subsumed by product-market
# structure. Reports lambda_Geo baseline (+size) vs (+size +HP), both frequencies, and
# the HP controls' own average lambdas.
#
#   Rscript pipeline/R/05p_hp_controls.R   ->  out/analysis/hp_controls.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate); library(fixest) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); HP <- file.path(ROOT,"data/inputs/hp_raw")
say <- function(...) cat(sprintf(...),"\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
HPV  <- c("hhi","tsim","fluid","vint")                       # HP control short names
csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

# ---- HP controls, gvkey-year (integer gvkey) -------------------------------
hhi  <- fread(file.path(HP,"HHI/TNIC3HHIdata.txt"))[, .(gvkey=as.integer(gvkey), year=as.integer(year), hhi=tnic3hhi, tsim=tnic3tsimm)]
flu  <- fread(file.path(HP,"Fluidity/FluidityData.txt"))[, .(gvkey=as.integer(gvkey), year=as.integer(year), fluid=prodmktfluid)]
vint <- fread(file.path(HP,"VertInteg/VertInteg.txt"))[, .(gvkey=as.integer(gvkey), year=as.integer(year), vint=vertinteg)]
hp <- Reduce(function(a,b) merge(a,b,by=c("gvkey","year"),all=TRUE), list(hhi,flu,vint))
say("[hp] controls merged: %s gvkey-years (%d-%d) | hhi %.0f%% tsim %.0f%% fluid %.0f%% vint %.0f%%",
    format(nrow(hp),big.mark=","), min(hp$year), max(hp$year),
    100*mean(!is.na(hp$hhi)), 100*mean(!is.na(hp$tsim)), 100*mean(!is.na(hp$fluid)), 100*mean(!is.na(hp$vint)))

# ---- CRSP exposure panel + returns -----------------------------------------
fq <- as.data.table(readRDS(file.path(ROOT, if (EXV2) "out/exposure_v2" else "out/exposure", "exposure_firmquarter_crsp.rds")))
fq[, gvkey := as.integer(gvkey)]; fq <- fq[!is.na(gvkey)]
if (V2) hp[, year := year + 1L]   # V2: HP 10-K-derived vars usable from y+1 (publication lag)
fq <- merge(fq, hp, by=c("gvkey","year"), all.x=TRUE)
fq[, form_q := as.Date(ISOdate(year, (quarter-1L)*3L+1L, 1L))]

ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv"))
nw_t <- function(v,lag){v<-v[is.finite(v)]; if(length(v)<5) return(c(mean=NA,t=NA)); m<-lm(v~1)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=lag,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]; c(mean=mean(v),t=mean(v)/se)}

# Fama-MacBeth lambda on the geo measure, with a given control set
fm_lambda <- function(p, meas, ctrls, pcol, lag){
  p <- copy(p); p[, ms := csz(get(meas)), by=pcol]
  for (cc in ctrls) p[, (paste0("z_",cc)) := csz(get(cc)), by=pcol]
  zc <- paste0("z_",ctrls); fcols <- c("Ret_w","ms",zc)
  per_t <- p[, { d<-.SD[complete.cases(.SD)]; if(nrow(d)<30) NA_real_ else
                 coef(lm(reformulate(c("ms",zc),"Ret_w"), d))["ms"] }, by=pcol, .SDcols=fcols]$V1
  s <- nw_t(per_t, lag); list(lambda=unname(s["mean"]), t=unname(s["t"]),
                              n_per=sum(is.finite(per_t)))
}
# average lambda for the HP controls themselves (single multivariate FM, size+all HP)
fm_hp_own <- function(p, pcol, lag){
  p<-copy(p); for(cc in HPV) p[, (paste0("z_",cc)):=csz(get(cc)), by=pcol]; p[, sz:=csz(size), by=pcol]
  zc<-paste0("z_",HPV); fcols<-c("Ret_w","sz",zc)
  per <- p[, { d<-.SD[complete.cases(.SD)]; if(nrow(d)<30) as.list(rep(NA_real_,length(zc))) else
               as.list(coef(lm(reformulate(c("sz",zc),"Ret_w"), d))[zc]) }, by=pcol, .SDcols=fcols]
  setnames(per, c(pcol, zc)); out<-list()
  for(cc in zc){ s<-nw_t(per[[cc]], lag); out[[sub("z_","",cc)]]<-list(lambda=unname(s["mean"]),t=unname(s["t"])) }
  out
}

run_freq <- function(freq){
  if (freq=="monthly"){
    ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[, permno:=as.integer(permno)]
    fq[, permno:=as.integer(permno)]
    hm <- fq[rep(seq_len(.N), each=3L)]; hm[, kk:=rep(0:2,times=nrow(fq))]; hm[, per := form_q %m+% months(3L+kk)]; hm[, kk:=NULL]
    p <- merge(hm, ret[,.(permno, per=Month, RetM, MCap_MEnd)], by=c("permno","per")); p<-p[is.finite(RetM)]
    p[, Ret_w := pmin(pmax(RetM,quantile(RetM,.005,na.rm=TRUE)),quantile(RetM,.995,na.rm=TRUE)), by=per]; lag<-6L
  } else {
    retq <- as.data.table(readRDS(file.path(ANA,"crsp_returns_quarterly.rds"))); retq[, permno:=as.integer(permno)]
    fq[, permno:=as.integer(permno)]; setorder(retq, permno, Quarter)
    retq[, Rlead := shift(RetQ,1L,type="lead"), by=permno]
    retq[, qn := shift(Quarter,1L,type="lead"), by=permno]; retq[is.na(qn)|as.integer(qn-Quarter)>95L, Rlead:=NA_real_]; retq[, qn:=NULL]
    p <- merge(fq, retq[,.(permno, per=Quarter, Ret=Rlead, MCap_QEnd)], by.x=c("permno","form_q"), by.y=c("permno","per")); p<-p[is.finite(Ret)]
    setnames(p, "form_q", "per"); p[, Ret_w := pmin(pmax(Ret,quantile(Ret,.005,na.rm=TRUE)),quantile(Ret,.995,na.rm=TRUE)), by=per]; lag<-4L
    p[, MCap_MEnd := MCap_QEnd]
  }
  p[, size := log(pmax(MCap_MEnd,1))]
  pc <- p[complete.cases(p[, c(MEAS, HPV), with=FALSE])]                  # firms with HP coverage
  say("[%s] panel %s rows | HP-covered %s rows | %s gvkeys | %d periods",
      freq, format(nrow(p),big.mark=","), format(nrow(pc),big.mark=","), format(uniqueN(pc$gvkey),big.mark=","), uniqueN(pc$per))
  u <- if(freq=="monthly") "%/m" else "%/q"
  say("  measure              lambda(+size)        lambda(+size+HP)   [%s, NW-t]", u)
  q2 <- lapply(MEAS, function(m){
    base <- fm_lambda(pc, m, "size", "per", lag)                          # +size only (on HP-covered sample)
    full <- fm_lambda(pc, m, c("size",HPV), "per", lag)                   # +size +HP
    say("  %-18s   %7.4f (t=%5.2f)   %7.4f (t=%5.2f)", m,
        100*base$lambda, base$t, 100*full$lambda, full$t)
    list(measure=m, base_lambda=base$lambda, base_t=base$t, hp_lambda=full$lambda, hp_t=full$t, n_per=full$n_per) })
  own <- fm_hp_own(pc, "per", lag)
  say("  -- HP controls' own avg lambda (FM, +size) --")
  for(cc in names(own)) say("     %-6s lambda=%7.4f%s t=%5.2f", cc, 100*own[[cc]]$lambda, u, own[[cc]]$t)

  # ---- panel FE (the proposal's headline eq): Ret_lead ~ geo (+HP) | firm + period
  pc[, z_size := csz(size), by=per]; for(cc in HPV) pc[, (paste0("z_",cc)):=csz(get(cc)), by=per]
  say("  -- panel FE (firm+period, cluster firm): coef on geo, base(+size) vs +HP --")
  fe <- lapply(MEAS, function(m){ pc[, ms := csz(get(m)), by=per]
    b <- tryCatch(feols(Ret_w ~ ms + z_size | gvkey + per, pc, vcov=~gvkey), error=function(e)NULL)
    f <- tryCatch(feols(Ret_w ~ ms + z_size + z_hhi + z_tsim + z_fluid + z_vint | gvkey + per, pc, vcov=~gvkey), error=function(e)NULL)
    g <- function(mm) if(is.null(mm)) c(NA,NA) else c(coef(mm)["ms"], coef(mm)["ms"]/se(mm)["ms"])
    bb<-g(b); ff<-g(f)
    say("  %-18s   %7.4f (t=%5.2f)   %7.4f (t=%5.2f)", m, 100*bb[1], bb[2], 100*ff[1], ff[2])
    list(measure=m, base_coef=unname(bb[1]), base_t=unname(bb[2]), hp_coef=unname(ff[1]), hp_t=unname(ff[2])) })

  list(q2=q2, panel_fe=fe, hp_own=own, n_obs=nrow(pc), n_gvkey=uniqueN(pc$gvkey), n_periods=uniqueN(pc$per),
       year_range=c(min(pc$year),max(pc$year)))
}

say("\n=== Q2 Fama-MacBeth with Hoberg-Phillips controls (US/CRSP, gvkey-year) ===")
out <- list(monthly=run_freq("monthly"), quarterly=run_freq("quarterly"),
            hp_coverage=list(years=c(min(hp$year),max(hp$year)), n_gvkey_years=nrow(hp)))
write_json(out, vS(file.path(ANA,"hp_controls.json")), pretty=TRUE, auto_unbox=TRUE, na="null")
say("\n  wrote out/analysis/hp_controls.json")
