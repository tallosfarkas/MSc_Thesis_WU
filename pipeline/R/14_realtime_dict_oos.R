# ==============================================================================
# pipeline/R/14_realtime_dict_oos.R   (Phase C of the expanding-window OOS analysis)
#
# Answers Randl's look-ahead question — "would a trader who only had pre-t text
# have discovered a materially different dictionary, and would the strategy still
# work?" — using the point-in-time vintage dictionaries (Phase A) and the
# real-time exposure (Phase B, exposure_rt_<year>.rds: each year scored with the
# dictionary discovered on text <= year-1).
#
#  (1) DICTIONARY DIVERGENCE: how much each vintage overlaps the full headline
#      dictionary (esp. its high-G2 terms) and the next vintage.
#  (2) REAL-TIME L/S, LSEG (ticker) and CRSP (permno via the ec_ccm crosswalk):
#      GeoRisk & GeoSentiment monthly Q5-Q1 FF5 alpha under the FIXED vs the
#      REAL-TIME dictionary, in-sample and walk-forward OOS (36-mo warmup).
#
#   GEO_FIX=1 GEO_TAG=_v11 GEO_TIES=first Rscript pipeline/R/14_realtime_dict_oos.R
# Output: out/analysis/realtime_dict_oos.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(lubridate); library(jsonlite) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
exp_dirname <- function() { .e <- Sys.getenv("GEO_EXPO"); if (nzchar(Sys.getenv("GEOV2"))||identical(.e,"v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_",.e) else "exposure" }
TAG  <- Sys.getenv("GEO_TAG")   # "" (untagged) | "_min" (v2) | "_v11" (frozen); tags the output JSON
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT,"out",exp_dirname())
DV   <- file.path(ROOT,"out","dict","vintage"); CFG <- file.path(ROOT,"pipeline","config")
say  <- function(...) cat(sprintf(...),"\n")
NW <- 6L; TIEM <- "first"; WARM <- 36L; nrm <- function(x) tolower(x)
if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
ff5  <- tryCatch({f<-fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); f[, Month:=floor_date(as.Date(Date),"month")]; f}, error=function(e) NULL)

# shared: FF5 alpha (in-sample) + walk-forward OOS alpha for a monthly L/S table (Month, LS)
alpha_inoos <- function(w){
  if (is.null(ff5) || !nrow(w)) return(NULL)
  dd <- merge(w[,.(Month,LS)], ff5, by="Month"); dd <- dd[is.finite(LS)&is.finite(MktRF)][order(Month)]
  if (nrow(dd) < WARM+12L) return(list(alpha=NA,t=NA,n=nrow(dd),oos_alpha=NA,oos_t=NA,oos_n=0))
  m  <- lm(LS~MktRF+SMB+HML+RMW+CMA, dd)
  se <- if (have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
  n <- nrow(dd); oos <- rep(NA_real_, n)
  for (i in seq_len(n)) if (i > WARM) { fit <- lm(LS~MktRF+SMB+HML+RMW+CMA, dd[1:(i-1)])
    oos[i] <- dd$LS[i] - sum(coef(fit)[-1]*as.numeric(dd[i,.(MktRF,SMB,HML,RMW,CMA)])) }
  ov <- oos[is.finite(oos)]; om <- lm(ov~1)
  ose <- if (have_nw) sqrt(sandwich::NeweyWest(om,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(om)$coef[1,2]
  list(alpha=unname(coef(m)[1]), t=unname(coef(m)[1]/se), n=n,
       oos_alpha=mean(ov), oos_t=mean(ov)/ose, oos_n=length(ov))
}
# shared: monthly Q5-Q1 EW L/S from a firm-quarter panel keyed by `idc`, joined to monthly returns
ls_table <- function(fq, idc, mc, ret, retcol){
  d0 <- fq[is.finite(get(mc))]; if (!nrow(d0)) return(NULL)
  d0[, form_q := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
  hm <- d0[rep(seq_len(.N), each=3L)]; hm[, k:=rep(0:2,times=nrow(d0))]; hm[, Month:=form_q %m+% months(3L+k)]; hm[,k:=NULL]
  p  <- merge(hm, ret, by=c(idc,"Month")); p <- p[is.finite(get(retcol))]
  if (!nrow(p)) return(NULL)
  p[, rw := pmin(pmax(get(retcol),quantile(get(retcol),.005,na.rm=TRUE)),quantile(get(retcol),.995,na.rm=TRUE)), by=Month]
  p[, b := as.integer(ceiling(frank(get(mc),ties.method=TIEM)/.N*5)), by=Month]
  w <- dcast(p[,.(r=mean(rw,na.rm=TRUE)),by=.(Month,b)], Month~b, value.var="r")
  setnames(w, as.character(1:5), paste0("Q",1:5)); w <- w[is.finite(Q1)&is.finite(Q5)]; w[, LS := Q5-Q1]; w[,.(Month,LS)]
}

out <- list(note="expanding-window OOS-on-discovery: vintage dicts vs full headline dict, and real-time (LSEG + CRSP) GeoRisk/GeoSentiment L/S, fixed vs real-time dictionary", ties=TIEM, warmup_months=WARM)

# ---- (1) dictionary divergence ----------------------------------------------
fd <- tryCatch(fread(file.path(CFG,"dictionary_geoeconomic.csv")), error=function(e) NULL)
vfiles <- sort(list.files(DV, pattern="^dictionary_vintage_\\d{4}\\.csv$", full.names=TRUE))
if (!is.null(fd) && length(vfiles)) {
  fullset <- unique(nrm(fd$bigram)); top1000 <- if ("G2" %in% names(fd)) nrm(head(fd[order(-G2)]$bigram,1000)) else head(fullset,1000)
  vb <- setNames(lapply(vfiles, function(f) unique(nrm(fread(f)$bigram))), as.integer(sub(".*_(\\d{4})\\.csv$","\\1", vfiles)))
  div <- rbindlist(lapply(names(vb), function(y){ v<-vb[[y]]
    data.table(vintage=as.integer(y), n_terms=length(v),
               cover_full=round(mean(fullset %in% v),4), cover_top1000=round(mean(top1000 %in% v),4),
               jaccard_full=round(length(intersect(v,fullset))/length(union(v,fullset)),4)) }))
  yrs <- sort(as.integer(names(vb)))
  consec <- if (length(yrs)>1) rbindlist(lapply(seq_len(length(yrs)-1L), function(i){a<-vb[[as.character(yrs[i])]];b<-vb[[as.character(yrs[i+1])]]
    data.table(from=yrs[i],to=yrs[i+1],jaccard=round(length(intersect(a,b))/length(union(a,b)),4),new_terms=length(setdiff(b,a)))})) else NULL
  out$divergence <- list(by_vintage=div, consecutive=consec)
  say("[divergence] %d vintages | cover_top1000 %.2f..%.2f", length(vb), min(div$cover_top1000), max(div$cover_top1000))
} else say("[divergence] no vintage dicts yet (Phase A pending?)")

# ---- load the real-time per-call exposure (Phase B) --------------------------
rtf <- sort(list.files(EXP, pattern="^exposure_rt_\\d{4}\\.rds$", full.names=TRUE))
rt  <- if (length(rtf)) rbindlist(lapply(rtf, readRDS), use.names=TRUE, fill=TRUE) else NULL

# ---- (2a) LSEG real-time L/S (ticker) ---------------------------------------
mpath <- { cand <- c(file.path(ANA,sprintf("panel_ric_monthly%s.rds",TAG)), file.path(ANA,"panel_ric_monthly.rds")); w <- cand[file.exists(cand)]; if (length(w)) w[1] else cand[1] }  # prefer the tagged panel (v2: _min)
if (!is.null(rt) && all(c("ticker","year","quarter") %in% names(rt)) && file.exists(mpath)) {
  rt_fq <- rt[!is.na(ticker), .(GeoRisk_rt=mean(GeoRisk,na.rm=TRUE), GeoSentiment_rt=mean(GeoSentiment,na.rm=TRUE)), by=.(ticker,year,quarter)]
  pm <- as.data.table(readRDS(mpath)); pm[, Month:=as.Date(Month)]
  tk <- intersect(c("ticker","Ticker"), names(pm))[1]; rc <- intersect(c("RetM_w","RetM","Ret"), names(pm))[1]
  if (!is.na(tk) && !is.na(rc)) {
    setnames(pm, tk, "ticker"); if(!"year"%in%names(pm)) pm[,year:=year(Month)]; if(!"quarter"%in%names(pm)) pm[,quarter:=quarter(Month)]
    fixedL <- unique(pm[,.(ticker,year,quarter,GeoRisk,GeoSentiment)])
    retL <- pm[, c("ticker","Month",rc), with=FALSE]; setnames(retL, rc, "RetM")
    fqL <- merge(fixedL, rt_fq, by=c("ticker","year","quarter"), all.x=TRUE)
    lse <- list()
    for (base in c("GeoRisk","GeoSentiment")) lse[[base]] <- list(
      fixed   = alpha_inoos(ls_table(fqL, "ticker", base,           retL, "RetM")),
      realtime= alpha_inoos(ls_table(fqL, "ticker", paste0(base,"_rt"), retL, "RetM")))
    out$lseg_realtime <- lse; say("[realtime LSEG] done")
  }
} else say("[realtime LSEG] inputs not ready -> skipped")

# ---- (2b) CRSP real-time L/S (permno via ec_ccm crosswalk) ------------------
mapf <- { p2 <- file.path(ROOT,"ra_project/GEO_RA_mapping_v8.2/output/ec_ccm_map_v8.rds"); if (file.exists(p2)) p2 else file.path(ROOT,"ra_project/mapping/output/ec_ccm_map_v8.rds") }  # prefer v8.2 crosswalk (matches 03e), fall back to pre-v8.2
crm <- file.path(ANA,"crsp_returns_monthly.rds"); fcx <- file.path(EXP,"exposure_firmquarter_crsp.rds")
if (!is.null(rt) && "Id" %in% names(rt) && file.exists(mapf) && file.exists(crm) && file.exists(fcx)) {
  emap <- as.data.table(readRDS(mapf)); emap <- unique(emap[!is.na(permno), .(Id=eventId, permno=as.integer(permno))], by="Id")
  rtc  <- merge(rt[,.(Id,year,quarter,GeoRisk,GeoSentiment)], emap, by="Id")
  rt_fqc <- rtc[!is.na(permno), .(GeoRisk_rt=mean(GeoRisk,na.rm=TRUE), GeoSentiment_rt=mean(GeoSentiment,na.rm=TRUE)), by=.(permno,year,quarter)]
  fixedC <- as.data.table(readRDS(fcx)); fixedC <- fixedC[, .(permno=as.integer(permno), year, quarter, GeoRisk, GeoSentiment)]
  fqC <- merge(fixedC, rt_fqc, by=c("permno","year","quarter"), all.x=TRUE)
  retC <- as.data.table(readRDS(crm)); retC[, permno:=as.integer(permno)]; if("Month"%in%names(retC)) retC[,Month:=as.Date(Month)]
  rcn <- intersect(c("RetM","ret","RetM_w"), names(retC))[1]; retC <- retC[, c("permno","Month",rcn), with=FALSE]; setnames(retC, rcn, "RetM")
  crp <- list()
  for (base in c("GeoRisk","GeoSentiment")) crp[[base]] <- list(
    fixed   = alpha_inoos(ls_table(fqC, "permno", base,           retC, "RetM")),
    realtime= alpha_inoos(ls_table(fqC, "permno", paste0(base,"_rt"), retC, "RetM")))
  out$crsp_realtime <- crp; say("[realtime CRSP] done")
} else say("[realtime CRSP] inputs not ready (need ec_ccm map + crsp returns + fixed CRSP exposure) -> skipped")

outf <- sprintf("realtime_dict_oos%s.json", TAG)
write_json(out, file.path(ANA,outf), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("\n  wrote out/analysis/%s", outf)
