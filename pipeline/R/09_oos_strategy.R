# ==============================================================================
# pipeline/R/09_oos_strategy.R   (Stage 5 — genuine walk-forward OOS of GeoRisk L/S)
#
# Addresses the in-sample critiques of the headline Q3 alpha (full-sample FF5
# regression; recency concentration). The portfolio FORMATION is already real-time
# (sort on the signal known that month, hold forward). This adds the rigorous part:
#   (1) WALK-FORWARD OOS alpha: at each month t, estimate FF5 betas on months < t
#       only, then OOS abnormal return AR_t = LS_t - beta'F_t (no future info).
#       Mean(AR) + NW t = a genuinely out-of-sample risk-adjusted return.
#   (2) EXPANDING-WINDOW alpha path: full-sample-to-date alpha & t at each year-end
#       (shows WHEN the effect became significant).
#   (3) Real-time cumulative L/S equity curve.
# Runs for the active version (GEOV2 -> _v2 panels + _v2 output). The DICTIONARY is
# held fixed (full-sample discovered) -- documented limitation; this is OOS on the
# pricing/strategy, not on dictionary discovery.
#
#   Rscript pipeline/R/09_oos_strategy.R          # v1
#   GEOV2=1 Rscript pipeline/R/09_oos_strategy.R  # v2
# Output: out/analysis/oos_strategy{,_v2}.json + out/figures/fig_P_oos_{...}.png
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate); library(ggplot2) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); FIG <- file.path(ROOT,"out","figures")
dir.create(FIG, showWarnings=FALSE, recursive=TRUE); say <- function(...) cat(sprintf(...),"\n")
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX; vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
RP <- if (RAW) "RetM" else "RetM_w"; TIEM <- if (FIX) "random" else "first"; if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
NW <- 6L; WARM <- 36L; NSEED <- if (FIX && TIEM=="random") 25L else 1L     # seed-average the random tie-break for a stable OOS number
EXP <- file.path(ROOT, "out", { .e <- Sys.getenv("GEO_EXPO")   # self-contained (09 doesn't source utils.R)
  if (EXV2) "exposure_v2" else if (nzchar(.e)) paste0("exposure_", .e) else "exposure" })

ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); ff5[, Month := floor_date(as.Date(Date),"month")]
FC <- intersect(names(ff5), c("MktRF","SMB","HML","RMW","CMA")); ff5 <- ff5[, c("Month",FC), with=FALSE]

# ---- build a real-time monthly L/S series for one universe + measure ----------
ls_series <- function(panel, retcol, meas="GeoRisk", seed=NULL){
  d <- panel[is.finite(get(meas)) & is.finite(get(retcol))]; if(!is.null(seed)) set.seed(seed)
  d[, b := as.integer(ceiling(frank(get(meas), ties.method=TIEM)/.N*5)), by=Month]   # real-time cross-sectional sort
  ew <- d[, .(Ret=mean(get(retcol),na.rm=TRUE)), by=.(Month,b)]
  w  <- dcast(ew, Month~b, value.var="Ret"); w[, LS := get("5")-get("1")]
  w[is.finite(LS), .(Month, LS)][order(Month)]
}

# ---- the two universes (CRSP monthly = headline; LSEG monthly = breadth) ------
crsp_panel <- {
  fq <- as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); fq[, permno:=as.integer(permno)]
  fq[, form_q := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
  hm <- fq[rep(seq_len(.N), each=3L)]; hm[, k:=rep(0:2,times=nrow(fq))]; hm[, Month:=form_q %m+% months(3L+k)]; hm[,k:=NULL]
  ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[, permno:=as.integer(permno)]
  p <- merge(hm, ret[,.(permno,Month,RetM,MCap_MEnd)], by=c("permno","Month")); p <- p[is.finite(RetM)]
  p[, RetM_w := pmin(pmax(RetM, quantile(RetM,.005,na.rm=TRUE)), quantile(RetM,.995,na.rm=TRUE)), by=Month]; p }
lseg_panel <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric_monthly.rds")))); lseg_panel[, Month:=as.Date(Month)]

# ---- OOS engine for one L/S series -------------------------------------------
oos_one <- function(ls, label){
  d <- merge(ls, ff5, by="Month")[order(Month)]; d <- d[is.finite(LS) & is.finite(MktRF)]
  n <- nrow(d); if (n < WARM+24) { say("  [%s] too short (%d)", label, n); return(NULL) }
  # (1) walk-forward OOS abnormal returns: betas from months < t (>=WARM history)
  ar <- rep(NA_real_, n)
  for (t in (WARM+1):n){
    tr <- d[1:(t-1)]; fit <- lm(reformulate(FC,"LS"), tr); b <- coef(fit)[-1]
    ar[t] <- d$LS[t] - sum(b * as.numeric(d[t, ..FC]))
  }
  oos <- ar[is.finite(ar)]; mo <- lm(oos~1)
  se <- if(have_nw) sqrt(sandwich::NeweyWest(mo,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(mo)$coef[1,2]
  oos_alpha <- mean(oos); oos_t <- oos_alpha/se
  # (2) expanding-window in-sample-to-date alpha path (year-ends)
  ye <- d[, .I[.N], by=year(Month)]$V1; ye <- ye[d$Month[ye] >= d$Month[WARM]]
  path <- rbindlist(lapply(ye, function(i){ dd<-d[1:i]; m<-lm(reformulate(FC,"LS"),dd)
    s<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
    data.table(through=year(d$Month[i]), n=i, alpha=unname(coef(m)[1]), t=unname(coef(m)[1]/s)) }))
  # (3) real-time cumulative equity curve
  eq <- data.table(Month=d$Month, LS=d$LS, cum=cumprod(1+d$LS))
  full <- { m<-lm(reformulate(FC,"LS"),d); s<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
            list(alpha=unname(coef(m)[1]), t=unname(coef(m)[1]/s), n=n) }
  say("  [%s] full-sample alpha %.3f%%/m t=%.2f | WALK-FWD OOS alpha %.3f%%/m t=%.2f (n_oos=%d) | mean LS %.3f%%/m Sharpe(ann) %.2f",
      label, 100*full$alpha, full$t, 100*oos_alpha, oos_t, length(oos),
      100*mean(d$LS), sqrt(12)*mean(d$LS)/sd(d$LS))
  list(label=label, full=full, oos=list(alpha=oos_alpha, t=oos_t, n=length(oos), warmup_m=WARM),
       mean_ls=mean(d$LS), sharpe_ann=sqrt(12)*mean(d$LS)/sd(d$LS),
       expanding_path=path, equity=eq, n_months=n)
}

# seed-average a measure's L/S OOS over NSEED random-tie draws (stable headline number);
# keep the first seed's path/equity for the figure.
oos_avg <- function(panel, retcol, meas, label){
  runs <- lapply(seq_len(NSEED), function(s) oos_one(ls_series(panel, retcol, meas, seed=if(FIX) s else NULL), label))
  runs <- Filter(Negate(is.null), runs); if(!length(runs)) return(NULL)
  ft <- sapply(runs, function(r) r$full$t); ot <- sapply(runs, function(r) r$oos$t)
  fa <- sapply(runs, function(r) r$full$alpha); oa <- sapply(runs, function(r) r$oos$alpha)
  list(label=label, n_seeds=length(runs),
       full=list(alpha=mean(fa), t=mean(ft), t_sd=sd(ft)),
       oos =list(alpha=mean(oa), t=mean(ot), t_sd=sd(ot), n=runs[[1]]$oos$n, warmup_m=WARM),
       sharpe_ann=mean(sapply(runs,function(r) r$sharpe_ann)),
       expanding_path=runs[[1]]$expanding_path, equity=runs[[1]]$equity)
}

# ---- RQ1: split-half stability of the contemporaneous realization -------------
rq1_split <- function(){
  suppressPackageStartupMessages(library(fixest))
  p <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds")))); setorder(p, Ticker, year, quarter)
  p[, qidx := year*4 + quarter]; csz<-function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}
  med <- median(p$qidx, na.rm=TRUE)
  one <- function(meas){
    p[, dv := { d<-get(meas)-shift(get(meas)); dq<-qidx-shift(qidx); ifelse(dq==1L,d,NA_real_) }, by=Ticker]
    p[, dz := csz(dv), by=Quarter]; res<-list()
    for(h in c("H1","H2")){ d <- p[is.finite(dz) & is.finite(QuarterlyRet_W) & (if(h=="H1") qidx<=med else qidx>med)]
      m <- tryCatch(feols(QuarterlyRet_W ~ dz | Ticker + Quarter, d, vcov=if(FIX) ~Ticker+Quarter else ~Ticker), error=function(e)NULL)
      res[[h]] <- if(is.null(m)) list(coef=NA,t=NA) else list(coef=unname(coef(m)["dz"]), t=unname(coef(m)["dz"]/se(m)["dz"])) }
    res }
  mm <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
  setNames(lapply(mm, one), mm)
}

# ---- RQ2: split-half stability of the Fama-MacBeth price of risk (headline spec) ----
rq2_fm_split <- function(meas="GeoSentiment"){
  p <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds")))); p[, qidx := year*4 + quarter]
  csz<-function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}
  med <- median(p$qidx, na.rm=TRUE)
  p[, ms := csz(get(meas)), by=Quarter]
  for (v in c("Size","Momentum","BM","Beta_CAPM")) p[, paste0("z_",v) := csz(get(v)), by=Quarter]
  zc <- paste0("z_", c("Size","Momentum","BM","Beta_CAPM")); cols <- c("Ret_lead","ms",zc)
  res <- list()
  for (h in c("H1","H2")) {
    d <- p[(if (h=="H1") qidx<=med else qidx>med)]
    lam <- d[, { x<-.SD[complete.cases(.SD)]; if(nrow(x)<30) NA_real_ else
                 coef(lm(reformulate(c("ms",zc),"Ret_lead"), x))["ms"] }, by=Quarter, .SDcols=cols]$V1
    lam <- lam[is.finite(lam)]; m<-lm(lam~1)
    se <- if (requireNamespace("sandwich",quietly=TRUE))
            sqrt(sandwich::NeweyWest(m,lag=4L,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]
    res[[h]] <- list(lambda=mean(lam), t=mean(lam)/se, n_q=length(lam))
  }
  res
}

say("=== OOS across all RQs (%s) — dictionary held fixed (OOS on strategy/pricing, not discovery) ===", ifelse(nzchar(TAG),TAG,"v1"))
say("[RQ3] GeoRisk L/S walk-forward (seed-avg over %d):", NSEED)
rq3 <- list(crsp = oos_avg(crsp_panel, RP, "GeoRisk", "CRSP monthly EW"),
            lseg = oos_avg(lseg_panel, RP, "GeoRisk", "LSEG monthly EW"))
say("[RQ2] GeoSentiment L/S walk-forward (the tone strategy, OOS):")
rq2 <- list(crsp = oos_avg(crsp_panel, RP, "GeoSentiment", "CRSP monthly EW"),
            lseg = oos_avg(lseg_panel, RP, "GeoSentiment", "LSEG monthly EW"))
say("[RQ1] contemporaneous realization split-half (H1 vs H2):")
rq1 <- rq1_split()
for(m in names(rq1)) say("   %-14s H1 t=%.2f | H2 t=%.2f", m, rq1[[m]]$H1$t, rq1[[m]]$H2$t)
say("[RQ2] Fama-MacBeth price-of-risk split-half (GeoSentiment, headline controls):")
rq2fm <- rq2_fm_split("GeoSentiment")
say("   GeoSentiment   H1 lam=%.3f t=%.2f | H2 lam=%.3f t=%.2f",
    100*rq2fm$H1$lambda, rq2fm$H1$t, 100*rq2fm$H2$lambda, rq2fm$H2$t)
res <- rq3   # back-compat: existing figure/readers use res$crsp (GeoRisk)

write_json(list(version=ifelse(nzchar(TAG),sub("_","",TAG),"v1"), warmup_months=WARM, nw_lag=NW, n_seeds=NSEED,
                note="Real-time portfolio formation; walk-forward OOS alpha = FF5 betas from months < t. Seed-averaged over the random tie-break. RQ1 = split-half stability of the contemporaneous realization; rq2_fm_split = split-half FM price of risk (headline controls). Dictionary full-sample discovered (documented limitation).",
                rq1_realization_split=rq1, rq2_fm_split=list(GeoSentiment=rq2fm),
                rq2_tone_oos=rq2, rq3_strategy_oos=rq3,
                results=res), vS(file.path(ANA,"oos_strategy.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)

# ---- figure: expanding-window t-path + cumulative equity (CRSP headline) ------
if (!is.null(res$crsp)) {
  pa <- as.data.table(res$crsp$expanding_path); eq <- as.data.table(res$crsp$equity)
  PAL <- if (V2) c(line="#2E5A88", ref="#3d9c86") else c(line="#7a3b8f", ref="#c0792e")
  g1 <- ggplot(pa, aes(through, t)) + geom_hline(yintercept=c(1.96,-1.96), linetype="dotted", colour="grey50") +
    geom_line(linewidth=0.9, colour=PAL["line"]) + geom_point(size=1.6, colour=PAL["line"]) +
    labs(title=sprintf("GeoRisk L/S — expanding-window FF5 alpha t (%s, CRSP)", toupper(if(V2)"v2" else "v1")),
         subtitle="t-stat using data only through each year-end; dotted = +/-1.96", x=NULL, y="alpha t-stat") +
    theme_minimal(base_size=12)
  ggsave(file.path(FIG, sprintf("fig_P_oos_tpath_%s.png", if(V2)"v2" else "v1")), g1, width=8, height=4.2, dpi=300, bg="white")
  g2 <- ggplot(eq, aes(Month, cum)) + geom_line(linewidth=0.8, colour=PAL["line"]) +
    scale_y_log10() + labs(title=sprintf("GeoRisk L/S real-time cumulative return (%s, CRSP, log)", toupper(if(V2)"v2" else "v1")),
                           x=NULL, y="cumulative (1+LS), log") + theme_minimal(base_size=12)
  ggsave(file.path(FIG, sprintf("fig_P_oos_equity_%s.png", if(V2)"v2" else "v1")), g2, width=8, height=4.2, dpi=300, bg="white")
  say("  wrote fig_P_oos_{tpath,equity}_%s.png", if(V2)"v2" else "v1")
}
say("  wrote %s", vS("out/analysis/oos_strategy.json"))
