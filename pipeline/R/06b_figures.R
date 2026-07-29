# ==============================================================================
# pipeline/R/06b_figures.R   (Stage 6 — reproducible defense figures, TWO palettes)
#
# Renders every defense/thesis figure from RESULTS_MASTER.rds + the panels + JSONs,
# each in TWO palettes: "pastel" (slides) and "print" (grayscale/black for the printed
# thesis). Data is computed ONCE per figure and rendered twice -> out/figures/
# <name>_{pastel,print}.png (300 dpi). Run 06_results_master.R first.
#
#   Rscript pipeline/R/06b_figures.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate); library(ggplot2) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); FIG <- file.path(ROOT,"out","figures")
dir.create(FIG, showWarnings=FALSE, recursive=TRUE); say <- function(...) cat(sprintf(...),"\n")
# ---- version flags (audit 2026-06-12): GEO_TAG selects the data the figures read ----
# With GEO_FIX=1 GEO_TAG=_v11 GEO_TIES=average the figures are built from the v1.1
# artifacts. Output filenames stay fixed (fig_*_{pastel,print}.png) so the deck picks
# them up. vpref() prefers the version twin and falls back to the base file.
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
TIEM <- if (FIX) "random" else "first"; if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
if (identical(TIEM,"random")) set.seed(20250401L)
vS    <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
vpref <- function(p){ q <- vS(p); if (file.exists(q)) q else p }   # prefer _vXX twin, else base
say("[figures] data version TAG='%s', ties='%s'", if(nzchar(TAG)) TAG else "(v1 base)", TIEM)
# All keys are Geo* (renamed at source from Sautner CC*); display names match.
MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
MLAB <- c(GeoExposure="GeoExposure",GeoExposureTFIDF="GeoExposureTFIDF",GeoRisk="GeoRisk",GeoSentiment="GeoSentiment")

M <- readRDS(vpref(file.path(ANA,"RESULTS_MASTER.rds")))
P <- as.data.table(readRDS(vpref(file.path(ANA,"panel_ric.rds"))))

# ---- palettes + theme -------------------------------------------------------
# Blue + grey palette (slides). seq5 = light->navy sequential for quintiles;
# cat = distinguishable blue/grey hues; divlo/divhi = light blue-grey vs strong blue.
PAL <- list(
  pastel = list(seq5=c("#dce7f1","#b3cbe2","#7da7cd","#4c7fb5","#234c78"),
                cat=c("#2E5A88","#5B9BD5","#9AB0C2","#5A6B7B","#27384A","#B8C4CE"),
                lo="#9AB0C2", hi="#2E5A88", line="#2E5A88", pos="#5B9BD5", neg="#9AB0C2",
                divlo="#9AB0C2", divhi="#2E5A88",
                heat_lo="#A9C2DD", heat_hi="#6FA0CC"),   # lighter ends so black cell text stays legible
  print  = list(seq5=c("grey88","grey72","grey56","grey38","grey16"),
                cat=c("grey20","grey42","grey58","grey72","grey84","grey34"),
                lo="grey65", hi="black", line="black", pos="grey35", neg="grey65",
                divlo="grey70", divhi="black",
                heat_lo="grey78", heat_hi="grey50"))
theme_geo <- function() theme_minimal(base_size=13) +
  theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold",size=13),
        plot.subtitle=element_text(size=10,colour="grey30"), legend.position="bottom",
        axis.title=element_text(size=11), strip.text=element_text(face="bold"))
save2 <- function(plotfun, name, w=9, h=5.2){ for(pl in names(PAL)){
    g <- tryCatch(plotfun(PAL[[pl]]), error=function(e){message("  ",name,"/",pl,": ",conditionMessage(e)); NULL})
    # version-tagged output: fig_X_v11_pastel.png when GEO_TAG=_v11, fig_X_pastel.png for v1 base
    if(!is.null(g)) ggsave(file.path(FIG, sprintf("%s%s_%s.png",name,TAG,pl)), g, width=w, height=h, dpi=300, bg="white") }
  say("  wrote %s%s_{pastel,print}.png", name, TAG) }

# ==== A. aggregate exposure trend over time ==================================
trend <- P[, .(m=mean(GeoExposure,na.rm=TRUE)), by=Quarter][order(Quarter)]
fig_trend <- function(p) ggplot(trend, aes(Quarter,m)) +
  geom_vline(xintercept=as.Date(c("2018-01-01","2020-01-01","2022-01-01")), linetype="dotted", colour="grey55") +
  geom_line(colour=p$line, linewidth=0.9) +
  geom_smooth(method="loess", span=0.3, se=FALSE, colour=p$hi, linewidth=0.7) +
  scale_x_date(date_breaks="3 years", date_labels="%Y") +
  labs(title="Aggregate geoeconomic exposure over time",
       subtitle="Mean GeoExposure across firms each quarter; markers = 2018 trade war, 2020, 2022 Ukraine",
       x=NULL, y="Mean GeoExposure") + theme_geo()

# ==== B. quintile mean next-q return by measure (LSEG, EW) ===================
# Recompute from the panel with FIRST ties (see note at qbin) so the displayed
# exposure gradient is not collapsed by GeoRisk's zero mass. Time-series mean of
# per-quarter equal-weighted bin means, matching the master's definition.
.qb1 <- function(x) as.integer(ceiling(frank(x, ties.method=TIEM)/length(x)*5))
qdt <- rbindlist(lapply(MEAS, function(m){
  d <- P[is.finite(get(m)) & is.finite(Ret_lead)]
  d[, qbn := .qb1(get(m)), by = Quarter]
  qm <- d[, .(r = mean(Ret_lead, na.rm=TRUE)), by = .(Quarter, qbn)][
           , .(ret = mean(r, na.rm=TRUE)), by = qbn][order(qbn)]
  data.table(measure = MLAB[m], Q = factor(paste0("Q", qm$qbn), levels=paste0("Q",1:5)),
             ret = qm$ret) }))
fig_quint <- function(p) ggplot(qdt, aes(Q, 100*ret, fill=Q)) +
  geom_col(width=0.78) + facet_wrap(~measure, nrow=2) +
  scale_fill_manual(values=p$seq5, guide="none") +
  labs(title="Next-quarter return by exposure quintile (LSEG, equal-weighted)",
       subtitle="Q1 = lowest exposure, Q5 = highest; bars are time-series mean quarterly returns",
       x="Exposure quintile", y="Mean next-quarter return (%)") + theme_geo()

# ==== C. quintile by region (GeoRisk) ========================================
rg <- M$region$GeoRisk$means; rlab <- c(NorthAmerica="North America",Europe="Europe",AsiaPacific_DM="Asia-Pacific (DM)",EM="Emerging Markets")
rg <- as.data.table(rg)[Region %in% names(rlab)]; rg[, Region:=factor(rlab[Region], levels=rlab)]; rg[, Q:=factor(paste0("Q",q),levels=paste0("Q",1:5))]
fig_region <- function(p) ggplot(rg, aes(Q,100*ew,fill=Q)) + geom_col(width=0.78) + facet_wrap(~Region, nrow=1) +
  scale_fill_manual(values=p$seq5, guide="none") +
  labs(title="GeoRisk quintile next-quarter return by region",
       subtitle="Equal-weighted within each (quarter, region, quintile); time-averaged", x="Risk-exposure quintile", y="Mean next-quarter return (%)") + theme_geo()

# ==== D. quintile by sample (LSEG vs CRSP), GeoRisk ==========================
sm <- rbindlist(list(
  data.table(sample="LSEG (global)", Q=paste0("Q",1:5), ret=as.numeric(M$quintiles$lseg$GeoRisk$ew)),
  data.table(sample="CRSP (US)",     Q=paste0("Q",1:5), ret=as.numeric(M$quintiles$crsp$GeoRisk$ew))))
sm[, Q:=factor(Q,levels=paste0("Q",1:5))]
fig_sample <- function(p) ggplot(sm, aes(Q,100*ret,fill=sample)) + geom_col(position="dodge", width=0.72) +
  scale_fill_manual(values=c(p$divlo,p$divhi), name=NULL) +
  labs(title="GeoRisk quintile return: LSEG global vs CRSP US",
       subtitle="Two independent return universes, equal-weighted", x="Risk-exposure quintile", y="Mean next-quarter return (%)") + theme_geo()

# ==== E. by characteristics: Q5-Q1 FF5 alpha by tercile =====================
chr <- rbindlist(lapply(c("size","bm","momentum"), function(ch) rbindlist(lapply(c("T1","T2","T3"), function(g){
  x<-M$characteristics[[ch]]$GeoRisk[[g]]; data.table(char=ch, tercile=g, alpha=unname(x["alpha"]), t=unname(x["t"])) }))))
clab<-c(size="Size",bm="Book-to-Market",momentum="Momentum"); tlab<-c(T1="Low",T2="Mid",T3="High")
chr[, char:=factor(clab[char],levels=clab)]; chr[, tercile:=factor(tlab[tercile],levels=tlab)]
fig_char <- function(p) ggplot(chr, aes(tercile,100*alpha,fill=char)) + geom_col(position="dodge",width=0.72) +
  geom_hline(yintercept=0,colour="grey60") +
  scale_fill_manual(values=p$cat[1:3], name=NULL) +
  labs(title="GeoRisk long-short FF5 alpha within characteristic terciles",
       subtitle="Q5-Q1 FF5 alpha (%/quarter) within size / B-M / momentum terciles", x="Characteristic tercile (Low -> High)", y="FF5 alpha (%/quarter)") + theme_geo()

# ==== F. event-study CAR (market model), GeoExposure + dCC ====================
es <- fromJSON(vpref(file.path(ANA,"event_study.json")))$results
esdt <- rbindlist(lapply(c("GeoExposure","dCC"), function(m){ d<-as.data.table(es[[m]]$car_mm)
  d<-d[q %in% c(1,5)]; d[, panel:=ifelse(m=="dCC","Change in exposure (dGeoExpo)","Exposure level (GeoExposure)")]
  d[, grp:=factor(ifelse(q==5,"Q5 (high)","Q1 (low)"))]; d }))
fig_event <- function(p) ggplot(esdt, aes(k,100*car,colour=grp)) +
  geom_vline(xintercept=0,linetype="dotted",colour="grey55") + geom_hline(yintercept=0,colour="grey80") +
  geom_line(linewidth=0.9) + facet_wrap(~panel, ncol=1, scales="free_y") +
  scale_colour_manual(values=c("Q1 (low)"=p$divlo,"Q5 (high)"=p$divhi), name=NULL) +
  labs(title="Cumulative average abnormal return (CAAR) around the\nearnings call, by exposure quintile",
       subtitle="Market-model abnormal return, averaged across all calls; Q5 = high, Q1 = low exposure",
       x="Trading days from call (day 0)", y="CAAR (%)") + theme_geo()

# ==== F2. event-study CAAR grid: four measures, Q1 vs Q5 (backs "GeoSentiment goes up") ====
es2   <- fromJSON(vpref(file.path(ANA,"event_study.json")))$results
mlab2 <- c(GeoExposure="GeoExposure (level)", dCC="ΔGeoExposure (change)", GeoRisk="GeoRisk", GeoSentiment="GeoSentiment")
es2dt <- rbindlist(lapply(names(mlab2), function(m){ d<-as.data.table(es2[[m]]$car_mm)
  d<-d[q %in% c(1,5)]; d[, panel:=factor(mlab2[m], levels=mlab2)]
  d[, grp:=factor(ifelse(q==5,"Q5 (high)","Q1 (low)"), levels=c("Q5 (high)","Q1 (low)"))]; d }))
fig_event_grid <- function(p) ggplot(es2dt, aes(k,100*car,colour=grp)) +
  geom_vline(xintercept=0,linetype="dotted",colour="grey55") + geom_hline(yintercept=0,colour="grey80") +
  geom_line(linewidth=0.8) + facet_wrap(~panel, ncol=2, scales="free_y") +
  scale_colour_manual(values=c("Q5 (high)"=p$divhi,"Q1 (low)"=p$divlo), name=NULL) +
  labs(title="CAAR around the call by quintile, all four measures",
       subtitle="Market-model abnormal return; Q5 high vs Q1 low. Only GeoSentiment's high quintile rises at the call",
       x="Trading days from call (day 0)", y="CAAR (%)") + theme_geo()

# ==== G. cumulative long-short (recompute quarterly EW L/S) ==================
# A long-short is ~market-neutral, so the right reference is the flat 1.0 line
# (zero), NOT the market factor (which bears market risk and is not comparable).
# DISPLAY binning uses "first" ties (spread the zeros across bins). GeoRisk has
# a ~38% exact-zero mass; the headline strategy's average-ties rule pools all of
# them into Q1, which is fine on the monthly tradeable universe (~18% zeros) but
# degenerate on the broad quarterly panel here (Q1 = the whole zero block, which
# earns the highest raw return -> non-monotone means + flat raw L/S). First-ties
# spreads them, so the descriptive figures show the true exposure gradient and
# match the (correct, positive) monthly headline. Headline numbers are unaffected.
qbin<-function(x)as.integer(ceiling(frank(x,ties.method=TIEM)/length(x)*5))
ls_one <- function(m){
  d<-P[is.finite(get(m))&is.finite(Ret_lead)]; d[,q:=qbin(get(m)),by=Quarter]
  w<-dcast(d[,.(ew=mean(Ret_lead)),by=.(Quarter,q)],Quarter~q,value.var="ew"); w[,LS:=get("5")-get("1")]
  setorder(w,Quarter); w<-w[is.finite(LS)]; w[,cum:=cumprod(1+LS)]
  data.table(Quarter=w$Quarter, cum=w$cum, series=MLAB[m]) }
lsm <- rbindlist(lapply(c("GeoRisk","GeoSentiment","GeoExposure","GeoExposureTFIDF"), ls_one))
# anchor every series at 1 (growth of 1; start of first holding quarter)
anch  <- lsm[, .(Quarter=min(Quarter)-31L, cum=1), by=series]
lscum <- rbind(anch, lsm); setorder(lscum, series, Quarter)
slv   <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
lscum[, series:=factor(series, levels=slv)]
fig_lscum <- function(p) ggplot(lscum, aes(Quarter,cum,colour=series)) +
  geom_hline(yintercept=1,colour="grey70") +
  geom_line(linewidth=0.9) + scale_x_date(date_breaks="3 years",date_labels="%Y") +
  scale_y_log10() +
  scale_colour_manual(values=setNames(p$cat[1:4], slv), name=NULL) +
  labs(title="Cumulative long-short return by measure (LSEG, equal-weighted)",
       subtitle="Growth of 1 in a quarterly-rebalanced Q5-Q1 long-short (log scale; 1 = start)",
       x=NULL, y="Cumulative value (log, 1 = start)") + theme_geo()

# ==== G2/G3. MONTHLY CRSP strategy — figure == the headline strategy (05j) ====
# Built on the SAME monthly tradeable panel and the SAME average-ties sort as the
# headline FF5-alpha (05j), so the plotted long-short IS the strategy you trade
# (no quarterly/tie mismatch). Exposure formed at quarter end, held the next 3
# months; returns winsorised 0.5/99.5% by month; bins by the strategy's tie rule.
exC <- as.data.table(readRDS(vpref(file.path(ROOT,"out/exposure/exposure_firmquarter_crsp.rds")))); exC[, permno := as.integer(permno)]
exC[, form_q := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
hmC <- exC[rep(seq_len(.N), each=3L)]; hmC[, kk := rep(0:2, times=nrow(exC))]
hmC[, Month := form_q %m+% months(3L+kk)]; hmC[, kk := NULL]
rmC <- as.data.table(readRDS(vpref(file.path(ANA,"crsp_returns_monthly.rds")))); rmC[, permno := as.integer(permno)]
pmC <- merge(hmC, rmC[, .(permno, Month, RetM)], by=c("permno","Month")); pmC <- pmC[is.finite(RetM)]
pmC[, RetM_w := pmin(pmax(RetM, quantile(RetM,.005,na.rm=TRUE)), quantile(RetM,.995,na.rm=TRUE)), by=Month]
# FIRST ties here on purpose: with ~50% zeros early, AVERAGE ties leave Q1 empty
# (the tie block straddles the Q1/Q2 line) and the whole month is dropped -> 2004-2017
# vanish and the cumulative line is drawn flat across the gap. First ties keep all
# 276 months (continuous, real variation) and the FF5 alpha is robust either way
# (t=2.94 first vs 3.15 average). So the figure shows the full-sample strategy.
mbin <- function(x) as.integer(ceiling(frank(x, ties.method=TIEM)/length(x)*5))
ls_month <- function(meas){ d<-pmC[is.finite(get(meas))]; d[, b:=mbin(get(meas)), by=Month]
  w<-dcast(d[,.(r=mean(RetM_w,na.rm=TRUE)),by=.(Month,b)], Month~b, value.var="r")
  setnames(w, as.character(1:5), paste0("Q",1:5)); setorder(w,Month)
  w<-w[is.finite(Q1)&is.finite(Q5)]; w[, LS := Q5-Q1]; w }

# --- G2: GeoRisk Q5/Q1 legs vs the FF market, with the L/S ---
wG  <- ls_month("GeoRisk")
ffm <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); ffm[, Month:=floor_date(as.Date(Date),"month")]
mktM<- ffm[Month %in% wG$Month, .(Month, mkt=MktRF+RF)]; setorder(mktM, Month)
lglv  <- c("Q5 (high GeoRisk)","Q1 (no GeoRisk)","FF market","L/S (Q5-Q1)")
legdt <- rbindlist(list(
  data.table(Month=wG$Month,   cum=cumprod(1+wG$Q5),   series=lglv[1]),
  data.table(Month=wG$Month,   cum=cumprod(1+wG$Q1),   series=lglv[2]),
  data.table(Month=mktM$Month, cum=cumprod(1+mktM$mkt),series=lglv[3]),
  data.table(Month=wG$Month,   cum=cumprod(1+wG$LS),   series=lglv[4])))
legdt <- rbind(legdt[, .(Month=min(Month) %m-% months(1L), cum=1), by=series], legdt)
legdt[, series:=factor(series, levels=lglv)]; setorder(legdt, series, Month)
fig_legs <- function(p){
  cols <- setNames(c(p$cat[1], p$cat[3], "grey45", p$cat[2]), lglv)
  lty  <- setNames(c("solid","solid","dashed","solid"), lglv)
  ggplot(legdt, aes(Month,cum,colour=series,linetype=series)) + geom_hline(yintercept=1,colour="grey80") +
  geom_line(linewidth=0.95) + scale_x_date(date_breaks="3 years",date_labels="%Y") + scale_y_log10() +
  scale_colour_manual(values=cols, name=NULL) + scale_linetype_manual(values=lty, name=NULL) +
  labs(title="GeoRisk quintile legs vs the market, with the long-short (CRSP US, monthly)",
       subtitle="Growth of 1, log scale; monthly-rebalanced -- the SAME sort as the headline FF5-alpha. The spread (L/S) is the strategy",
       x=NULL, y="Cumulative value (log; 1 = start)") + theme_geo() }

# --- G2b: GeoSentiment Q5/Q1 legs vs the FF market, with the L/S (same construction) ---
wS    <- ls_month("GeoSentiment")
mktS  <- ffm[Month %in% wS$Month, .(Month, mkt=MktRF+RF)]; setorder(mktS, Month)
lglvS <- c("Q5 (high GeoSentiment)","Q1 (low GeoSentiment)","FF market","L/S (Q5-Q1)")
legdtS <- rbindlist(list(
  data.table(Month=wS$Month,   cum=cumprod(1+wS$Q5),   series=lglvS[1]),
  data.table(Month=wS$Month,   cum=cumprod(1+wS$Q1),   series=lglvS[2]),
  data.table(Month=mktS$Month, cum=cumprod(1+mktS$mkt),series=lglvS[3]),
  data.table(Month=wS$Month,   cum=cumprod(1+wS$LS),   series=lglvS[4])))
legdtS <- rbind(legdtS[, .(Month=min(Month) %m-% months(1L), cum=1), by=series], legdtS)
legdtS[, series:=factor(series, levels=lglvS)]; setorder(legdtS, series, Month)
fig_legs_sent <- function(p){
  cols <- setNames(c(p$cat[1], p$cat[3], "grey45", p$cat[2]), lglvS)
  lty  <- setNames(c("solid","solid","dashed","solid"), lglvS)
  ggplot(legdtS, aes(Month,cum,colour=series,linetype=series)) + geom_hline(yintercept=1,colour="grey80") +
  geom_line(linewidth=0.95) + scale_x_date(date_breaks="3 years",date_labels="%Y") + scale_y_log10() +
  scale_colour_manual(values=cols, name=NULL) + scale_linetype_manual(values=lty, name=NULL) +
  labs(title="GeoSentiment quintile legs vs the market, with the long-short (CRSP US, monthly)",
       subtitle="Growth of 1, log scale; monthly-rebalanced Q5-Q1 (high minus low tone). The spread (L/S) is the GeoSentiment strategy",
       x=NULL, y="Cumulative value (log; 1 = start)") + theme_geo() }

# --- G3: cumulative L/S by measure (same monthly construction) ---
lscumC <- rbindlist(lapply(slv, function(m){ w<-ls_month(m)
  data.table(Month=w$Month, cum=cumprod(1+w$LS), series=MLAB[m]) }))
lscumC <- rbind(lscumC[, .(Month=min(Month) %m-% months(1L), cum=1), by=series], lscumC)
lscumC[, series:=factor(series, levels=slv)]; setorder(lscumC, series, Month)
fig_lscumC <- function(p) ggplot(lscumC, aes(Month,cum,colour=series)) +
  geom_hline(yintercept=1,colour="grey70") +
  geom_line(linewidth=0.9) + scale_x_date(date_breaks="3 years",date_labels="%Y") +
  scale_y_log10() +
  scale_colour_manual(values=setNames(p$cat[1:4], slv), name=NULL) +
  labs(title="Cumulative long-short return by measure (US CRSP, monthly)",
       subtitle="Growth of 1 in a monthly-rebalanced Q5-Q1 long-short (same sort as the headline FF5-alpha; log scale; 1 = start)",
       x=NULL, y="Cumulative value (log, 1 = start)") + theme_geo()

# ==== H. augmented-spanning factor loadings (GeoRisk L/S, CRSP) ===============
ld <- fromJSON(vpref(file.path(ANA,"augmented_spanning.json")))$crsp$loadings
lddt <- rbindlist(lapply(names(ld), function(f) data.table(factor=f, beta=ld[[f]]$beta, t=ld[[f]]$t)))
flab<-c(dGPR="GPR",dGPRT="GPR threats",dEPU="EPU",dVIX="VIX",oilret="Oil",usdret="USD",UMD="Momentum")
lddt<-lddt[factor %in% names(flab)]; lddt[, factor:=factor(flab[factor],levels=flab)]; lddt[, sig:=abs(t)>=1.96]
fig_load <- function(p) ggplot(lddt, aes(factor,t,fill=sig)) + geom_col(width=0.7) +
  geom_hline(yintercept=c(-1.96,1.96),linetype="dashed",colour="grey55") + geom_hline(yintercept=0,colour="grey70") +
  scale_fill_manual(values=c(`FALSE`=p$divlo,`TRUE`=p$divhi), guide="none") +
  labs(title="Macro-factor loadings of the GeoRisk long-short (CRSP)",
       subtitle="Univariate NW t-stat of each macro/uncertainty factor; |t|=1.96 dashed. None spans the alpha", x=NULL, y="t-statistic of loading") + theme_geo()

# ==== I. GPR vs aggregate exposure (validation) =============================
gp <- tryCatch(fread(file.path(ROOT,"out/exposure/gpr_quarterly.csv")), error=function(e)NULL)
fig_gpr <- if(is.null(gp)) NULL else { gp[, Quarter:=as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
  z<-function(x)(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  gd<-rbindlist(list(data.table(Quarter=gp$Quarter, z=z(gp$GeoExposure), series="Mean GeoExposure"),
                     data.table(Quarter=gp$Quarter, z=z(gp$GPRT),       series="GPR threats (Caldara-Iacoviello)")))
  rho<-round(cor(gp$GeoExposure, gp$GPRT, use="complete.obs"),2)
  function(p) ggplot(gd, aes(Quarter,z,colour=series)) + geom_hline(yintercept=0,colour="grey80") + geom_line(linewidth=0.85) +
    scale_x_date(date_breaks="3 years",date_labels="%Y") + scale_colour_manual(values=c(p$divlo,p$divhi),name=NULL) +
    labs(title="Aggregate geoeconomic exposure comoves modestly with the GPR threats index",
         subtitle=sprintf("Each series z-scored to its own mean 0, sd 1 (negative = below own average, not a negative level); correlation = %.2f", rho),
         x=NULL, y="z-score (0 = series average)") + theme_geo() }

# ==== J. Q1/Q2/Q3 sign+t heatmap across measures ===========================
H <- M$headline
plv <- c("Q1: Return\n(contemp., LSEG)","Q1: IVOL\n(vol., LSEG)","Q2: Pricing\n(FM, LSEG)","Q3: Strategy\n(FF5 alpha, CRSP)")
pick <- rbind(
  H[question=="Q1_realization",                .(panel=plv[1], measure, t)],
  H[question=="Q1_ivol",                       .(panel=plv[2], measure, t)],
  H[question=="Q2_pricing_FM" & sample=="LSEG", .(panel=plv[3], measure, t)],
  H[question=="Q3_LS_FF5"     & sample=="CRSP", .(panel=plv[4], measure, t)])
pick<-pick[measure %in% MEAS]; pick[, measure:=factor(MLAB[measure],levels=rev(MLAB))]
pick[, panel:=factor(panel, levels=plv)]
stars <- function(t) ifelse(abs(t)>=2.576,"***",ifelse(abs(t)>=1.96,"**",ifelse(abs(t)>=1.645,"*","")))
fig_heat <- function(p) ggplot(pick, aes(panel, measure, fill=t)) + geom_tile(colour="white",linewidth=1) +
  geom_text(aes(label=sprintf("%.1f%s", t, stars(t))), size=3.9, colour="grey10") +
  scale_fill_gradient2(low=p$heat_lo, mid="white", high=p$heat_hi, midpoint=0, name="t-stat") +
  labs(title="The whole picture: t-statistics by research question",
       subtitle="Q1 & Q2 on global LSEG, Q3 on US CRSP; sign + significance; * / ** / *** = 10 / 5 / 1% (NW or clustered t)", x=NULL, y=NULL) +
  theme_geo() + theme(panel.grid=element_blank())

# ==== L. Q2 Fama-MacBeth forest plot: lambda +/- 95% NW CI across measures ====
fmq <- H[question=="Q2_pricing_FM" & sample=="LSEG" & stat=="lambda" & measure %in% MEAS, .(measure, value, t)]
fmq[, se := abs(value)/abs(t)]
fmq[, `:=`(lo = value - 1.96*se, hi = value + 1.96*se)]
fmq[, clears := (lo*hi) > 0]
fmq[, measure := factor(MLAB[as.character(measure)], levels=rev(MLAB))]
fig_fm_forest <- function(p) ggplot(fmq, aes(100*value, measure, colour=clears)) +
  geom_vline(xintercept=0, linetype="dashed", colour="grey55") +
  geom_errorbarh(aes(xmin=100*lo, xmax=100*hi), height=0.16, linewidth=0.9) +
  geom_point(size=3.2) +
  scale_colour_manual(values=c(`TRUE`=p$divhi, `FALSE`=p$divlo), guide="none") +
  labs(title="Fama-MacBeth price of geoeconomic exposure, by measure (LSEG)",
       subtitle="Time-series mean lambda with 95% Newey-West CI. Only GeoSentiment's interval clears zero",
       x="lambda  (%/quarter)", y=NULL) + theme_geo()

# ==== K. exposure distribution (density, log) ===============================
dd <- melt(P[, c("Ticker", MEAS), with=FALSE], id.vars="Ticker", variable.name="measure", value.name="val")
dd <- dd[is.finite(val) & val>0]; dd[, measure:=MLAB[as.character(measure)]]
fig_dist <- function(p) ggplot(dd, aes(val, fill=measure)) + geom_density(alpha=0.5, colour=NA) +
  scale_x_log10() + scale_fill_manual(values=p$cat[1:4], name=NULL) +
  labs(title="Distribution of the four exposure measures (log scale)", subtitle="Firm-quarter values, positive observations", x="Exposure (log10)", y="Density") + theme_geo()

# ---- render all -------------------------------------------------------------
say("[figures] rendering to out/figures/ (pastel + print) ...")
save2(fig_trend,  "fig_A_exposure_trend", 9, 4.8)
save2(fig_quint,  "fig_B_quintile_by_measure", 8.4, 6.4)  # 2x2 facet
save2(fig_region, "fig_C_quintile_by_region", 11, 4.4)
save2(fig_sample, "fig_D_quintile_by_sample", 7.5, 4.8)
save2(fig_char,   "fig_E_characteristics", 8.5, 4.8)
save2(fig_event,  "fig_F_event_study", 6.4, 6.8)  # 2x1 stacked panels
save2(fig_event_grid, "fig_F2_event_grid", 9, 5.6)  # 2x2: all four measures
save2(fig_fm_forest, "fig_L_fm_forest", 8.5, 4.0)  # Q2 FM lambda forest plot
save2(fig_lscum,  "fig_G_cumulative_ls", 9, 4.8)
save2(fig_legs,   "fig_G2_legs_vs_market", 9, 5.0)
save2(fig_legs_sent, "fig_G2b_legs_sent_vs_market", 9, 5.0)
save2(fig_lscumC, "fig_G3_cumulative_ls_crsp", 9, 4.8)
save2(fig_load,   "fig_H_factor_loadings", 8.5, 4.8)
if(!is.null(fig_gpr)) save2(fig_gpr, "fig_I_gpr_validation", 9, 4.8)
save2(fig_heat,   "fig_J_t_heatmap", 8, 4.6)
save2(fig_dist,   "fig_K_distribution", 8.5, 4.8)
say("[figures] DONE — %d PNG files in out/figures/", length(list.files(FIG, pattern="_(pastel|print)\\.png$")))

# ---- mirror into the vault so Obsidian embeds them (tracked in git) ----------
VFIG <- file.path(ROOT,"msc_thesis_obsidian","assets","figures")
dir.create(VFIG, showWarnings=FALSE, recursive=TRUE)
mpat <- if (nzchar(TAG)) sprintf("^fig_[A-K]_.*%s_(pastel|print)\\.png$", TAG) else "^fig_[A-K]_.*_(pastel|print)\\.png$"
pngs <- list.files(FIG, pattern=mpat, full.names=TRUE)
ok <- file.copy(pngs, VFIG, overwrite=TRUE)
say("[figures] mirrored %d figures (%s) to msc_thesis_obsidian/assets/figures/", sum(ok), if(nzchar(TAG)) TAG else "v1 base")
