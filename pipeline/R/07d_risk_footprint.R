# ==============================================================================
# pipeline/R/07d_risk_footprint.R   (Stage 6c — daily IVOL window, risk footprint, MT haircut)
#
# Three defense-interrogation tests in one script:
#  (a) DAILY call-window realized vol: does volatility actually SPIKE at the call (the
#      quarterly IVOL "spike" the proposal predicted is absent, all |t|<1.6)? Realized vol
#      in [-1,+5] vs a [-20,-6] baseline, by dGeoExposure quintile. Reuses the 05n event anchor.
#  (b) RISK FOOTPRINT: does GeoRisk carry systematic-risk loadings (-> risk premium) or not
#      (-> mispricing)? Regress Beta_CAPM / downside beta / coskewness on GeoRisk + controls.
#  (c) MULTIPLE-TESTING haircut: BH-FDR + Harvey-Liu-Zhu (t>3.0) across the RESULTS_MASTER
#      headline t-grid -> which results survive.
#
#   Rscript pipeline/R/07d_risk_footprint.R   ->  out/analysis/risk_footprint.json + figs
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(jsonlite); library(fixest); library(ggplot2) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT, if (nzchar(Sys.getenv("GEOV2"))||identical(Sys.getenv("GEO_EXPO"),"v2")) "out/exposure_v2" else "out/exposure"); FIG <- file.path(ROOT,"out","figures")
dir.create(FIG, showWarnings=FALSE, recursive=TRUE); say <- function(...) cat(sprintf(...),"\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}
PAL <- list(pastel=c(lo="#6FA8DC",hi="#E8998D",bar="#3d9c86"), print=c(lo="grey65",hi="black",bar="grey35"))
theme_geo <- function() theme_minimal(base_size=13)+theme(panel.grid.minor=element_blank(),plot.title=element_text(face="bold",size=13),plot.subtitle=element_text(size=10,colour="grey30"),legend.position="bottom")

# ============================================================================
# (a) DAILY call-window realized vol by dGeoExposure quintile
# ============================================================================
ev <- as.data.table(readRDS(file.path(EXP,"exposure_calls_dedup.rds")))
ev <- ev[!is.na(ric) & !is.na(date)]; ev[, edate := as.Date(date)]; ev[, eid := .I]
setorder(ev, ric, year, quarter); ev[, qidx := year*4+quarter]
ev[, dCC := { dv<-GeoExposure-shift(GeoExposure); dq<-qidx-shift(qidx); ifelse(dq==1L,dv,NA_real_) }, by=ric]
ev <- ev[is.finite(dCC)]; ev[, dq5 := as.integer(ceiling(frank(dCC,ties.method=TIEM)/.N*5)), by=.(year,quarter)]

tk <- unique(ev$ric)
dt <- as.data.table(open_dataset(file.path(ROOT,"data/processed/LSEG_Final_Panel.parquet")) |>
        dplyr::select(Date,Ticker,Ret) |> dplyr::filter(Ticker %in% tk) |> dplyr::collect())
dt[, Date:=as.Date(Date)]; dt <- dt[is.finite(Ret)]
ff3 <- fread(file.path(ROOT,"data/inputs/ff3_factors_daily.csv")); ff3[, Date:=as.Date(Date)]; ff3[, Rm:=MktRF+RF]
dt <- ff3[,.(Date,Rm)][dt,on="Date"]; dt[, AR:=Ret-Rm]
setorder(dt,Ticker,Date); dt[, ti:=seq_len(.N), by=Ticker]
setkey(dt,Ticker,Date)
t0 <- dt[ev[,.(Ticker=ric,Date=edate,eid)], on=.(Ticker,Date), roll=-Inf, .(eid,Ticker,t0=ti)][!is.na(t0)]
# realized abnormal-return vol in two windows per event, via per-ticker index lookups
setkey(dt,Ticker,ti)
win_sd <- function(lo,hi){ ks<-lo:hi; n0<-nrow(t0)
  g <- t0[rep(seq_len(n0),each=length(ks)),.(eid,Ticker,t0)]; g[, ti:=t0+rep(ks,times=n0)]
  g <- dt[,.(Ticker,ti,AR)][g,on=.(Ticker,ti)]
  g[is.finite(AR), .(sd=sd(AR), n=.N), by=eid][n>=3] }
pre  <- win_sd(-20,-6); setnames(pre,"sd","sd_pre")
post <- win_sd(-1, 5);  setnames(post,"sd","sd_evt")
vol <- merge(pre[,.(eid,sd_pre)], post[,.(eid,sd_evt)], by="eid")
vol <- ev[,.(eid,dq5)][vol,on="eid"]
vol[, ratio := sd_evt/sd_pre]
volq <- vol[is.finite(ratio) & ratio<10, .(mean_ratio=mean(ratio), med_ratio=median(ratio), n=.N), by=dq5][order(dq5)]
# Q5-Q1 difference in the event/pre vol ratio
tt <- tryCatch(t.test(vol[dq5==5 & is.finite(ratio) & (!V2 | ratio<10), ratio], vol[dq5==1 & is.finite(ratio) & (!V2 | ratio<10), ratio]), error=function(e)NULL)
ivol_spike <- list(by_quintile=volq, q5_minus_q1=if(is.null(tt)) NA else unname(diff(rev(tt$estimate))), t=if(is.null(tt)) NA else unname(tt$statistic),
                   note="event window [-1,+5] realized abnormal-return vol / pre-window [-20,-6] baseline")
say("[a] daily call-window vol ratio (event[-1,+5]/pre[-20,-6]) by dGeoExposure quintile:")
print(volq); say("    Q5-Q1 ratio diff t=%.2f", ivol_spike$t)

# ============================================================================
# (b) RISK FOOTPRINT: does GeoRisk carry systematic-risk loadings?
# ============================================================================
P <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds"))))
# downside beta + coskewness from daily (market down-days), per firm over full sample
dd <- merge(dt[,.(Ticker,Date,Ret)], ff3[,.(Date,MktRF,RF)], by="Date")
dd[, exret:=Ret-RF]; setorder(dd,Ticker,Date)
mkt_mean <- dd[, mean(MktRF,na.rm=TRUE)]
beta_tbl <- dd[, { n<-.N
  if(n<120) list(dbeta=NA_real_, coskew=NA_real_) else {
    down <- MktRF < mkt_mean
    db <- if(sum(down)>30) cov(exret[down],MktRF[down])/var(MktRF[down]) else NA_real_
    mc <- MktRF-mean(MktRF); ec<-exret-mean(exret)
    ck <- mean(ec*mc^2)/(sd(exret)*var(MktRF))
    list(dbeta=db, coskew=ck) } }, by=Ticker]
P2 <- beta_tbl[P, on="Ticker"]
fp <- list()
for(dvar in c("Beta_CAPM","dbeta","coskew")){ if(!dvar %in% names(P2)) next
  P2[, y := get(dvar)]; P2[, cz := csz(GeoRisk), by=Quarter]
  m <- tryCatch(feols(y ~ cz + Size + Momentum | Quarter, P2[is.finite(y)&is.finite(cz)], vcov=~Ticker), error=function(e)NULL)
  fp[[dvar]] <- if(is.null(m)) list(coef=NA,t=NA) else list(coef=unname(coef(m)["cz"]), t=unname(coef(m)["cz"]/se(m)["cz"]))
}
say("\n[b] risk footprint — GeoRisk -> systematic-risk measures (coef per +1 SD, quarter FE, cluster firm):")
for(k in names(fp)) say("    %-10s coef=%+.4f  t=%+.2f", k, fp[[k]]$coef, fp[[k]]$t)

# ============================================================================
# (c) MULTIPLE-TESTING haircut across the RESULTS_MASTER headline t-grid
# ============================================================================
H <- fread(vS(file.path(ANA,"RESULTS_MASTER.csv")))   # version-tagged master (min/v11)
# focus on the primary inferential tests (exclude per-region descriptive + robustness sub-cells)
core <- H[question %in% c("Q1_realization","Q1_ivol","Q2_pricing_FM","Q2_pricing_FE","Q3_LS_FF5","Q3_augmented_spanning") & is.finite(t)]
core[, p := 2*pnorm(-abs(t))]
core[, p_bh := p.adjust(p, method="BH")]
core[, survive_bh := p_bh < 0.05]
core[, survive_hlz := abs(t) > 3.0]      # Harvey-Liu-Zhu suggested threshold for "new factors"
mt <- list(n_tests=nrow(core), n_raw_sig=sum(core$p<0.05), n_bh_sig=sum(core$survive_bh), n_hlz_sig=sum(core$survive_hlz),
           survivors_hlz = core[survive_hlz==TRUE, .(question,measure,sample,freq,t=round(t,2))],
           survivors_bh  = core[survive_bh==TRUE,  .(question,measure,sample,freq,t=round(t,2),p_bh=round(p_bh,4))])
say("\n[c] multiple-testing across %d core tests: raw p<.05 = %d | BH-FDR<.05 = %d | HLZ |t|>3 = %d",
    mt$n_tests, mt$n_raw_sig, mt$n_bh_sig, mt$n_hlz_sig)
say("    HLZ survivors (|t|>3):"); print(mt$survivors_hlz)

# ============================================================================
# write + figures
# ============================================================================
out <- list(ivol_spike=ivol_spike, risk_footprint=fp, multiple_testing=mt)
write_json(out, vS(file.path(ANA,"risk_footprint.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("\n  wrote out/analysis/risk_footprint.json")

# fig M: daily vol ratio by dGeoExposure quintile
volq[, Q:=factor(paste0("Q",dq5),levels=paste0("Q",1:5))]
for(pl in names(PAL)){ p<-PAL[[pl]]
  g <- ggplot(volq, aes(Q, mean_ratio)) + geom_col(width=0.74, fill=p["bar"]) + geom_hline(yintercept=1,colour="grey55",linetype="dashed") +
    labs(title="Daily realized-vol ratio around the call, by change-in-exposure quintile",
         subtitle="Abnormal-return vol in [-1,+5] / pre-window [-20,-6]; >1 = volatility rises at the call",
         x="dGeoExposure quintile (Q5 = biggest rise in geo-talk)", y="Event / pre vol ratio") + theme_geo()
  ggsave(file.path(FIG, sprintf("fig_M_callwindow_vol_%s.png",pl)), g, width=8.5, height=4.8, dpi=300, bg="white") }
file.copy(list.files(FIG,pattern="^fig_M_callwindow_vol_(pastel|print)\\.png$",full.names=TRUE),
          file.path(ROOT,"msc_thesis_obsidian","assets","figures"), overwrite=TRUE)
say("  wrote fig_M_callwindow_vol_{pastel,print}.png. DONE.")
