# ==============================================================================
# pipeline/R/07b_extra_factors.R   (Stage 6c — is the L/S a known factor?)
#
# The GeoRisk long-short is EW + small-cap-tilted, so the obvious confounds are
# illiquidity, betting-against-beta, and quality. This extends the augmented spanning
# (05q) of the GeoRisk Q5-Q1 with:
#   - Amihud ILLIQUIDITY factor (built from the daily LSEG panel: mean |Ret|/(Price*Volume),
#     monthly high-minus-low EW),
#   - SHORT-TERM REVERSAL factor (prior-month return, loser-minus-winner, EW),
#   - AQR BAB + QMJ (USA, from the downloaded AQR data sets).
# Run on both the CRSP headline L/S (t=2.76) and the LSEG L/S. If the alpha survives, the
# premium is not a repackaged illiquidity/BAB/quality exposure.
#
#   Rscript pipeline/R/07b_extra_factors.R   ->  out/analysis/extra_factors.json + fig
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(jsonlite); library(lubridate); library(readxl); library(ggplot2) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT, "out", { .e <- Sys.getenv("GEO_EXPO")
    if (nzchar(Sys.getenv("GEOV2")) || identical(.e, "v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_", .e) else "exposure" }); MAC <- file.path(ROOT,"data/inputs/macro"); FIG <- file.path(ROOT,"out","figures")
say <- function(...) cat(sprintf(...),"\n"); NW <- 6L
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

qbin <- function(x) as.integer(ceiling(frank(x,ties.method=TIEM)/length(x)*5))

# ---- factor set: FF5 + UMD + BAB + QMJ + ILLIQ + REV ------------------------
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv"))[, .(Month=floor_date(as.Date(Date),"month"), MktRF,SMB,HML,RMW,CMA,RF)]
umd <- fread(file.path(MAC,"F-F_Momentum_Factor.csv"), skip=13, header=FALSE, col.names=c("ym","Mom"), fill=TRUE)
umd <- umd[grepl("^[0-9]{6}$",trimws(ym))]; umd[, Month:=floor_date(as.Date(paste0(ym,"01"),"%Y%m%d"),"month")]; umd[, UMD:=as.numeric(Mom)/100]
read_aqr <- function(f,nm){ p<-file.path(MAC,paste0(f,".xlsx")); if(!file.exists(p)) return(NULL)
  d<-suppressMessages(read_excel(p, sheet=paste0(nm," Factors"), skip=18)); setDT(d)
  d[!is.na(USA), .(Month=floor_date(as.Date(DATE,"%m/%d/%Y"),"month"), val=as.numeric(USA))] }
bab <- read_aqr("AQR_BAB","BAB"); if(!is.null(bab)) setnames(bab,"val","BAB")
qmj <- read_aqr("AQR_QMJ","QMJ"); if(!is.null(qmj)) setnames(qmj,"val","QMJ")
say("[factors] FF5+UMD loaded | BAB:%s QMJ:%s", !is.null(bab), !is.null(qmj))

# ---- build Amihud ILLIQ + REVERSAL factors from LSEG daily/monthly ----------
PM <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric_monthly.rds")))); PM[, Month:=as.Date(Month)]
# Amihud per firm-month from the daily panel
tk <- unique(PM$Ticker)
dl <- as.data.table(open_dataset(file.path(ROOT,"data/processed/LSEG_Final_Panel.parquet")) |>
        dplyr::select(Date,Ticker,Ret,Price,Volume) |> dplyr::filter(Ticker %in% tk) |> dplyr::collect())
dl[, Date:=as.Date(Date)]; dl <- dl[is.finite(Ret) & is.finite(Price) & is.finite(Volume) & Volume>0 & Price>0]
dl[, Month := floor_date(Date,"month")]; dl[, dilliq := abs(Ret)/(Price*Volume)]
amihud <- dl[, .(illiq=mean(dilliq), nd=.N), by=.(Ticker,Month)][nd>=10]
# ILLIQ factor: within month, EW high-illiq minus low-illiq (quintile)
amihud[, ib := qbin(illiq), by=Month]
am <- merge(amihud, PM[,.(Ticker,Month,RetM_w)], by=c("Ticker","Month"))
illiqf <- dcast(am[, .(r=mean(RetM_w,na.rm=TRUE)), by=.(Month,ib)], Month~ib, value.var="r")
illiqf <- illiqf[, .(Month, ILLIQ=get("5")-get("1"))][is.finite(ILLIQ)]
# REVERSAL factor: prior-month return, EW loser-minus-winner
setorder(PM, Ticker, Month); PM[, prev_r := shift(RetM_w), by=Ticker]
PM[, gap := as.integer((year(Month)*12+month(Month)) - (year(shift(Month))*12+month(shift(Month)))), by=Ticker]
PM[gap!=1L, prev_r := NA]
rv <- PM[is.finite(prev_r) & is.finite(RetM_w)]; rv[, rb := qbin(prev_r), by=Month]
revf <- dcast(rv[, .(r=mean(RetM_w)), by=.(Month,rb)], Month~rb, value.var="r")
revf <- revf[, .(Month, REV=get("1")-get("5"))][is.finite(REV)]   # losers minus winners
say("[factors] ILLIQ %d months, REV %d months built from LSEG", nrow(illiqf), nrow(revf))

F <- Reduce(function(a,b) merge(a,b,by="Month",all.x=TRUE),
            Filter(Negate(is.null), list(ff5, umd[,.(Month,UMD)], bab, qmj, illiqf, revf)))
setorder(F, Month)

# ---- GeoRisk L/S series: CRSP (headline) + LSEG -----------------------------
# 2026-07-31: the same ladder is now run for GeoSentiment as well. The GeoRisk keys
# (crsp/lseg) are left exactly as they were so nothing downstream changes; the new
# measure is written under crsp_<meas>/lseg_<meas>.
mk_crsp <- function(MEAS) { fq<-as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); fq[,permno:=as.integer(permno)]
  fq[, form_q:=as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
  hm<-fq[rep(seq_len(.N),each=3L)]; hm[,kk:=rep(0:2,times=nrow(fq))]; hm[,Month:=form_q %m+% months(3L+kk)]
  ret<-as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[,permno:=as.integer(permno)]
  p<-merge(hm,ret[,.(permno,Month,RetM)],by=c("permno","Month")); p<-p[is.finite(RetM)]
  p[, RetM:=pmin(pmax(RetM,quantile(RetM,.005,na.rm=T)),quantile(RetM,.995,na.rm=T)),by=Month]
  p<-p[is.finite(get(MEAS))]
  p[, q:=qbin(get(MEAS)),by=Month]; w<-dcast(p[,.(r=mean(RetM)),by=.(Month,q)],Month~q,value.var="r"); w[,LS:=get("5")-get("1")]; w[is.finite(LS),.(Month,LS)] }
mk_lseg <- function(MEAS) { p<-PM[is.finite(get(MEAS))&is.finite(RetM_w)]; p[,q:=qbin(get(MEAS)),by=Month]
  w<-dcast(p[,.(r=mean(RetM_w)),by=.(Month,q)],Month~q,value.var="r"); w[,LS:=get("5")-get("1")]; w[is.finite(LS),.(Month,LS)] }
ls_crsp <- mk_crsp("GeoRisk"); ls_lseg <- mk_lseg("GeoRisk")

span <- function(ls, rhs){ d<-merge(ls,F,by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]
  d<-d[complete.cases(d[,c("LS",rhs),with=FALSE])]; if(nrow(d)<24) return(c(alpha=NA,t=NA,n=nrow(d)))
  m<-lm(reformulate(rhs,"LS"),d); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
  c(alpha=unname(coef(m)[1]), t=unname(coef(m)[1]/se), n=nrow(d)) }
ff5v <- c("MktRF","SMB","HML","RMW","CMA"); have <- names(F)
models <- list(M0_FF5=ff5v, M1_FF5_UMD=c(ff5v,"UMD"),
               M2_FF5_BAB_QMJ=c(ff5v, intersect(c("BAB","QMJ"),have)),
               M3_FF5_ILLIQ_REV=c(ff5v, intersect(c("ILLIQ","REV"),have)),
               M4_all=c(ff5v, intersect(c("UMD","BAB","QMJ","ILLIQ","REV"),have)))
run <- function(ls,label){ say("\n=== %s GeoRisk L/S — extra-factor spanning (alpha %%/m, NW-t) ===", label)
  res<-lapply(models, function(r) span(ls,r))
  for(nm in names(res)) say("  %-18s alpha=%.4f%%/m (t=%.2f, n=%d)", nm, 100*res[[nm]]["alpha"], res[[nm]]["t"], res[[nm]]["n"])
  res }
out <- list(crsp = run(ls_crsp,"CRSP"), lseg = run(ls_lseg,"LSEG"),
            crsp_GeoSentiment = run(mk_crsp("GeoSentiment"),"CRSP GeoSentiment"),
            lseg_GeoSentiment = run(mk_lseg("GeoSentiment"),"LSEG GeoSentiment"),
            factors_available = intersect(c("UMD","BAB","QMJ","ILLIQ","REV"), have),
            note="ILLIQ/REV built from LSEG daily/monthly; BAB/QMJ = AQR USA series")
write_json(out, vS(file.path(ANA,"extra_factors.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("\n  wrote out/analysis/extra_factors.json")

# ---- figure: CRSP GeoRisk L/S alpha across factor models --------------------
bardt <- rbindlist(lapply(names(models), function(nm) data.table(model=nm, alpha=out$crsp[[nm]]["alpha"], t=out$crsp[[nm]]["t"])))
bardt <- bardt[is.finite(alpha)]; bardt[, ann:=100*((1+alpha)^12-1)]; lbl<-c(M0_FF5="FF5",M1_FF5_UMD="+UMD",M2_FF5_BAB_QMJ="+BAB+QMJ",M3_FF5_ILLIQ_REV="+ILLIQ+REV",M4_all="+all")
bardt[, model:=factor(lbl[model],levels=lbl)]
PAL<-list(pastel=c(sig="#3d9c86",ns="#E8998D"),print=c(sig="grey30",ns="grey65"))
for(pl in names(PAL)){ p<-PAL[[pl]]; bardt[, fill:=ifelse(abs(t)>=1.96,p["sig"],p["ns"])]
  g<-ggplot(bardt,aes(model,ann,fill=I(fill)))+geom_col(width=0.7)+geom_hline(yintercept=0,colour="grey60")+
    geom_text(aes(label=sprintf("t=%.1f",t)),vjust=-0.4,size=3.4,colour="grey20")+
    labs(title="CRSP GeoRisk long-short alpha under extra-factor spanning",
         subtitle="Annualised FF5 alpha adding momentum, BAB, QMJ, illiquidity, reversal. Bars green/dark = |t|>=1.96",
         x=NULL,y="alpha (% per year)")+theme_minimal(base_size=13)+theme(panel.grid.minor=element_blank(),plot.title=element_text(face="bold",size=13),plot.subtitle=element_text(size=9.5,colour="grey30"),legend.position="none")
  ggsave(file.path(FIG,sprintf("fig_N_extra_factors_%s.png",pl)),g,width=8.5,height=4.8,dpi=300,bg="white") }
file.copy(list.files(FIG,pattern="^fig_N_extra_factors_(pastel|print)\\.png$",full.names=TRUE), file.path(ROOT,"msc_thesis_obsidian","assets","figures"), overwrite=TRUE)
say("  wrote fig_N_extra_factors_{pastel,print}.png. DONE.")
