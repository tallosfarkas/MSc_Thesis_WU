# ==============================================================================
# pipeline/R/07c_source_reconcile.R   (Stage 6c — why does the return source flip it? + net-of-cost)
#
# CRSP confirms the GeoRisk L/S (t=2.76) but the LSEG version is marginal; for Q2 Sentiment
# it is the reverse. This builds the GeoRisk Q5-Q1 monthly L/S on BOTH return sources for the
# SAME US firms (permno<->ric crosswalk from the dedup), and decomposes the gap:
#   - correlation + mean difference of the two L/S series,
#   - per-leg return difference (does CRSP's delisting adjustment lift the short/long leg?),
#   - alpha on each source for the identical firm set.
# Then NET-OF-COST: portfolio turnover + a cost grid (10/25/50 bps one-way) + break-even cost.
#
#   Rscript pipeline/R/07c_source_reconcile.R   ->  out/analysis/source_reconcile.json + fig
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate); library(ggplot2) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT, if (nzchar(Sys.getenv("GEOV2"))||identical(Sys.getenv("GEO_EXPO"),"v2")) "out/exposure_v2" else "out/exposure"); FIG <- file.path(ROOT,"out","figures")
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
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv"))[, .(Month=floor_date(as.Date(Date),"month"), MktRF,SMB,HML,RMW,CMA,RF)]
alpha_of <- function(ls){ d<-merge(ls,ff5,by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]; m<-lm(LS~MktRF+SMB+HML+RMW+CMA,d)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]; c(alpha=unname(coef(m)[1]),t=unname(coef(m)[1]/se),n=nrow(d)) }

# ---- same US firms on both sources: permno<->ric crosswalk -------------------
xw <- unique(as.data.table(readRDS(file.path(EXP,"exposure_calls_dedup.rds")))[!is.na(ric)&!is.na(permno)&us_can%in%TRUE, .(ric,permno)])
xw[, permno:=as.integer(permno)]; xw <- xw[, .SD[1], by=permno][, .SD[1], by=ric]   # 1-1
fq <- as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); fq[,permno:=as.integer(permno)]
fq[, form_q:=as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
hm <- fq[rep(seq_len(.N),each=3L)]; hm[,kk:=rep(0:2,times=nrow(fq))]; hm[,Month:=form_q %m+% months(3L+kk)]
hm <- xw[hm, on="permno"][!is.na(ric)]
# returns: CRSP (permno) + LSEG (ric)
crsp <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); crsp[,permno:=as.integer(permno)]
lseg <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric_monthly.rds"))))[, .(ric=Ticker, Month=as.Date(Month), RetM_lseg=RetM_w)]
P <- merge(hm[,.(permno,ric,Month,GeoRisk)], crsp[,.(permno,Month,RetM_crsp=RetM)], by=c("permno","Month"))
P <- merge(P, lseg, by=c("ric","Month"))
P <- P[is.finite(RetM_crsp) & is.finite(RetM_lseg) & is.finite(GeoRisk)]
for(c in c("RetM_crsp","RetM_lseg")) P[, (c):=pmin(pmax(get(c),quantile(get(c),.005,na.rm=T)),quantile(get(c),.995,na.rm=T)), by=Month]
say("[recon] same-firm panel (both sources): %s firm-months | %s firms | %d months",
    format(nrow(P),big.mark=","), format(uniqueN(P$permno),big.mark=","), uniqueN(P$Month))

# ---- L/S on each source for the identical firm set --------------------------
P[, q := qbin(GeoRisk), by=Month]
mk_ls <- function(retc){ w<-dcast(P[,.(r=mean(get(retc))),by=.(Month,q)],Month~q,value.var="r"); w[,LS:=get("5")-get("1")]; w[is.finite(LS),.(Month,LS)] }
ls_c <- mk_ls("RetM_crsp"); ls_l <- mk_ls("RetM_lseg")
ac <- alpha_of(ls_c); al <- alpha_of(ls_l)
both <- merge(ls_c[,.(Month,LS_crsp=LS)], ls_l[,.(Month,LS_lseg=LS)], by="Month")
rho <- cor(both$LS_crsp, both$LS_lseg)
# per-leg means by source
legs <- P[, .(crsp=mean(RetM_crsp), lseg=mean(RetM_lseg)), by=q][order(q)]
say("[recon] SAME firms: CRSP L/S alpha=%.3f%%/m t=%.2f | LSEG L/S alpha=%.3f%%/m t=%.2f | corr(L/S)=%.2f",
    100*ac["alpha"],ac["t"],100*al["alpha"],al["t"],rho)
say("  per-quintile mean monthly return by source (Q1..Q5):")
say("    CRSP: %s", paste(sprintf("%.2f",100*legs$crsp),collapse=" "))
say("    LSEG: %s", paste(sprintf("%.2f",100*legs$lseg),collapse=" "))
say("  mean Q1(short-leg) CRSP %.3f%% vs LSEG %.3f%%  -> delisting-adjustment lifts/depresses the short leg",
    100*legs$crsp[1], 100*legs$lseg[1])

# ---- net-of-cost on the headline CRSP GeoRisk L/S ---------------------------
# turnover: fraction of the long (Q5) + short (Q1) legs that rotate each month
P2 <- as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); P2[,permno:=as.integer(permno)]
P2[, form_q:=as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
hm2 <- P2[rep(seq_len(.N),each=3L)]; hm2[,kk:=rep(0:2,times=nrow(P2))]; hm2[,Month:=form_q %m+% months(3L+kk)]
cr <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); cr[,permno:=as.integer(permno)]
H <- merge(hm2, cr[,.(permno,Month,RetM)], by=c("permno","Month")); H<-H[is.finite(RetM)&is.finite(GeoRisk)]
H[, q:=qbin(GeoRisk), by=Month]
mem <- H[q %in% c(1,5), .(permno,Month,leg=ifelse(q==5,"L","S"))]
setorder(mem,leg,Month,permno)
turn <- mem[, { ms<-sort(unique(Month)); to<-numeric(0)
  for(i in 2:length(ms)){ a<-permno[Month==ms[i-1]]; b<-permno[Month==ms[i]]; to<-c(to, length(setdiff(b,a))/max(length(b),1)) }
  .(mean_oneway_turnover=mean(to)) }, by=leg]
avg_turn <- mean(turn$mean_oneway_turnover)        # one-way, per month, per leg
gross_ann <- (1+ac["alpha"])^12-1
say("\n[cost] one-way monthly turnover: long %.0f%% / short %.0f%% (avg %.0f%%)",
    100*turn$mean_oneway_turnover[turn$leg=="L"], 100*turn$mean_oneway_turnover[turn$leg=="S"], 100*avg_turn)
costs <- c(10,25,50)/1e4
netgrid <- lapply(costs, function(cps){ drag_m <- 2*avg_turn*cps; net_m <- ac["alpha"]-drag_m; list(cost_bps=cps*1e4, net_alpha_m=unname(net_m), net_ann=unname((1+net_m)^12-1)) })
breakeven_bps <- unname(ac["alpha"]/(2*avg_turn))*1e4
say("[cost] gross alpha %.3f%%/m (~%.1f%%/yr). Net of cost (both legs, monthly rebal):", 100*ac["alpha"], 100*gross_ann)
for(g in netgrid) say("    @%d bps -> net %.3f%%/m (~%.1f%%/yr)", g$cost_bps, 100*g$net_alpha_m, 100*g$net_ann)
say("[cost] break-even one-way cost ~ %.0f bps", breakeven_bps)

out <- list(same_firm=list(n_firms=uniqueN(P$permno), n_months=uniqueN(P$Month),
              crsp_alpha=unname(ac["alpha"]), crsp_t=unname(ac["t"]), lseg_alpha=unname(al["alpha"]), lseg_t=unname(al["t"]),
              corr_ls=rho, quintile_means_crsp=legs$crsp, quintile_means_lseg=legs$lseg),
            net_of_cost=list(oneway_turnover_avg=avg_turn, turnover_by_leg=turn, gross_alpha_m=unname(ac["alpha"]),
              gross_ann=unname(gross_ann), grid=netgrid, breakeven_oneway_bps=breakeven_bps))
write_json(out, vS(file.path(ANA,"source_reconcile.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("\n  wrote out/analysis/source_reconcile.json")

# figure: same-firm L/S alpha by source + net-of-cost
d1 <- data.table(kind="By return source (same US firms)", x=c("CRSP","LSEG"),
                 ann=c(100*((1+ac["alpha"])^12-1),100*((1+al["alpha"])^12-1)), t=c(ac["t"],al["t"]))
d2 <- rbindlist(lapply(netgrid, function(g) data.table(kind="CRSP alpha net of cost", x=sprintf("%d bps",g$cost_bps), ann=100*g$net_ann, t=NA)))
d2 <- rbind(data.table(kind="CRSP alpha net of cost", x="gross", ann=100*gross_ann, t=ac["t"]), d2)
bd <- rbind(d1,d2)
PAL<-list(pastel=c("#7FB3D5","#3d9c86"),print=c("grey60","grey25"))
for(pl in names(PAL)){ p<-PAL[[pl]]
  g<-ggplot(bd,aes(x,ann,fill=kind))+geom_col(width=0.7)+facet_wrap(~kind,scales="free_x")+geom_hline(yintercept=0,colour="grey60")+
    geom_text(aes(label=ifelse(is.na(t),"",sprintf("t=%.1f",t))),vjust=-0.4,size=3.2,colour="grey20")+
    scale_fill_manual(values=p,guide="none")+
    labs(title="Return-source reconciliation and net-of-cost (GeoRisk long-short)",
         subtitle="Left: identical US firms, CRSP vs LSEG returns. Right: CRSP alpha gross vs net of one-way cost",x=NULL,y="alpha (% per year)")+
    theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),plot.title=element_text(face="bold",size=12),plot.subtitle=element_text(size=9,colour="grey30"))
  ggsave(file.path(FIG,sprintf("fig_O_source_cost_%s.png",pl)),g,width=9,height=4.6,dpi=300,bg="white") }
file.copy(list.files(FIG,pattern="^fig_O_source_cost_(pastel|print)\\.png$",full.names=TRUE), file.path(ROOT,"msc_thesis_obsidian","assets","figures"), overwrite=TRUE)
say("  wrote fig_O_source_cost_{pastel,print}.png. DONE.")
