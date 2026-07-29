# ==============================================================================
# pipeline/R/06c_region_benchmarks.R   (Stage 6 — REGION-MATCHED benchmarks)
#
# International-benchmark robustness, FF2012-faithful. The headline keeps US FF5 (correct
# for US firms); this adds, for the LSEG-GLOBAL sample, the academically preferred
# REGION-MATCHED Fama-French factors (Griffin 2002; Fama-French 2012: factors are
# local/regional, not global). Each firm is benchmarked to its OWN region's FF5:
#   - per-region GeoRisk Q5-Q1 FF5 alpha (NA / Europe / Japan / AsiaPac-ex-Japan / EM);
#   - a region-NEUTRAL global L/S vs region-matched factors (one headline number);
#   - GLOBAL cross-check (Developed FF5) — the integration view, expected weaker;
#   - within-region Fama-MacBeth (GeoRisk + GeoSentiment).
# Ken French regional 5-factor sets (USD, monthly) in data/inputs/macro/ff_regional/.
#
#   Rscript pipeline/R/06c_region_benchmarks.R
# Output: out/analysis/region_benchmarks.json + out/figures/fig_L_region_benchmarks_{pastel,print}.png
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate); library(ggplot2) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); FIG <- file.path(ROOT,"out","figures"); FFR <- file.path(ROOT,"data/inputs/macro/ff_regional")
say <- function(...) cat(sprintf(...),"\n"); NWm <- 6L
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

qbin <- function(x) as.integer(ceiling(frank(x, ties.method=TIEM)/length(x)*5))
csz  <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

# ---- read a Ken French regional 5-factor file (monthly block) ---------------
read_ff <- function(file){ p<-file.path(FFR,file); if(!file.exists(p)) return(NULL)
  r <- suppressWarnings(fread(p, skip=6, header=TRUE, fill=TRUE, blank.lines.skip=TRUE))
  setnames(r, 1, "ym"); r <- r[grepl("^[0-9]{6}$", trimws(ym))]
  r[, Month := floor_date(as.Date(paste0(trimws(ym),"01"),"%Y%m%d"),"month")]
  for(c in c("Mkt-RF","SMB","HML","RMW","CMA","RF")) r[, (c) := as.numeric(trimws(get(c)))/100]
  setnames(r, "Mkt-RF", "MktRF"); r[, .(Month, MktRF, SMB, HML, RMW, CMA, RF)] }
FFREG <- list(NorthAmerica=read_ff("North_America_5_Factors.csv"), Europe=read_ff("Europe_5_Factors.csv"),
              Japan=read_ff("Japan_5_Factors.csv"), AsiaPacExJapan=read_ff("Asia_Pacific_ex_Japan_5_Factors.csv"),
              Emerging=read_ff("Emerging_5_Factors.csv"))
DEV <- read_ff("Developed_5_Factors.csv")
say("[ff] regional sets loaded: %s | Developed: %s rows", paste(names(FFREG),collapse=", "), if(is.null(DEV)) "NA" else nrow(DEV))

# ---- firm -> French region (REGION_MAP + Country_Clean for the Japan split) --
rmap <- as.data.table(readRDS(file.path(ROOT,"data/processed/REGION_MAP.rds")))
nb <- function(x) gsub("^[^A-Z0-9]+","", toupper(sub("\\..*$","",x)))
rmap[, fr := fifelse(Region=="NorthAmerica","NorthAmerica",
              fifelse(Region=="Europe","Europe",
              fifelse(Region=="EM","Emerging",
              fifelse(Region=="AsiaPacific_DM", fifelse(Country_Clean=="Japan","Japan","AsiaPacExJapan"), NA_character_))))]
rmap_full <- unique(rmap[!is.na(fr), .(Ticker = toupper(trimws(Ticker)), fr)], by="Ticker")  # V2: keyed on FULL RIC
rmap[, base := nb(Ticker)]; rmap <- unique(rmap[!is.na(fr) & !is.na(base) & base!="", .(base, fr)], by="base")

# ---- monthly LSEG panel + region tag ---------------------------------------
PM <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric_monthly.rds")))); PM[, base := nb(Ticker)]
PM <- if (V2) rmap_full[PM, on="Ticker"] else rmap[PM, on="base"]; PM[, Month := as.Date(Month)]
say("[map] firm-months with a French region: %.1f%%", 100*mean(!is.na(PM$fr)))

# ---- helpers: region L/S series + FF5 alpha ---------------------------------
ls_region <- function(frname){ d <- PM[fr==frname & is.finite(GeoRisk) & is.finite(RetM_w)]
  if(uniqueN(d$Month) < 24) return(NULL)
  d[, q := qbin(GeoRisk), by=Month]
  w <- dcast(d[, .(ew=mean(RetM_w)), by=.(Month,q)], Month~q, value.var="ew")
  if(!all(c("1","5") %in% names(w))) return(NULL)
  w[, LS := get("5")-get("1")]; w[is.finite(LS), .(Month, LS)] }
ff5_alpha <- function(ls, ff){ d<-merge(ls, ff, by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]; if(nrow(d)<24) return(c(alpha=NA,t=NA,n=nrow(d)))
  m<-lm(LS~MktRF+SMB+HML+RMW+CMA, d); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NWm,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
  c(alpha=unname(coef(m)[1]), t=unname(coef(m)[1]/se), n=nrow(d)) }

# ---- 1. per-region GeoRisk L/S vs region-matched FF5 -------------------------
rlab <- c(NorthAmerica="North America", Europe="Europe", Japan="Japan", AsiaPacExJapan="Asia-Pacific ex-Japan", Emerging="Emerging")
per_region <- list(); lsmat <- list()
for(fr in names(FFREG)){ ls<-ls_region(fr)
  if(is.null(ls)||is.null(FFREG[[fr]])){ per_region[[fr]]<-list(alpha=NA,t=NA,n=0); next }
  a<-ff5_alpha(ls, FFREG[[fr]]); per_region[[fr]]<-list(alpha=unname(a["alpha"]), t=unname(a["t"]), n=unname(a["n"]))
  lsmat[[fr]] <- merge(ls, FFREG[[fr]], by="Month")[, .(Month, region=fr, LS, MktRF,SMB,HML,RMW,CMA)] }
say("[1] per-region GeoRisk L/S FF5 alpha (region-matched):")
for(fr in names(per_region)) say("    %-22s alpha=%6.3f%%/m  t=%5.2f  (n=%d)", rlab[fr], 100*per_region[[fr]]$alpha, per_region[[fr]]$t, per_region[[fr]]$n)

# ---- 2. region-NEUTRAL global L/S vs region-matched factors -----------------
stack <- rbindlist(lsmat)
if (V2) { fm <- stack[, uniqueN(region), by=Month][V1 == length(lsmat)]$Month; stack <- stack[Month %in% fm] }  # V2: fixed composition
comp <- stack[, .(LS=mean(LS), MktRF=mean(MktRF), SMB=mean(SMB), HML=mean(HML), RMW=mean(RMW), CMA=mean(CMA)), by=Month]
mc <- lm(LS~MktRF+SMB+HML+RMW+CMA, comp); sec<-if(have_nw) sqrt(sandwich::NeweyWest(mc,lag=NWm,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(mc)))[1]
region_matched <- list(alpha=unname(coef(mc)[1]), t=unname(coef(mc)[1]/sec), n=nrow(comp))
say("[2] region-NEUTRAL global L/S vs region-matched FF5: alpha=%.3f%%/m t=%.2f (~%.1f%%/yr, n=%d months)",
    100*region_matched$alpha, region_matched$t, 100*((1+region_matched$alpha)^12-1), region_matched$n)

# ---- 3. GLOBAL cross-check (Developed FF5) on the global L/S ----------------
glob <- { d<-PM[is.finite(GeoRisk)&is.finite(RetM_w)]; d[, q:=qbin(GeoRisk), by=Month]
  w<-dcast(d[,.(ew=mean(RetM_w)),by=.(Month,q)],Month~q,value.var="ew"); w[,LS:=get("5")-get("1")]; w[is.finite(LS),.(Month,LS)] }
global_dev <- if(is.null(DEV)) list(alpha=NA,t=NA,n=0) else { a<-ff5_alpha(glob, DEV); list(alpha=unname(a["alpha"]),t=unname(a["t"]),n=unname(a["n"])) }
say("[3] GLOBAL cross-check (Developed FF5) on global GeoRisk L/S: alpha=%.3f%%/m t=%.2f", 100*global_dev$alpha, global_dev$t)

# ---- 4. within-region Fama-MacBeth (quarterly) -----------------------------
PQ <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds")))); PQ[, base:=nb(Ticker)]; PQ <- if (V2) rmap_full[PQ, on="Ticker"] else rmap[PQ, on="base"]
fm_region <- function(meas, frname){ d<-PQ[fr==frname & is.finite(get(meas)) & is.finite(Ret_lead) & is.finite(Size)]
  if(uniqueN(d$Quarter)<20) return(c(lambda=NA,t=NA))
  d[, ms:=csz(get(meas)), by=Quarter]; d[, sz:=csz(Size), by=Quarter]
  lam <- d[, { x<-.SD[complete.cases(.SD)]; if(nrow(x)<20) NA_real_ else coef(lm(Ret_lead~ms+sz,x))["ms"] }, by=Quarter, .SDcols=c("Ret_lead","ms","sz")]$V1
  lam<-lam[is.finite(lam)]; if(length(lam)<8) return(c(lambda=NA,t=NA)); m<-lm(lam~1)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=4,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]
  c(lambda=mean(lam), t=mean(lam)/se) }
fm <- list()
for(meas in c("GeoRisk","GeoSentiment")) fm[[meas]] <- setNames(lapply(names(FFREG), function(fr){ a<-fm_region(meas,fr); list(lambda=unname(a["lambda"]),t=unname(a["t"])) }), names(FFREG))
say("[4] within-region Fama-MacBeth (lambda %%/q, t):")
for(meas in names(fm)) for(fr in names(fm[[meas]])) say("    %-11s %-22s lambda=%6.3f%%  t=%5.2f", meas, rlab[fr], 100*fm[[meas]][[fr]]$lambda, fm[[meas]][[fr]]$t)

# ---- 5. write json ----------------------------------------------------------
out <- list(method="Region-matched Fama-French (Griffin 2002; Fama-French 2012)",
            per_region=per_region, region_matched_global=region_matched, global_developed=global_dev,
            within_region_fm=fm, region_labels=as.list(rlab))
write_json(out, vS(file.path(ANA,"region_benchmarks.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("[5] wrote out/analysis/region_benchmarks.json")

# ---- 6. figure: region-matched alpha bar (both palettes) -------------------
PAL <- list(pastel=list(pos="#3d9c86", neg="#E8998D", us="#7FB3D5"),
            print =list(pos="grey30",  neg="grey62",  us="grey78"))
theme_geo <- function() theme_minimal(base_size=13) + theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold",size=13), plot.subtitle=element_text(size=10,colour="grey30"), legend.position="none")
us_alpha <- tryCatch(fromJSON(file.path(ANA,"crsp_monthly.json"))$q3, error=function(e)NULL)
us_val <- if(!is.null(us_alpha)) us_alpha$ew_alpha[us_alpha$measure=="GeoRisk"] else NA
us_t   <- if(!is.null(us_alpha)) us_alpha$ew_t[us_alpha$measure=="GeoRisk"] else NA
bar <- rbindlist(list(
  data.table(grp="US (FF5, headline)",        a=us_val,                  t=us_t,                   kind="us"),
  rbindlist(lapply(names(per_region), function(fr) data.table(grp=rlab[fr], a=per_region[[fr]]$alpha, t=per_region[[fr]]$t, kind="reg"))),
  data.table(grp="Region-neutral (matched)",  a=region_matched$alpha,    t=region_matched$t,       kind="reg"),
  data.table(grp="Global (Developed FF5)",    a=global_dev$alpha,        t=global_dev$t,           kind="glob")))
bar <- bar[is.finite(a)]; bar[, ann := 100*((1+a)^12-1)]; bar[, grp := factor(grp, levels=grp)]
fig <- function(p){ bar[, fill := fifelse(kind=="us", p$us, fifelse(t>=1.96, p$pos, p$neg))]
  ggplot(bar, aes(grp, ann, fill=I(fill))) + geom_col(width=0.74) + geom_hline(yintercept=0,colour="grey60") +
    geom_text(aes(label=sprintf("t=%.1f",t)), vjust=ifelse(bar$ann>=0,-0.4,1.2), size=3.4, colour="grey20") +
    labs(title="GeoRisk long-short FF5 alpha under region-matched benchmarks",
         subtitle="Annualised FF5 alpha; US FF5 = headline (blue/grey). Region-matched = Griffin 2002 / Fama-French 2012",
         x=NULL, y="FF5 alpha (% per year)") + theme_geo() +
    theme(axis.text.x=element_text(angle=25, hjust=1)) }
dir.create(FIG, showWarnings=FALSE, recursive=TRUE)
for(pl in names(PAL)) ggsave(file.path(FIG, sprintf("fig_L_region_benchmarks_%s.png",pl)), fig(PAL[[pl]]), width=9, height=5, dpi=300, bg="white")
VFIG <- file.path(ROOT,"msc_thesis_obsidian","assets","figures"); dir.create(VFIG,showWarnings=FALSE,recursive=TRUE)
file.copy(list.files(FIG,pattern="^fig_L_region_benchmarks_(pastel|print)\\.png$",full.names=TRUE), VFIG, overwrite=TRUE)
say("[6] wrote fig_L_region_benchmarks_{pastel,print}.png (+ vault mirror). DONE.")
