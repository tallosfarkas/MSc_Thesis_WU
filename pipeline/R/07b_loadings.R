# ==============================================================================
# pipeline/R/07b_loadings.R  (diagnostic companion to 07b_extra_factors.R)
#
# 07b saves only the spanning ALPHA + t. This reruns the same factor build and
# GeoRisk L/S, then prints the FULL coefficient table (loadings + NW-t) for the
# M3 (FF5+ILLIQ+REV) and M4 (all) models — to see which factor the L/S loads on
# and why the alpha falls under ILLIQ+REV alone but rises in the full model.
#
#   Rscript pipeline/R/07b_loadings.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(lubridate); library(readxl) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT,"out/exposure"); MAC <- file.path(ROOT,"data/inputs/macro")
say <- function(...) cat(sprintf(...),"\n"); NW <- 6L
TIEM <- "first"
qbin <- function(x) as.integer(ceiling(frank(x,ties.method=TIEM)/length(x)*5))

ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv"))[, .(Month=floor_date(as.Date(Date),"month"), MktRF,SMB,HML,RMW,CMA,RF)]
umd <- fread(file.path(MAC,"F-F_Momentum_Factor.csv"), skip=13, header=FALSE, col.names=c("ym","Mom"), fill=TRUE)
umd <- umd[grepl("^[0-9]{6}$",trimws(ym))]; umd[, Month:=floor_date(as.Date(paste0(ym,"01"),"%Y%m%d"),"month")]; umd[, UMD:=as.numeric(Mom)/100]
read_aqr <- function(f,nm){ p<-file.path(MAC,paste0(f,".xlsx")); if(!file.exists(p)) return(NULL)
  d<-suppressMessages(read_excel(p, sheet=paste0(nm," Factors"), skip=18)); setDT(d)
  d[!is.na(USA), .(Month=floor_date(as.Date(DATE,"%m/%d/%Y"),"month"), val=as.numeric(USA))] }
bab <- read_aqr("AQR_BAB","BAB"); if(!is.null(bab)) setnames(bab,"val","BAB")
qmj <- read_aqr("AQR_QMJ","QMJ"); if(!is.null(qmj)) setnames(qmj,"val","QMJ")

PM <- as.data.table(readRDS(file.path(ANA,"panel_ric_monthly.rds"))); PM[, Month:=as.Date(Month)]
tk <- unique(PM$Ticker)
dl <- as.data.table(open_dataset(file.path(ROOT,"data/processed/LSEG_Final_Panel.parquet")) |>
        dplyr::select(Date,Ticker,Ret,Price,Volume) |> dplyr::filter(Ticker %in% tk) |> dplyr::collect())
dl[, Date:=as.Date(Date)]; dl <- dl[is.finite(Ret) & is.finite(Price) & is.finite(Volume) & Volume>0 & Price>0]
dl[, Month := floor_date(Date,"month")]; dl[, dilliq := abs(Ret)/(Price*Volume)]
amihud <- dl[, .(illiq=mean(dilliq), nd=.N), by=.(Ticker,Month)][nd>=10]
amihud[, ib := qbin(illiq), by=Month]
am <- merge(amihud, PM[,.(Ticker,Month,RetM_w)], by=c("Ticker","Month"))
illiqf <- dcast(am[, .(r=mean(RetM_w,na.rm=TRUE)), by=.(Month,ib)], Month~ib, value.var="r")
illiqf <- illiqf[, .(Month, ILLIQ=get("5")-get("1"))][is.finite(ILLIQ)]
setorder(PM, Ticker, Month); PM[, prev_r := shift(RetM_w), by=Ticker]
PM[, gap := as.integer((year(Month)*12+month(Month)) - (year(shift(Month))*12+month(shift(Month)))), by=Ticker]
PM[gap!=1L, prev_r := NA]
rv <- PM[is.finite(prev_r) & is.finite(RetM_w)]; rv[, rb := qbin(prev_r), by=Month]
revf <- dcast(rv[, .(r=mean(RetM_w)), by=.(Month,rb)], Month~rb, value.var="r")
revf <- revf[, .(Month, REV=get("1")-get("5"))][is.finite(REV)]
F <- Reduce(function(a,b) merge(a,b,by="Month",all.x=TRUE),
            Filter(Negate(is.null), list(ff5, umd[,.(Month,UMD)], bab, qmj, illiqf, revf)))
setorder(F, Month)

ls_crsp <- { fq<-as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); fq[,permno:=as.integer(permno)]
  fq[, form_q:=as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
  hm<-fq[rep(seq_len(.N),each=3L)]; hm[,kk:=rep(0:2,times=nrow(fq))]; hm[,Month:=form_q %m+% months(3L+kk)]
  ret<-as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[,permno:=as.integer(permno)]
  p<-merge(hm,ret[,.(permno,Month,RetM)],by=c("permno","Month")); p<-p[is.finite(RetM)]
  p[, RetM:=pmin(pmax(RetM,quantile(RetM,.005,na.rm=T)),quantile(RetM,.995,na.rm=T)),by=Month]
  p[, q:=qbin(GeoRisk),by=Month]; w<-dcast(p[,.(r=mean(RetM)),by=.(Month,q)],Month~q,value.var="r"); w[,LS:=get("5")-get("1")]; w[is.finite(LS),.(Month,LS)] }

ff5v <- c("MktRF","SMB","HML","RMW","CMA")
loads <- function(ls, rhs, label){ d<-merge(ls,F,by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]
  d<-d[complete.cases(d[,c("LS",rhs),with=FALSE])]
  m<-lm(reformulate(rhs,"LS"),d)
  V<-if(have_nw) sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE) else vcov(m)
  est<-coef(m); se<-sqrt(diag(V)); tt<-est/se
  say("\n=== %s  (n=%d months) ===", label, nrow(d))
  say("  %-10s %10s %8s", "term","coef","NW-t")
  for(k in names(est)) say("  %-10s %10.4f %8.2f", ifelse(k=="(Intercept)","alpha",k),
                           if(k=="(Intercept)") 100*est[k] else est[k], tt[k]) }
loads(ls_crsp, c(ff5v,intersect(c("ILLIQ","REV"),names(F))), "CRSP  M3 = FF5 + ILLIQ + REV  (alpha in %/m)")
loads(ls_crsp, c(ff5v,intersect(c("UMD","BAB","QMJ","ILLIQ","REV"),names(F))), "CRSP  M4 = FF5 + UMD + BAB + QMJ + ILLIQ + REV  (alpha in %/m)")
