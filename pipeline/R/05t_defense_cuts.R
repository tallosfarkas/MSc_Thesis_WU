# ==============================================================================
# pipeline/R/05t_defense_cuts.R   (Stage 5 -- defense-feedback deeper cuts, v1.1)
#
# Answers two committee questions on the FROZEN v1.1 results (winsorised returns,
# average-rank ties -- the same conventions as the _v11 headline):
#   #1 Jankowitsch -- CRSP-sample GeoRisk long-short, INDUSTRY-ADJUSTED (SIC 2-digit
#      and SIC 1-digit division). The existing sic2/FIC300 rows sit on the weaker
#      LSEG sort; this puts them on the strong CRSP sample.
#   #2 Jankowitsch/MSCI -- GeoRisk L/S SECTOR-NEUTRAL under GICS (Compustat gsector,
#      pulled from WRDS) and, as a no-WRDS cross-check, SIC 1-digit division.
#   #3 Randl -- GeoSentiment underreaction: decompose the CONTEMPORANEOUS vs FORWARD
#      pricing into the positive (good-news) and negative (bad-news) tone legs, and
#      test the underreaction prediction that the good-news premium is stronger in
#      small (low-attention) firms.
#
#   Rscript pipeline/R/05t_defense_cuts.R  ->  out/analysis/defense_cuts_v11.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(lubridate); library(jsonlite) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){ d<-normalizePath(getwd()); while(d!="/"){ if(file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d<-dirname(d)}; stop("no root") }
exp_dirname <- function() { .e <- Sys.getenv("GEO_EXPO"); if (nzchar(Sys.getenv("GEOV2"))||identical(.e,"v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_",.e) else "exposure" }
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- "_v11"   # tags the output JSON; v2 run sets GEO_TAG=_min
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT,"out",exp_dirname())
say <- function(...) cat(sprintf(...),"\n")
NW <- 6L
TIEM <- "average"; if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")   # v1.1=average, v2=first
qbin <- function(x,n) as.integer(ceiling(frank(x, ties.method=TIEM)/length(x)*n))

# ---- CRSP monthly panel, reconstructed exactly as 05j_crsp_monthly.R --------
fq <- as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); fq[, permno := as.integer(permno)]
fq[, form_q := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
hm <- fq[rep(seq_len(.N), each=3L)]; hm[, k := rep(0:2, times=nrow(fq))]
hm[, Month := form_q %m+% months(3L + k)]; hm[, k := NULL]
ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_monthly.rds"))); ret[, permno := as.integer(permno)]
p <- merge(hm, ret[, .(permno, Month, RetM, MCap_MEnd)], by=c("permno","Month")); p <- p[is.finite(RetM)]
p[, RetM_w := pmin(pmax(RetM, quantile(RetM,.005,na.rm=TRUE)), quantile(RetM,.995,na.rm=TRUE)), by=Month]
p[, sic2 := floor(as.integer(siccd)/100)]; p[, sic1 := floor(as.integer(siccd)/1000)]
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); ff5[, Month := floor_date(as.Date(Date),"month")]
ff5 <- ff5[, c("Month", intersect(names(ff5), c("MktRF","SMB","HML","RMW","CMA","RF"))), with=FALSE]
say("[defense] CRSP panel %s firm-months | %d months", format(nrow(p),big.mark=","), uniqueN(p$Month))

alpha_of <- function(ls){ d<-merge(ls,ff5,by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]
  if(nrow(d)<24) return(list(a=NA,t=NA,n=nrow(d)))
  m<-lm(LS~MktRF+SMB+HML+RMW+CMA,d)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
  list(a=unname(coef(m)[1]), t=unname(coef(m)[1]/se), n=nrow(d)) }
ls_simple <- function(d, meas){ d<-d[is.finite(get(meas))]; d[, b:=qbin(get(meas),5L), by=Month]
  r<-d[, .(Ret=mean(RetM_w,na.rm=TRUE)), by=.(Month,b)]; w<-dcast(r,Month~b,value.var="Ret")
  w[, LS := get("5")-get("1")]; w[,.(Month,LS)] }
ls_indadj <- function(d, meas, indcol){ d<-d[is.finite(get(meas))&!is.na(get(indcol))]
  d[, adj := get(meas)-mean(get(meas),na.rm=TRUE), by=c("Month",indcol)]; ls_simple(d,"adj") }

# ============================ #1 + #2 (CRSP GeoRisk) =========================
res1 <- list(base = alpha_of(ls_simple(p, "GeoRisk")),
             sic2_adj = alpha_of(ls_indadj(p, "GeoRisk", "sic2")),
             sic1_div_adj = alpha_of(ls_indadj(p, "GeoRisk", "sic1")))

## GICS sector from WRDS (Compustat comp.company)
gicsadj <- list(a=NA,t=NA,n=0); gics_cov <- NA_real_
gics <- tryCatch({
  suppressPackageStartupMessages(library(DBI))
  con <- dbConnect(RPostgres::Postgres(), host="wrds-pgdata.wharton.upenn.edu", port=9737L, dbname="wrds",
                   user=Sys.getenv("WRDS_USER"), password=Sys.getenv("WRDS_PASSWORD"), sslmode="require")
  on.exit(try(dbDisconnect(con), silent=TRUE), add=TRUE)
  g <- as.data.table(dbGetQuery(con, "select gvkey, gsector from comp.company where gsector is not null"))
  g[, gvkey := as.integer(gvkey)]; unique(g[, .(gvkey, gsector)], by="gvkey")
}, error = function(e){ message("WRDS gsector pull failed: ", conditionMessage(e)); NULL })
if (!is.null(gics)) {
  p[, gvkey_i := as.integer(gvkey)]
  p <- merge(p, gics, by.x="gvkey_i", by.y="gvkey", all.x=TRUE)
  gics_cov <- 100*mean(!is.na(p$gsector))
  gicsadj <- alpha_of(ls_indadj(p, "GeoRisk", "gsector"))
  say("[defense] GICS gsector coverage on CRSP panel: %.0f%%", gics_cov)
}
res1$gics_adj <- gicsadj
say("\n== #1/#2  GeoRisk EW quintile L/S, CRSP monthly (FF5 alpha %%/m, t) ==")
for (k in names(res1)) { v<-res1[[k]]; say("   %-14s %.3f%%  t=%.2f  (n=%s)", k, 100*v$a, v$t, v$n) }

# ============================ #3 GeoSentiment underreaction ==================
pq <- as.data.table(readRDS({ cand <- c(file.path(ANA,sprintf("panel_ric%s.rds",TAG)), file.path(ANA,"panel_ric.rds")); w <- cand[file.exists(cand)]; if (length(w)) w[1] else cand[1] }))  # prefer the tagged panel (v2: _min)
zc <- function(x){ s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s }
for (v in c("GeoSentiment","GeoSentimentPos","GeoSentimentNeg"))
  pq[, paste0(v,"_zz") := zc(get(v)), by=Quarter]

CTRL <- c("Size","Momentum","BM","Beta_CAPM")
fm <- function(dt, xvar, yvar, ctrls=CTRL, nw=4L){
  cols <- c(yvar, xvar, ctrls); dt <- dt[, ..cols]; dt <- dt[complete.cases(dt)]
  form <- as.formula(paste(yvar,"~",paste(c(xvar,ctrls),collapse="+")))
  lam <- dt[, if(.N>=30) coef(lm(form,.SD))[xvar] else NA_real_, by=dt$Quarter]$V1
  # (re-do with by properly)
  NULL
}
# proper Fama-MacBeth (quarter-by-quarter slope on xvar, then NW mean)
fmacbeth <- function(dt, xvar, yvar, ctrls=CTRL, nw=4L){
  cols <- c("Quarter", yvar, xvar, ctrls)
  d <- dt[, ..cols]; d <- d[complete.cases(d)]
  form <- as.formula(paste(yvar,"~",paste(c(xvar,ctrls),collapse="+")))
  lam <- d[, .(l = if(.N>=30) coef(lm(form,.SD))[xvar] else NA_real_), by=Quarter]$l
  lam <- lam[is.finite(lam)]; if(length(lam)<8) return(list(lambda=NA,t=NA,nq=length(lam)))
  m <- lm(lam~1); se <- if(have_nw) sqrt(sandwich::NeweyWest(m,lag=nw,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]
  list(lambda=unname(coef(m)[1]), t=unname(coef(m)[1]/se), nq=length(lam)) }

legs <- c(GeoSentiment="GeoSentiment_zz", Pos="GeoSentimentPos_zz", Neg="GeoSentimentNeg_zz")
res3 <- list()
for (nm in names(legs)) res3[[nm]] <- list(
  contemp = fmacbeth(pq, legs[[nm]], "QuarterlyRet_W"),   # pops at the call?
  forward = fmacbeth(pq, legs[[nm]], "Ret_lead"))          # predicts ahead? (underreaction drift)
say("\n== #3  GeoSentiment tone legs: Fama-MacBeth lambda(%%)  [t]  contemp | forward ==")
for (nm in names(res3)) say("   %-14s contemp %.3f%% [t=%.2f] | forward %.3f%% [t=%.2f]",
   nm, 100*res3[[nm]]$contemp$lambda, res3[[nm]]$contemp$t, 100*res3[[nm]]$forward$lambda, res3[[nm]]$forward$t)

## low-attention (size) split: underreaction should be stronger in SMALL firms
pq[, small := MCap_Q < median(MCap_Q, na.rm=TRUE), by=Quarter]
res3_size <- list()
for (nm in c("GeoSentiment","Pos")) res3_size[[nm]] <- list(
  small = fmacbeth(pq[small==TRUE],  legs[[nm]], "Ret_lead"),
  large = fmacbeth(pq[small==FALSE], legs[[nm]], "Ret_lead"))
say("\n== #3  forward premium by size (underreaction = stronger in small) ==")
for (nm in names(res3_size)) say("   %-14s small %.3f%% [t=%.2f] | large %.3f%% [t=%.2f]",
   nm, 100*res3_size[[nm]]$small$lambda, res3_size[[nm]]$small$t, 100*res3_size[[nm]]$large$lambda, res3_size[[nm]]$large$t)

# ================================ write =====================================
out <- list(note="defense-feedback deeper cuts on frozen v1.1 (_v11); winsorised returns, average-rank ties",
            crsp_industry = res1, gics_coverage_pct = gics_cov,
            sentiment_underreaction = res3, sentiment_underreaction_by_size = res3_size)
outf <- sprintf("defense_cuts%s.json", TAG)
write_json(out, file.path(ANA,outf), pretty=TRUE, auto_unbox=TRUE, digits=6)
say("\n  wrote out/analysis/%s", outf)
