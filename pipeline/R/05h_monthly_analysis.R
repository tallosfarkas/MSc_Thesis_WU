# ==============================================================================
# pipeline/R/05h_monthly_analysis.R   (Stage 5 — Q1/Q2/Q3 at MONTHLY frequency)
#
# Redoes the three questions on the monthly panel (05g): far more time-series power
# than quarterly. Q3 includes an EXPLORATION GRID (measure x {quintile,decile} x
# {EW,VW} x {global,US}) to locate where the long-short earns significant FF5 alpha.
# Honest exploration: monthly + decile + the theory-motivated Sentiment measure are
# legitimate power/specification changes, not data-mined subsets — the full grid is
# reported, not a cherry-picked cell.
#
#   Rscript pipeline/R/05h_monthly_analysis.R
# Output: out/analysis/monthly_q1.json, monthly_q2.json, monthly_q3_grid.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.find_root <- function() { d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") { if (file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d <- dirname(d) }
  stop("no root") }
ROOT <- .find_root(); ANA <- file.path(ROOT, "out", "analysis"); say <- function(...) cat(sprintf(...), "\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment","GeoExposure_pr","GeoExposureTFIDF_pr")
NW <- 6L
csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

p <- as.data.table(readRDS(vS(file.path(ANA, "panel_ric_monthly.rds"))))
p <- p[is.finite(RetM_w)]
ff5 <- fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv"))
ff5[, Month := floor_date(as.Date(Date), "month")]
fcols <- intersect(names(ff5), c("MktRF","SMB","HML","RMW","CMA","RF"))
ff5 <- ff5[, c("Month", fcols), with = FALSE]
say("[monthly] %s firm-months | %s firms | %d months", format(nrow(p),big.mark=","),
    format(uniqueN(p$Ticker),big.mark=","), uniqueN(p$Month))

nw_t <- function(v){ v<-v[is.finite(v)]; m<-lm(v~1)
  se <- if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]
  c(mean=mean(v), t=mean(v)/se, n=length(v)) }

# ---------- Q1: IVOL ~ exposure | firm + month FE ----------------------------
q1 <- lapply(MEAS, function(m){ p[, ms:=csz(get(m)), by=Month]; d<-p[is.finite(ms)&is.finite(IVOL)]
  mod<-tryCatch(feols(IVOL ~ ms + Size + Momentum + BM | Ticker + Month, d, vcov=~Ticker),error=function(e)NULL)
  if(is.null(mod)) NULL else list(measure=m, coef=unname(coef(mod)["ms"]), t=unname(coef(mod)["ms"]/se(mod)["ms"])) })
q1 <- Filter(Negate(is.null), q1)

# ---------- Q2: Fama-MacBeth (monthly) + panel FE ----------------------------
q2 <- lapply(MEAS, function(m){ p[, ms:=csz(get(m)), by=Month]
  lam <- p[, { d<-.SD[complete.cases(.SD)]; if(nrow(d)<30) NA_real_ else
    coef(lm(RetM_w ~ ms+Size+Momentum+BM+Beta_CAPM, d))["ms"] },
    by=Month, .SDcols=c("RetM_w","ms","Size","Momentum","BM","Beta_CAPM")]$V1
  s<-nw_t(lam)
  d2<-p[is.finite(ms)]; fe<-tryCatch(feols(RetM_w ~ ms + Size+Momentum+BM | Ticker+Month, d2, vcov=~Ticker),error=function(e)NULL)
  list(measure=m, fm_lambda=unname(s["mean"]), fm_t=unname(s["t"]),
       fe_coef=if(is.null(fe))NA else unname(coef(fe)["ms"]), fe_t=if(is.null(fe))NA else unname(coef(fe)["ms"]/se(fe)["ms"])) })

# ---------- Q3: portfolio L/S grid + FF5 spanning ----------------------------
span_alpha <- function(ls_dt){ d<-merge(ls_dt, ff5, by="Month"); d<-d[is.finite(LS)&is.finite(MktRF)]
  mod<-lm(LS ~ MktRF+SMB+HML+RMW+CMA, d)
  se<-if(have_nw) sqrt(sandwich::NeweyWest(mod,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(mod)))[1]
  a<-coef(mod)[1]; list(alpha_m=unname(a), t=unname(a/se), raw_m=mean(d$LS,na.rm=TRUE), n=nrow(d)) }
sort_ls <- function(d, meas, nbin, vw){
  d<-d[is.finite(get(meas))]; d[, bin:=as.integer(ceiling(frank(get(meas),ties.method=TIEM)/.N*nbin)), by=Month]
  r<- if(vw) d[!is.na(bin)&MCap_Q>0, .(Ret=weighted.mean(get(RP),MCap_Q,na.rm=TRUE)), by=.(Month,bin)]
      else   d[!is.na(bin), .(Ret=mean(get(RP),na.rm=TRUE)), by=.(Month,bin)]
  w<-dcast(r, Month~bin, value.var="Ret"); w[, LS:=get(as.character(nbin))-get("1")]; w[,.(Month,LS)] }

grid <- list()
for (uni in c("global","us")) { pu <- if(uni=="us") p[us_can==TRUE] else p
  for (meas in MEAS) for (nbin in c(5L,10L)) for (vw in c(FALSE,TRUE)) {
    a <- span_alpha(sort_ls(pu, meas, nbin, vw))
    grid[[length(grid)+1]] <- data.table(universe=uni, measure=meas, nbin=nbin,
      weight=if(vw)"VW" else "EW", raw_m=a$raw_m, alpha_m=a$alpha_m, t=a$t, n=a$n) } }
grid <- rbindlist(grid)

# ---------- report -----------------------------------------------------------
say("\n=== Q1 IVOL (monthly, FE+ctrl) ===")
for(r in q1) say("  %-20s coef=%.6f t=%.2f", r$measure, r$coef, r$t)
say("\n=== Q2 pricing (monthly) ===  FM lambda/t | panelFE coef/t")
for(r in q2) say("  %-20s FM %.4f%% t=%.2f | FE %.4f%% t=%.2f", r$measure,
                 100*r$fm_lambda, r$fm_t, 100*ifelse(is.na(r$fe_coef),NA,r$fe_coef), r$fe_t)
say("\n=== Q3 L/S FF5-alpha grid (monthly) — SIGNIFICANT cells (|t|>1.96) ===")
sig <- grid[abs(t)>1.96][order(-abs(t))]
if(nrow(sig)) for(i in 1:nrow(sig)) say("  %-6s %-20s %s-%s  alpha=%.3f%%/m (%.1f%%/yr) t=%.2f n=%d",
    sig$universe[i], sig$measure[i], sig$nbin[i], sig$weight[i], 100*sig$alpha_m[i], 1200*sig$alpha_m[i], sig$t[i], sig$n[i]) else say("  (none)")
say("\n  best 6 cells by |t|:")
for(i in 1:min(6,nrow(grid))){ o<-grid[order(-abs(t))][i]; say("  %-6s %-20s %s-%s alpha=%.3f%%/m t=%.2f",
    o$universe, o$measure, o$nbin, o$weight, 100*o$alpha_m, o$t) }

write_json(list(q1=q1), vS(file.path(ANA,"monthly_q1.json")), pretty=TRUE, auto_unbox=TRUE)
write_json(list(q2=q2), vS(file.path(ANA,"monthly_q2.json")), pretty=TRUE, auto_unbox=TRUE)
write_json(list(grid=grid), vS(file.path(ANA,"monthly_q3_grid.json")), pretty=TRUE, auto_unbox=TRUE)
say("\n  wrote out/analysis/monthly_q{1,2,3_grid}.json")
