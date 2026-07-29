# ==============================================================================
# pipeline/R/05r_sentiment_decomposition.R   (Stage 5 — Q2 tone, directional split)
#
# Theory-motivated refinement of the Q2 pricing test (NOT a spec search): the
# life-cycle hypothesis is that BAD geoeconomic news carries a risk premium, but
# GeoSentiment = pos - neg nets the two tones together. Following Sautner et al.
# (who report the tone components separately), this decomposes the forward premium
# into the positive-tone (GeoSentimentPos) and negative-tone (GeoSentimentNeg)
# legs, alongside the combined measure. EVERY variant x method is reported — this
# is a disclosed decomposition, not a cherry-pick.
#
# Methods, all with the SAME controls as 05d/05f (Size/Momentum/BM[/Beta]):
#   - Fama-MacBeth, quarterly, LSEG  + US subsample  (NW 4)
#   - Fama-MacBeth, monthly,  LSEG                    (NW 6)
#   - Panel FE (firm+time), two-way clustered SE     (V2 default)
#
#   GEOV2=1 Rscript pipeline/R/05r_sentiment_decomposition.R
# Output: out/analysis/sentiment_decomp_v2.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); say <- function(...) cat(sprintf(...),"\n")
# ---- version flags (audit 2026-06-12): same block as 05e/06 so GEO_TAG works ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2   <- FIX
vS   <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
if (FIX) set.seed(20250401L)   # parity with the other scripts (FM/FE here use no ties, so GEO_TIES is a no-op)
csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}
VARS <- c("GeoSentiment","GeoSentimentPos","GeoSentimentNeg")

# ---- Fama-MacBeth helper (cross-section per period -> NW mean) ---------------
fm <- function(p, idtime, ret, meas, ctrls, nwlag){
  if (!meas %in% names(p)) return(list(lambda=NA, t=NA, n_per=NA))
  d <- copy(p); d[, ms := csz(get(meas)), by=c(idtime)]
  for (cc in ctrls) d[, (paste0("z_",cc)) := csz(get(cc)), by=c(idtime)]
  rhs <- c("ms", paste0("z_",ctrls)); cols <- c(ret, rhs)
  lam <- d[, { x<-.SD[complete.cases(.SD)]; if(nrow(x)<30) NA_real_ else
                coef(lm(reformulate(rhs, ret), x))["ms"] }, by=c(idtime), .SDcols=cols]$V1
  lam <- lam[is.finite(lam)]; if (length(lam)<8) return(list(lambda=NA,t=NA,n_per=length(lam)))
  m <- lm(lam~1); se <- if(have_nw) sqrt(sandwich::NeweyWest(m,lag=nwlag,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]
  list(lambda=mean(lam), t=mean(lam)/se, n_per=length(lam))
}
fe2 <- function(p, ret, meas, timefe, ctrls){
  if (!meas %in% names(p)) return(list(coef=NA,t=NA,n=NA))
  d <- copy(p); d[, ms := csz(get(meas)), by=c(timefe)]
  f <- as.formula(sprintf("%s ~ ms + %s | Ticker + %s", ret, paste(ctrls, collapse=" + "), timefe))
  vc <- as.formula(sprintf("~ Ticker + %s", timefe))
  m <- tryCatch(feols(f, d[is.finite(ms)], vcov = vc), error=function(e) NULL)
  if (is.null(m)) return(list(coef=NA,t=NA,n=NA))
  list(coef=unname(coef(m)["ms"]), t=unname(coef(m)["ms"]/se(m)["ms"]), n=m$nobs)
}

# ---- panels ------------------------------------------------------------------
pq  <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds"))))
pm  <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric_monthly.rds")))); pm[, Month := as.Date(Month)]
say("[panels] quarterly %s rows | monthly %s rows | V2=%s", format(nrow(pq),big.mark=","),
    format(nrow(pm),big.mark=","), V2)

res <- list()
for (v in VARS) {
  res[[v]] <- list(
    fm_q_lseg = fm(pq[is.finite(Ret_lead)],            "Quarter", "Ret_lead", v, c("Size","Momentum","BM","Beta_CAPM"), 4L),
    fm_q_us   = fm(pq[us_can==TRUE & is.finite(Ret_lead)],"Quarter","Ret_lead", v, c("Size","Momentum","BM","Beta_CAPM"), 4L),
    fm_m_lseg = fm(pm[is.finite(RetM_w)],              "Month",   "RetM_w",   v, c("Size","Momentum","BM"),               6L),
    fe_q_lseg = fe2(pq[is.finite(Ret_lead)], "Ret_lead", v, "Quarter", c("Size","Momentum","BM")) )
}

say("\n=== Q2 tone decomposition (lambda/coef per +1 SD; t in parens) — ALL reported ===")
say("  %-18s %14s %12s %14s %14s", "variant","FM q LSEG","FM q US","FM m LSEG","FE q LSEG")
for (v in VARS) { r<-res[[v]]
  f<-function(x) sprintf("%+.4f(%5.2f)", ifelse(is.null(x$lambda),x$coef,x$lambda), x$t)
  say("  %-18s %14s %12s %14s %14s", v, f(r$fm_q_lseg), f(r$fm_q_us), f(r$fm_m_lseg), f(r$fe_q_lseg)) }
say("\n  Note: GeoSentimentNeg = count of NEGATIVE-tone geo bigrams (bad-news intensity).")
say("  A positive lambda on Neg = bad-news firms earn higher forward returns (risk premium).")

write_json(list(note="theory-motivated tone decomposition; all variants/methods reported (no spec search)",
                controls=list(fm="Size,Momentum,BM,Beta", fe="Size,Momentum,BM, two-way clustered"),
                results=res), vS(file.path(ANA,"sentiment_decomp.json")),
           pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("\n  wrote %s", vS("out/analysis/sentiment_decomp.json"))
