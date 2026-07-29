# ==============================================================================
# pipeline/R/05n_event_study.R   (Stage 5 — daily EVENT STUDY around the EC date)
#
# Ties Q1 (realization) to Q2/Q3 (forward premium). For every earnings call we take the
# DAILY abnormal return over a [-20,+20] trading-day window and average it by EX-ANTE
# exposure quintile (sorted within quarter, the same sort Q2/Q3 use). Hypothesis (the
# proposal's "Bad News -> Price down/Vol up -> Expected Return up"): high-geoeconomic-
# exposure calls realise the bad news AT the call (negative event-window AR -> Q1), then
# earn a positive drift afterwards (the risk premium -> Q2/Q3). We also cut by the
# WITHIN-FIRM CHANGE (DGeoExpo), the realization regression's (05m) sort variable.
#
# DEFENSIBLE BENCHMARKS (estimated per event over the pre-event estimation window):
#   estimation window = [-170,-21] trading days (150 days, no overlap with [-20,+20]).
#   M1 Market model : (Ret-RF) = a + b*MktRF + e ; AR = (Ret-RF) - (a_hat + b_hat*MktRF)
#   M2 Fama-French5 : (Ret-RF) = a + b*MktRF + s*SMB + h*HML + r*RMW + c*CMA + e ;
#                     AR = excess - fitted (the full FF5 model)
#   M3 Market-adjusted (beta=1, AR = Ret - Rm) kept ONLY as a robustness comparison row.
# Coefficients: per-event sufficient stats from a windowed expansion -> a 6x6 solve per
# event (market model is the 2x2 sub-solve). Verified against lm() on a random sample.
# CAR over a window = sum of AR over its trading days. Significance: Q5-Q1 difference in
# window CAR, t = mean/(sd/sqrt(N)) across events (cross-sectional).
#
#   Rscript pipeline/R/05n_event_study.R
# Outputs: out/analysis/event_study.json  +  out/analysis/event_study_car.png
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(jsonlite) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); EXP <- file.path(ROOT, if (nzchar(Sys.getenv("GEOV2"))||identical(Sys.getenv("GEO_EXPO"),"v2")) "out/exposure_v2" else "out/exposure")
say <- function(...) cat(sprintf(...),"\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

WIN <- 20L                              # +/- trading days (event window)
EST_LEN <- 150L; EST_GAP <- 21L         # estimation window [-(EST_LEN+EST_GAP-1), -EST_GAP] = [-170,-21]
MEAS <- c("GeoSentiment","GeoSentimentPos","GeoSentimentNeg")   # tone-leg split (defense)

# ---- 1. events: one row per call, with event date, ric, exposure ------------
ev <- as.data.table(readRDS(file.path(EXP,"exposure_calls_dedup.rds")))
ev <- ev[!is.na(ric) & !is.na(date)]
ev[, edate := as.Date(date)]; ev[, eid := .I]
qbin <- function(x){ r <- frank(x, ties.method=TIEM, na.last="keep"); n <- sum(is.finite(r))   # ceiling-rank bins
                     as.integer(ceiling(r / n * 5)) }   # robust to ties (matches 05b/06); cut-on-quantile broke on avg-rank
for (m in MEAS) ev[, paste0("Q_",m) := qbin(get(m)), by=.(year,quarter)]
setorder(ev, ric, year, quarter); ev[, qidx := year*4+quarter]
ev[, dCC := { dv <- GeoExposure - shift(GeoExposure); dq <- qidx - shift(qidx)
              ifelse(dq==1L, dv, NA_real_) }, by=ric]
ev[, Q_dCC := qbin(dCC), by=.(year,quarter)]
say("[1] events: %s calls with ric+date | %s firms", format(nrow(ev),big.mark=","), uniqueN(ev$ric))

# ---- 2. daily returns for the event tickers + FF factors --------------------
tk <- unique(ev$ric)
dt <- as.data.table(open_dataset(file.path(ROOT,"data/processed/LSEG_Final_Panel.parquet")) |>
        dplyr::select(Date, Ticker, Ret) |>
        dplyr::filter(Ticker %in% tk) |> dplyr::collect())
dt[, Date := as.Date(Date)]; dt <- dt[!is.na(Ret)]
ff <- fread(file.path(ROOT,"data/inputs/ff5_factors_daily.csv")); ff[, Date := as.Date(Date)]
dt <- ff[, .(Date, MktRF, SMB, HML, RMW, CMA, RF)][dt, on="Date"]
dt <- dt[!is.na(MktRF)]
dt[, ex := Ret - RF]                                   # firm excess return
dt[, Rm := MktRF + RF]                                 # market return (for beta=1 row)
setorder(dt, Ticker, Date); dt[, ti := seq_len(.N), by=Ticker]
nmax <- dt[, .(nmax=max(ti)), by=Ticker]
say("[2] daily rows: %s | tickers matched: %s", format(nrow(dt),big.mark=","), uniqueN(dt$Ticker))

# ---- 3. locate t0 (first trading day on/after the call) + estimation window -
setkey(dt, Ticker, ti)
t0 <- dt[, .(Ticker, Date, ti)][ev[,.(Ticker=ric, Date=edate, eid)], on=.(Ticker,Date), roll=if (V2) -7 else -Inf,
         .(eid, Ticker, t0=ti)]
t0 <- t0[!is.na(t0)]
t0 <- nmax[t0, on="Ticker"]
t0[, `:=`(hi = t0-EST_GAP, lo = t0-EST_GAP-EST_LEN+1L)]   # estimation window in ti
n_anchor <- nrow(t0)
t0 <- t0[lo-1L >= 1L & hi <= nmax]                         # need full estimation history
if (V2) t0 <- t0[t0 + WIN <= nmax]                         # V2: require the FULL event window too
say("[3] events with t0: %s | with full [-170,-21] estimation history: %s",
    format(n_anchor,big.mark=","), format(nrow(t0),big.mark=","))

# ---- 4. per-event sufficient stats over the estimation window ---------------
# expand each event over the 150 estimation offsets, join factor values, sum by event.
est_off <- (-(EST_LEN+EST_GAP-1L)):(-EST_GAP)            # -170 .. -21
ne <- nrow(t0)
est <- t0[rep(seq_len(ne), each=length(est_off)), .(eid, Ticker, t0)]
est[, ti := t0 + rep(est_off, times=ne)]; est[, t0 := NULL]
est <- dt[, .(Ticker, ti, ex, mk=MktRF, sm=SMB, hm=HML, rm=RMW, cm=CMA)][est, on=.(Ticker, ti)]
S <- est[, .(n=.N, S_ex=sum(ex),
             S_mk=sum(mk), S_sm=sum(sm), S_hm=sum(hm), S_rm=sum(rm), S_cm=sum(cm),
             S_exmk=sum(ex*mk), S_exsm=sum(ex*sm), S_exhm=sum(ex*hm), S_exrm=sum(ex*rm), S_excm=sum(ex*cm),
             S_mk2=sum(mk*mk), S_sm2=sum(sm*sm), S_hm2=sum(hm*hm), S_rm2=sum(rm*rm), S_cm2=sum(cm*cm),
             S_mksm=sum(mk*sm), S_mkhm=sum(mk*hm), S_mkrm=sum(mk*rm), S_mkcm=sum(mk*cm),
             S_smhm=sum(sm*hm), S_smrm=sum(sm*rm), S_smcm=sum(sm*cm),
             S_hmrm=sum(hm*rm), S_hmcm=sum(hm*cm), S_rmcm=sum(rm*cm)), by=eid]
rm(est); gc(FALSE)
say("[4] estimation sufficient-stats built for %s events", format(nrow(S),big.mark=","))

# ---- 5. estimate coefficients (market model 2x2 + FF5 6x6 per event) --------
S[, denom := n*S_mk2 - S_mk^2]                           # market model (closed form)
S[, b_mm := (n*S_exmk - S_mk*S_ex)/denom]
S[, a_mm := (S_ex - b_mm*S_mk)/n]
ff5 <- function(r){
  A <- matrix(c(r$n,    r$S_mk,  r$S_sm,  r$S_hm,  r$S_rm,  r$S_cm,
                r$S_mk, r$S_mk2, r$S_mksm,r$S_mkhm,r$S_mkrm,r$S_mkcm,
                r$S_sm, r$S_mksm,r$S_sm2, r$S_smhm,r$S_smrm,r$S_smcm,
                r$S_hm, r$S_mkhm,r$S_smhm,r$S_hm2, r$S_hmrm,r$S_hmcm,
                r$S_rm, r$S_mkrm,r$S_smrm,r$S_hmrm,r$S_rm2, r$S_rmcm,
                r$S_cm, r$S_mkcm,r$S_smcm,r$S_hmcm,r$S_rmcm,r$S_cm2), 6, 6, byrow=TRUE)
  bvec <- c(r$S_ex, r$S_exmk, r$S_exsm, r$S_exhm, r$S_exrm, r$S_excm)
  tryCatch(solve(A, bvec), error=function(e) rep(NA_real_,6)) }
cf <- t(vapply(seq_len(nrow(S)), function(i) ff5(S[i]), numeric(6)))
S[, `:=`(a_ff=cf[,1], b_ff=cf[,2], s_ff=cf[,3], h_ff=cf[,4], r_ff=cf[,5], c_ff=cf[,6])]
# verify vs lm() on a random sample
set.seed(42); chk <- sample(S$eid, 30); mm_err <- ff_err <- 0
for (e in chk){ r0 <- t0[eid==e]; d <- dt[Ticker==r0$Ticker & ti>=r0$lo & ti<=r0$hi]
  m1 <- lm(ex~MktRF, d); m2 <- lm(ex~MktRF+SMB+HML+RMW+CMA, d)
  mm_err <- max(mm_err, abs(coef(m1)[2]-S[eid==e,b_mm]))
  ff_err <- max(ff_err, max(abs(coef(m2)[-1]-unlist(S[eid==e,.(b_ff,s_ff,h_ff,r_ff,c_ff)])))) }
say("[5] coefficient check vs lm() (max |coef diff| over 30 events): market=%.2e ff5=%.2e", mm_err, ff_err)

# ---- 6. event-window AR under each model -----------------------------------
setkey(dt, Ticker, ti); n0 <- nrow(t0); ks <- -WIN:WIN
tg <- t0[rep(seq_len(n0), each=length(ks)), .(eid, Ticker, t0)]
tg[, k := rep(ks, times=n0)]; tg[, ti := t0 + k]
tg <- dt[, .(Ticker, ti, ex, Rm, MktRF, SMB, HML, RMW, CMA, Ret)][tg, on=.(Ticker, ti)]
tg <- S[, .(eid, a_mm, b_mm, a_ff, b_ff, s_ff, h_ff, r_ff, c_ff)][tg, on="eid"]
tg[, AR_mm  := ex - (a_mm + b_mm*MktRF)]                                                  # market model
tg[, AR_ff5 := ex - (a_ff + b_ff*MktRF + s_ff*SMB + h_ff*HML + r_ff*RMW + c_ff*CMA)]      # FF5
tg[, AR_b1  := Ret - Rm]                                                                  # beta=1 market-adjusted
tg <- ev[, c("eid", paste0("Q_",MEAS), "Q_dCC"), with=FALSE][tg, on="eid"]
say("[6] event-day observations: %s (each with 3 AR models)", format(nrow(tg),big.mark=","))

# ---- 7. CAR by quintile + Q5-Q1 window tests, per model --------------------
windows <- list(pre=c(-WIN,-2), event=c(-1,1), post=c(2,WIN), full=c(-WIN,WIN))
car_by <- function(qcol, arcol){ d <- tg[is.finite(get(arcol)) & !is.na(get(qcol))]
  aar <- d[, .(aar=mean(get(arcol))), by=.(q=get(qcol), k)][order(q,k)]; aar[, car:=cumsum(aar), by=q]; aar }
ev_car <- function(qcol, arcol){ d <- tg[is.finite(get(arcol)) & !is.na(get(qcol)), .(eid, q=get(qcol), k, AR=get(arcol))]
  out <- list()
  for (wn in names(windows)){ lo<-windows[[wn]][1]; hi<-windows[[wn]][2]
    cc <- d[k>=lo & k<=hi, .(car=sum(AR)), by=.(eid,q)]
    hi5 <- cc[q==5,car]; lo1 <- cc[q==1,car]; tt <- tryCatch(t.test(hi5,lo1), error=function(e)NULL)
    out[[wn]] <- list(q5_minus_q1 = if(is.null(tt)) NA else unname(diff(rev(tt$estimate))),
                      t = if(is.null(tt)) NA else unname(tt$statistic), n5=length(hi5), n1=length(lo1)) }
  out }
models <- c(AR_mm="market_model", AR_ff5="ff5", AR_b1="market_adj_beta1")
res <- list()
for (m in c(MEAS,"dCC")){ qcol <- paste0("Q_",m)
  res[[m]] <- list(car_mm = car_by(qcol,"AR_mm"),
                   windows = setNames(lapply(names(models), function(a) ev_car(qcol,a)), models)) }

say("\n=== Q5-Q1 event-window CAR by model, t in () [event = [-1,+1]] ===")
say("  %-12s %-16s %14s %14s %14s %12s","measure","model","pre[-20,-2]","event[-1,1]","post[2,20]","full")
for (m in c(MEAS,"dCC")) for (a in models){ w <- res[[m]]$windows[[a]]
  f <- function(x) sprintf("%6.3f%%(%5.1f)", 100*x$q5_minus_q1, x$t)
  say("  %-12s %-16s %14s %14s %14s %12s", m, a, f(w$pre), f(w$event), f(w$post), f(w$full)) }
say("\n  (event[-1,1]<0 for high exposure = bad news priced at the call; post[2,20]>0 = forward premium.)")

write_json(list(window_def=windows, win=WIN, est_window=c(-(EST_LEN+EST_GAP-1L),-EST_GAP),
                models=unname(models), lm_check=list(market=mm_err, ff5=ff_err),
                results=res, n_events=nrow(t0)),
           vS(file.path(ANA,"event_study_tone.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=8)
say("  wrote out/analysis/event_study.json")

# ---- 8. plot CAR paths (market model; GeoExposure + dCC, Q1 vs Q5) -----------
ok <- tryCatch({
  png(file.path(ANA,"event_study_tone_car.png"), width=1100, height=520, res=110)
  par(mfrow=c(1,2), mar=c(4,4,3,1))
  for (m in c("GeoSentimentPos","GeoSentimentNeg")){ a <- res[[m]]$car_mm
    plot(NA, xlim=c(-WIN,WIN), ylim=range(a$car,na.rm=TRUE)*1.05, xlab="trading days from call",
         ylab="CAR (market-model abnormal return)", main=sprintf("Event-study CAR by %s quintile",m))
    abline(v=0, col="grey70", lty=2); abline(h=0, col="grey85")
    cols <- c("1"="#2c7bb6","5"="#d7191c")
    for (qq in c(1,5)){ s<-a[q==qq]; lines(s$k, s$car, col=cols[as.character(qq)], lwd=2) }
    legend("topleft", c("Q1 (low)","Q5 (high)"), col=cols, lwd=2, bty="n", cex=.9) }
  dev.off(); TRUE }, error=function(e){ message("plot skipped: ", conditionMessage(e)); FALSE })
if (isTRUE(ok)) say("  wrote out/analysis/event_study_car.png")
