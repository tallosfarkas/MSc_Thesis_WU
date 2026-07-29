# ==============================================================================
# pipeline/R/05g_build_monthly.R   (Stage 5 — MONTHLY panel from daily LSEG)
#
# Builds (a) a monthly return + monthly IVOL panel by compounding the daily LSEG
# price file, and (b) a monthly ANALYSIS panel: each firm-quarter's exposure is
# held over the 3 months of the FOLLOWING quarter (exposure known at quarter end,
# no look-ahead), merged to that month's return + formation-quarter controls/beta.
# Monthly frequency ~tripples the time-series (>=270 months) -> power for Q2/Q3.
#
#   Rscript pipeline/R/05g_build_monthly.R
# Outputs: out/analysis/lseg_monthly.rds (cache), panel_ric_monthly.rds
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(lubridate) })
.find_root <- function() { d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") { if (file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d <- dirname(d) }
  stop("no root") }
ROOT <- .find_root(); ANA <- file.path(ROOT, "out", "analysis"); if (!dir.exists(ANA)) dir.create(ANA, recursive=TRUE)
PROC <- file.path(ROOT, "data", "processed"); say <- function(...) cat(sprintf(...), "\n")
MIN_DAYS <- 15L
# V2 correctness track (audit 2026-06-12): v2 exposure inputs, _v2 outputs, and
# NO future-cap fallback for missing formation MCap (drop instead).
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")
exp_dirname <- function() { .e <- Sys.getenv("GEO_EXPO"); if (nzchar(Sys.getenv("GEOV2"))||identical(.e,"v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_",.e) else "exposure" }  # self-contained
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p

# ---- 1. daily -> monthly returns + monthly IVOL (cached) --------------------
mc <- file.path(ANA, "lseg_monthly.rds")
if (file.exists(mc)) { mon <- as.data.table(readRDS(mc)); say("[monthly] loaded cache (%s firm-months)", format(nrow(mon), big.mark=",")) } else {
  say("[monthly] compounding daily LSEG -> monthly (heavy) ...")
  d <- as.data.table(read_parquet(file.path(PROC, "LSEG_Final_Panel.parquet"),
                                  col_select = c("Date","Ticker","Ret","MCap")))
  d <- d[is.finite(Ret)]; d[, Date := as.Date(Date)]
  ff3 <- fread(file.path(ROOT, "data/inputs/ff3_factors_daily.csv")); ff3[, Date := as.Date(Date)]
  d <- merge(d, ff3[, .(Date, MktRF, SMB, HML, RF)], by = "Date")
  d[, ExcessRet := Ret - RF]; d[, Month := floor_date(Date, "month")]
  d <- d[is.finite(ExcessRet) & is.finite(MktRF) & is.finite(SMB) & is.finite(HML)]
  say("[monthly] daily rows: %s | tickers %s", format(nrow(d), big.mark=","), format(uniqueN(d$Ticker), big.mark=","))
  mon <- d[, {
    if (.N < MIN_DAYS) .(RetM = NA_real_, IVOL = NA_real_, MCap_MEnd = NA_real_, n_days = .N)
    else { fit <- .lm.fit(cbind(1, MktRF, SMB, HML), ExcessRet)
           .(RetM = prod(1 + Ret) - 1, IVOL = sd(fit$residuals),
             MCap_MEnd = last(MCap[!is.na(MCap)]), n_days = .N) }
  }, by = .(Ticker, Month)][!is.na(RetM)]
  saveRDS(mon, mc); say("[monthly] built + cached: %s firm-months", format(nrow(mon), big.mark=","))
}
# light winsor of monthly returns within month
mon[, RetM_w := pmin(pmax(RetM, quantile(RetM, .005, na.rm=TRUE)), quantile(RetM, .995, na.rm=TRUE)), by = Month]

# ---- 2. expand quarterly exposure to the 3 FOLLOWING months ----------------
fq <- as.data.table(readRDS(file.path(ROOT, "out", exp_dirname(),
                                      "exposure_firmquarter_ric.rds")))
setnames(fq, "ric", "Ticker")
fq[, form_q := as.Date(ISOdate(year, (quarter - 1L) * 3L + 1L, 1L))]      # formation quarter start
# held months = the 3 months of the NEXT quarter
hm <- fq[rep(seq_len(.N), each = 3L)]
hm[, k := rep(0:2, times = nrow(fq))]
hm[, Month := form_q %m+% months(3L + k)]                                # next quarter's 3 months
hm[, k := NULL]

# merge monthly returns (held-period), controls + beta (as of FORMATION quarter)
controls <- as.data.table(readRDS(file.path(PROC, "CONTROLS_PANEL.rds")))
beta     <- as.data.table(readRDS(file.path(PROC, "BETA_PANEL.rds")))
p <- merge(hm, mon[, .(Ticker, Month, RetM, RetM_w, MCap_MEnd, IVOL)], by = c("Ticker","Month"))
p <- merge(p, controls[, .(Ticker, form_q = Quarter, Size, Momentum, BM, MCap_Q)], by = c("Ticker","form_q"), all.x = TRUE)
p <- merge(p, beta[, .(Ticker, form_q = Quarter, Beta_CAPM)], by = c("Ticker","form_q"), all.x = TRUE)
if (V2) {
  # audit fix: never fill formation-quarter cap with the held month's END cap
  # (future info). Rows without a formation cap simply keep MCap_Q = NA (0.3%).
} else {
  p[is.na(MCap_Q), MCap_Q := MCap_MEnd]
}

say("[monthly] analysis panel: %s firm-months | %s firms | %s months (%s..%s) | with RetM=%s",
    format(nrow(p), big.mark=","), format(uniqueN(p$Ticker), big.mark=","), uniqueN(p$Month),
    as.character(min(p$Month)), as.character(max(p$Month)), format(sum(!is.na(p$RetM)), big.mark=","))
saveRDS(p, vS(file.path(ANA, "panel_ric_monthly.rds"))); write_parquet(p, vS(file.path(ANA, "panel_ric_monthly.parquet")))
say("=== wrote %s ===", vS("out/analysis/panel_ric_monthly.rds"))
