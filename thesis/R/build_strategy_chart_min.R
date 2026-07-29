#!/usr/bin/env Rscript
# =====================================================================
# build_strategy_chart.R
# Cumulative-return chart of the geoeconomic long-short trading strategy
# (GeoRisk Q5-Q1, equally weighted, CRSP monthly -- the headline t=2.76)
# against the Fama-French market factor (Mkt-RF) and the S&P 500.
# Reconstructs the L/S series exactly as pipeline/R/05j_crsp_monthly.R.
# Separate from build_thesis_artifacts.R because it fetches the S&P 500
# (cached to data/inputs/macro/sp500_monthly.csv for offline re-runs).
#   Rscript docs/thesis/R/build_strategy_chart.R
# =====================================================================
suppressMessages({ library(data.table); library(lubridate); library(ggplot2) })

args <- commandArgs(FALSE)
here <- dirname(sub("--file=", "", grep("--file=", args, value = TRUE)[1]))
root <- tryCatch(normalizePath(file.path(here, "..", "..", "..")), error = function(e) getwd())
if (!dir.exists(file.path(root, "out"))) root <- getwd()
thesis <- file.path(root, "docs", "thesis"); figdir <- file.path(thesis, "figures"); bwdir <- file.path(figdir, "bw")
dir.create(bwdir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. GeoRisk EW long-short, CRSP monthly (replicates 05j) ---------
fq <- as.data.table(readRDS(file.path(root, "out/exposure_min/exposure_firmquarter_crsp.rds")))
fq[, permno := as.integer(permno)]; fq[, form_q := as.Date(ISOdate(year, (quarter - 1L) * 3L + 1L, 1L))]
hm <- fq[rep(seq_len(.N), each = 3L)]; hm[, k := rep(0:2, times = nrow(fq))]
hm[, Month := form_q %m+% months(3L + k)]; hm[, k := NULL]
ret <- as.data.table(readRDS(file.path(root, "out/analysis/crsp_returns_monthly.rds"))); ret[, permno := as.integer(permno)]
p <- merge(hm, ret[, .(permno, Month, RetM, MCap_MEnd)], by = c("permno", "Month"))
p <- p[is.finite(RetM)]
p[, RetM_w := pmin(pmax(RetM, quantile(RetM, .005, na.rm = TRUE)), quantile(RetM, .995, na.rm = TRUE)), by = Month]
d <- p[is.finite(GeoRisk)]
d[, b := as.integer(ceiling(frank(GeoRisk, ties.method = "first") / .N * 5)), by = Month]
ew <- d[, .(Ret = mean(RetM_w, na.rm = TRUE)), by = .(Month, b)]
w  <- dcast(ew, Month ~ b, value.var = "Ret"); w[, LS := get("5") - get("1")]
ls <- w[is.finite(LS), .(Month = floor_date(Month, "month"), strat = LS)]
cat(sprintf("L/S series: %d months %s..%s, mean %.3f%%/m\n", nrow(ls),
            as.character(min(ls$Month)), as.character(max(ls$Month)), 100 * mean(ls$strat)))

# ---- 2. FF market factor (Mkt-RF) -----------------------------------
ff <- fread(file.path(root, "data/inputs/ff5_factors_monthly.csv")); ff[, Month := floor_date(as.Date(Date), "month")]
mkt <- ff[, .(Month, mkt = MktRF)]

# ---- 3. S&P 500 monthly return (cached fetch) -----------------------
cache <- file.path(root, "data/inputs/macro/sp500_monthly.csv")
sp <- tryCatch({
  if (file.exists(cache)) { x <- fread(cache); x[, Month := floor_date(as.Date(Month), "month")]; x }
  else {
    suppressMessages(library(quantmod))
    sym <- tryCatch({ getSymbols("^SP500TR", src = "yahoo", from = "2001-12-01", auto.assign = TRUE); "SP500TR" },
                    error = function(e) { getSymbols("^GSPC", src = "yahoo", from = "2001-12-01", auto.assign = TRUE); "GSPC" })
    r <- monthlyReturn(Ad(get(sym)))
    s <- data.table(Month = floor_date(as.Date(index(r)), "month"), sp500 = as.numeric(r))
    fwrite(s, cache); cat("fetched S&P 500 (", sym, "), cached.\n"); s }
}, error = function(e) { message("S&P 500 unavailable: ", conditionMessage(e)); NULL })

# ---- 4. merge, cumulate growth of $1 over the strategy sample -------
series <- Filter(Negate(is.null), list(ls, mkt, sp))
m <- Reduce(function(a, b) merge(a, b, by = "Month", all = FALSE), series)
setorder(m, Month)
labs <- c(strat = "Geoeconomic L/S (GeoRisk Q5–Q1)", mkt = "FF market factor (Mkt–RF)", sp500 = "S&P 500")
rcols <- intersect(c("strat", "mkt", "sp500"), names(m))
cum <- m[, c(list(Month = Month), lapply(.SD, function(x) cumprod(1 + x))), .SDcols = rcols]
long <- melt(cum, id.vars = "Month", variable.name = "series", value.name = "growth")
long[, series := factor(series, levels = rcols, labels = labs[rcols])]

# ---- 5. plot (pastel + grayscale) -----------------------------------
theme_thesis <- function(base = 11) theme_minimal(base_size = base, base_family = "serif") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = base), legend.position = "bottom", legend.title = element_blank())
PASTEL <- c("#3E7CB1", "#F2A6A6", "#A8D5A2")  # deep-pastel blue / coral / green
base <- ggplot(long, aes(Month, growth, group = series)) +
  geom_hline(yintercept = 1, colour = "grey80") +
  labs(x = NULL, y = "Cumulative growth of $1") + theme_thesis()
pc <- base + geom_line(aes(colour = series), linewidth = 0.8) + scale_colour_manual(values = PASTEL)
pb <- base + geom_line(aes(linetype = series), colour = "black", linewidth = 0.6)
# cairo_pdf where available (cluster/audited path); base pdf() fallback otherwise
PDF_DEV <- local({
  ok <- tryCatch({ tmp <- tempfile(fileext = ".pdf"); grDevices::cairo_pdf(tmp); grDevices::dev.off(); unlink(tmp); TRUE },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (ok) grDevices::cairo_pdf else { message("cairo_pdf unavailable; using base pdf() for fig_strategy_cumret."); grDevices::pdf }
})
ggsave(file.path(figdir, "fig_strategy_cumret.pdf"), pc, width = 6.3, height = 3.8, device = PDF_DEV)
ggsave(file.path(bwdir,  "fig_strategy_cumret.pdf"), pb, width = 6.3, height = 3.8, device = PDF_DEV)
cat("wrote fig_strategy_cumret.pdf (colour + bw);", nrow(m), "common months,",
    paste(rcols, collapse = "/"), "\n")

# ---- 6. by-measure L/S cumulative: GeoRisk+GeoSentiment 2-panel (main) + all-measure (appendix) ----
ls_series <- function(meas) {
  d2 <- p[is.finite(get(meas))]
  d2[, b := as.integer(ceiling(frank(get(meas), ties.method = "first") / .N * 5)), by = Month]
  ew2 <- d2[, .(Ret = mean(RetM_w, na.rm = TRUE)), by = .(Month, b)]
  w2 <- dcast(ew2, Month ~ b, value.var = "Ret"); w2[, LS := get("5") - get("1")]
  w2[is.finite(LS), .(Month = floor_date(Month, "month"), strat = LS)]
}
NICE4 <- c(GeoExposure = "GeoExposure", GeoExposureTFIDF = "GeoExposure (TF-IDF)",
           GeoRisk = "GeoRisk", GeoSentiment = "GeoSentiment")
MCOL4 <- c("GeoExposure" = "#8FB8DE", "GeoExposure (TF-IDF)" = "#F2A6A6",
           "GeoRisk" = "#A8D5A2", "GeoSentiment" = "#C9A0DC")

# 2x1 legs facet: GeoRisk + GeoSentiment, Q5/Q1 legs vs the FF market, with the L/S
# (reproduces the MTS deck fig_G2 (RQ3 slide) + fig_G2b (A12b) exactly: first ties, total
#  market MktRF+RF, log scale, flat line at 1, each series starting at 1).
mkt_tot <- ff[, .(Month, mkt = MktRF + RF)]
ls_legs <- function(meas) {
  d2 <- p[is.finite(get(meas))]
  d2[, b := as.integer(ceiling(frank(get(meas), ties.method = "first") / .N * 5)), by = Month]
  w2 <- dcast(d2[, .(r = mean(RetM_w, na.rm = TRUE)), by = .(Month, b)], Month ~ b, value.var = "r")
  setnames(w2, as.character(1:5), paste0("Q", 1:5)); setorder(w2, Month)
  w2 <- w2[is.finite(Q1) & is.finite(Q5)]; w2[, LS := Q5 - Q1]; w2
}
SER <- c("Q5 leg (high)", "Q1 leg (low)", "FF market", "Long-short (Q5-Q1)")
legrows <- function(meas, mlab) {
  w <- ls_legs(meas); mk <- mkt_tot[Month %in% w$Month]; setorder(mk, Month)
  dd <- rbindlist(list(
    data.table(Month = w$Month,  cum = cumprod(1 + w$Q5),  series = SER[1]),
    data.table(Month = w$Month,  cum = cumprod(1 + w$Q1),  series = SER[2]),
    data.table(Month = mk$Month, cum = cumprod(1 + mk$mkt), series = SER[3]),
    data.table(Month = w$Month,  cum = cumprod(1 + w$LS),  series = SER[4])))
  dd <- rbind(dd[, .(Month = min(Month) %m-% months(1L), cum = 1), by = series], dd)
  dd[, `:=`(series = factor(series, levels = SER), measure = mlab)][]
}
legs2 <- rbindlist(list(legrows("GeoRisk", "GeoRisk"), legrows("GeoSentiment", "GeoSentiment")))
legs2[, measure := factor(measure, levels = c("GeoRisk", "GeoSentiment"))]
SERCOL <- setNames(c("#8FB8DE", "#A8D5A2", "grey45", "#F2A6A6"), SER)   # Q5 blue, Q1 green, market grey, L/S coral
SERLTY <- setNames(c("solid", "solid", "dashed", "solid"), SER)
SERLTYBW <- setNames(c("solid", "dotted", "dashed", "longdash"), SER)
# one single-panel legs figure per measure (split from the old 2-panel so each is shorter)
one_legs <- function(mlab, colour = TRUE) {
  g <- ggplot(legs2[measure == mlab], aes(Month, cum, colour = series, linetype = series)) +
    geom_hline(yintercept = 1, colour = "grey80") +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") + scale_y_log10() +
    labs(x = NULL, y = "Cumulative value (log, 1 = start)") + theme_thesis()
  if (colour) g + geom_line(linewidth = 0.8) + scale_colour_manual(values = SERCOL, name = NULL) +
      scale_linetype_manual(values = SERLTY, name = NULL)
  else g + geom_line(colour = "black", linewidth = 0.6) + scale_linetype_manual(values = SERLTYBW, name = NULL) +
      guides(colour = "none")
}
for (mm in c("GeoRisk", "GeoSentiment")) {
  fn <- if (mm == "GeoRisk") "fig_q3_legs_georisk.pdf" else "fig_q3_legs_geosent.pdf"
  ggsave(file.path(figdir, fn), one_legs(mm, TRUE),  width = 6.4, height = 3.5, device = PDF_DEV)
  ggsave(file.path(bwdir,  fn), one_legs(mm, FALSE), width = 6.4, height = 3.5, device = PDF_DEV)
}

# all-measure single panel, EXACTLY like the MTS deck (fig_G3): four L/S lines,
# per-measure colours, log-scale y, flat line at 1 = market-neutral zero, each series starts at 1.
allm <- rbindlist(lapply(names(NICE4), function(mm) {
  s <- ls_series(mm); setorder(s, Month); s[, cum := cumprod(1 + strat)]
  rbind(data.table(Month = min(s$Month) %m-% months(1L), cum = 1), s[, .(Month, cum)])[, measure := NICE4[[mm]]][]
}))
allm[, measure := factor(measure, levels = unname(NICE4))]
pAbase <- ggplot(allm, aes(Month, cum)) + geom_hline(yintercept = 1, colour = "grey70") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") + scale_y_log10() +
  labs(x = NULL, y = "Cumulative value (log, 1 = start)") + theme_thesis()
ggsave(file.path(figdir, "fig_q3_cumret_all.pdf"),
       pAbase + geom_line(aes(colour = measure), linewidth = 0.9) + scale_colour_manual(values = MCOL4, name = NULL),
       width = 6.6, height = 4.0, device = PDF_DEV)
ggsave(file.path(bwdir, "fig_q3_cumret_all.pdf"),
       pAbase + geom_line(aes(linetype = measure), colour = "black", linewidth = 0.6),
       width = 6.6, height = 4.0, device = PDF_DEV)
cat("wrote fig_q3_legs_georisk+geosent.pdf + fig_q3_cumret_all.pdf\n")

# ---- 7. transaction costs: gross vs net for the two significant strategies (approximate) ----
# Market-cap-decreasing cost model (A17): TCbps = base + slope*sqrt(refcap/mcap), base=5, slope=5
# (so a median-cap firm costs ~10 bps one-way, smaller/less-liquid firms more). Applied to the
# actual measured one-way turnover of each leg. Net return = gross - traded-weight * TCbps.
suppressMessages(library(jsonlite))
TC_BASE <- 5; TC_SLOPE <- 5; TC_CAP <- 150
ff5m <- ff[, .(Month, MktRF, SMB, HML, RMW, CMA)]
tc_net <- function(meas, pp = p) {
  d2 <- pp[is.finite(get(meas)) & is.finite(MCap_MEnd) & MCap_MEnd > 0]
  d2[, b := as.integer(ceiling(frank(get(meas), ties.method = "first") / .N * 5)), by = Month]
  refcap <- d2[, .(rc = median(head(sort(MCap_MEnd, decreasing = TRUE), 500))), by = Month]  # ~S&P500 median cap proxy
  d2 <- merge(d2, refcap, by = "Month")
  d2[, tcbps := pmin(TC_BASE + TC_SLOPE * sqrt(rc / pmax(MCap_MEnd, 1)), TC_CAP)]
  ew <- d2[, .(r = mean(RetM_w, na.rm = TRUE), N = .N, tc = mean(tcbps)), by = .(Month, b)]
  w  <- dcast(ew[b %in% c(1L, 5L)], Month ~ b, value.var = c("r", "tc"))
  setnames(w, c("r_1", "r_5", "tc_1", "tc_5"), c("rQ1", "rQ5", "tcQ1", "tcQ5"))
  w <- w[is.finite(rQ1) & is.finite(rQ5)]; w[, LS := rQ5 - rQ1]; setorder(w, Month)
  # one-way turnover per leg (fraction of names replaced), times the leg's average cost
  legmem <- d2[b %in% c(1L, 5L), .(permno, Month, leg = ifelse(b == 5L, "Q5", "Q1"))]
  turn <- legmem[, {
    ms <- sort(unique(Month)); to <- numeric(length(ms)); prev <- integer(0)
    for (i in seq_along(ms)) { cur <- permno[Month == ms[i]]
      to[i] <- if (!length(prev)) 1 else length(setdiff(cur, prev)) / length(cur); prev <- cur }
    .(Month = ms, turnover = to) }, by = leg]
  tw <- dcast(turn, Month ~ leg, value.var = "turnover")
  m2 <- Reduce(function(a, b) merge(a, b, by = "Month", all.x = TRUE),
               list(w[, .(Month, LS, tcQ1, tcQ5)], tw))
  m2[is.na(Q5), Q5 := 0][is.na(Q1), Q1 := 0]
  m2[, cost := (Q5 * tcQ5 + Q1 * tcQ1) / 1e4]        # traded fraction * bps, both legs
  m2[, `:=`(gross = LS, net = LS - cost)]
  m2[, .(Month, gross, net)]
}
ann_alpha <- function(x, col) { dd <- merge(x, ff5m, by = "Month"); dd <- dd[is.finite(get(col))]
  a <- unname(coef(lm(reformulate(c("MktRF","SMB","HML","RMW","CMA"), col), dd))[1]); 100 * a * 12 }
# OOS (re-learned dictionary) panel: each year scored with the vintage discovered on
# text <= year-1 (exposure_rt_<year>.rds, Phase B of 14_realtime_dict_oos), mapped to
# permno via the v8.2 crosswalk -- the same book the OOS alpha in Section 4.4 trades.
p_rt <- tryCatch({
  rtf <- sort(list.files(file.path(root, "out/exposure_min"), pattern = "^exposure_rt_\\d{4}\\.rds$", full.names = TRUE))
  rt  <- rbindlist(lapply(rtf, readRDS), use.names = TRUE, fill = TRUE)
  emap <- as.data.table(readRDS(file.path(root, "ra_project/GEO_RA_mapping_v8.2/output/ec_ccm_map_v8.rds")))
  emap <- unique(emap[!is.na(permno), .(Id = eventId, permno = as.integer(permno))], by = "Id")
  fqr <- merge(rt[, .(Id, year, quarter, GeoRisk, GeoSentiment)], emap, by = "Id")
  fqr <- fqr[!is.na(permno), .(GeoRisk = mean(GeoRisk, na.rm = TRUE), GeoSentiment = mean(GeoSentiment, na.rm = TRUE)),
             by = .(permno, year, quarter)]
  fqr[, form_q := as.Date(ISOdate(year, (quarter - 1L) * 3L + 1L, 1L))]
  hmr <- fqr[rep(seq_len(.N), each = 3L)]; hmr[, k := rep(0:2, times = nrow(fqr))]
  hmr[, Month := form_q %m+% months(3L + k)]; hmr[, k := NULL]
  pr <- merge(hmr, ret[, .(permno, Month, RetM, MCap_MEnd)], by = c("permno", "Month")); pr <- pr[is.finite(RetM)]
  pr[, RetM_w := pmin(pmax(RetM, quantile(RetM, .005, na.rm = TRUE)), quantile(RetM, .995, na.rm = TRUE)), by = Month]
  pr
}, error = function(e) { message("OOS rt panel unavailable: ", conditionMessage(e)); NULL })

tc_out <- list(cost_base_bps = TC_BASE, cost_slope_bps = TC_SLOPE)
tc_long <- list(); tc_wide <- list()
for (mm in c("GeoRisk", "GeoSentiment")) {
  s <- tc_net(mm); setorder(s, Month)
  ga <- ann_alpha(s, "gross"); na <- ann_alpha(s, "net")
  tc_out[[mm]] <- list(gross_ann = round(ga, 2), net_ann = round(na, 2),
                       turnover_pm = round(100 * mean((s$gross - s$net) / pmax(abs(s$gross), 1e-9)), 1))
  m0 <- min(s$Month) %m-% months(1L); gc <- cumprod(1 + s$gross); nc <- cumprod(1 + s$net)
  tc_wide[[mm]] <- data.table(Month = c(m0, s$Month), gross = c(1, gc), net = c(1, nc), measure = mm)
  tc_long[[mm]] <- rbind(
    data.table(Month = c(m0, s$Month), cum = c(1, gc), series = "Gross", measure = mm),
    data.table(Month = c(m0, s$Month), cum = c(1, nc), series = "Net of costs", measure = mm))
  if (!is.null(p_rt)) {
    so <- tc_net(mm, p_rt); setorder(so, Month)
    tc_out[[paste0(mm, "_oos")]] <- list(gross_ann = round(ann_alpha(so, "gross"), 2),
                                         net_ann   = round(ann_alpha(so, "net"), 2))
    m0o <- min(so$Month) %m-% months(1L); gco <- cumprod(1 + so$gross); nco <- cumprod(1 + so$net)
    tc_long[[paste0(mm, "_oos")]] <- rbind(
      data.table(Month = c(m0o, so$Month), cum = c(1, gco), series = "OOS gross", measure = mm),
      data.table(Month = c(m0o, so$Month), cum = c(1, nco), series = "OOS net of costs", measure = mm))
  }
}
TAG <- Sys.getenv("GEO_TAG")   # honour the version tag like 05b-05o do
write_json(tc_out, file.path(root, paste0("out/analysis/q3_trading_costs", TAG, ".json")), auto_unbox = TRUE, digits = 4)
tc_dt <- rbindlist(tc_long); tcw <- rbindlist(tc_wide)
SLEV <- c("Gross", "Net of costs", "OOS gross", "OOS net of costs")
tc_dt[, `:=`(series = factor(series, levels = SLEV), measure = factor(measure, levels = c("GeoRisk", "GeoSentiment")))]
tcw[, measure := factor(measure, levels = c("GeoRisk", "GeoSentiment"))]
# shade the cost drag = the area between the gross and net paths (in-sample pair only);
# the OOS pair (re-learned dictionary book, starts 2013) is drawn dashed on the same panel
tcbase <- ggplot() + geom_hline(yintercept = 1, colour = "grey80") +
  facet_wrap(~measure, ncol = 1, scales = "free_y") + scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(x = NULL, y = "Cumulative growth of $1") + theme_thesis()
ggsave(file.path(figdir, "fig_q3_netcost.pdf"),
       tcbase + geom_ribbon(data = tcw, aes(Month, ymin = net, ymax = gross), fill = "#F2A6A6", alpha = 0.30) +
         geom_line(data = tc_dt, aes(Month, cum, colour = series, linetype = series), linewidth = 0.7) +
         scale_colour_manual(values = c("Gross" = "#8FB8DE", "Net of costs" = "#D9534F",
                                        "OOS gross" = "#3E7CB1", "OOS net of costs" = "#8A3634"), name = NULL) +
         scale_linetype_manual(values = c("Gross" = "solid", "Net of costs" = "solid",
                                          "OOS gross" = "22", "OOS net of costs" = "22"), name = NULL),
       width = 6.4, height = 5.2, device = PDF_DEV)
ggsave(file.path(bwdir, "fig_q3_netcost.pdf"),
       tcbase + geom_ribbon(data = tcw, aes(Month, ymin = net, ymax = gross), fill = "grey70", alpha = 0.35) +
         geom_line(data = tc_dt, aes(Month, cum, linetype = series), colour = "black", linewidth = 0.6) +
         scale_linetype_manual(values = c("Gross" = "solid", "Net of costs" = "42",
                                          "OOS gross" = "22", "OOS net of costs" = "13"), name = NULL),
       width = 6.4, height = 5.2, device = PDF_DEV)
cat(sprintf("wrote fig_q3_netcost.pdf; GeoRisk gross %.2f%%/net %.2f%%; GeoSent gross %.2f%%/net %.2f%%\n",
    tc_out$GeoRisk$gross_ann, tc_out$GeoRisk$net_ann, tc_out$GeoSentiment$gross_ann, tc_out$GeoSentiment$net_ann))
