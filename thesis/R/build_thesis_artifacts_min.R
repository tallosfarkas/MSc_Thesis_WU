#!/usr/bin/env Rscript
# =====================================================================
# build_thesis_artifacts.R
# Turns the locked results (out/analysis/*.json, out/exposure/*.rds) into
# thesis artefacts so the author writes prose around them, never hardcoding:
#   docs/thesis/numbers.tex      -- \newcommand inline-number macros
#   docs/thesis/tables/*.tex     -- booktabs result tables (all RQs + robustness)
#   docs/thesis/figures/*.pdf    -- pastel colour charts
#   docs/thesis/figures/bw/*.pdf -- grayscale / black-print versions
# Re-run after any results change, then recompile the thesis.
#   Rscript docs/thesis/R/build_thesis_artifacts.R
# =====================================================================
suppressMessages({ library(jsonlite); library(data.table); library(ggplot2) })

args <- commandArgs(FALSE)
here <- dirname(sub("--file=", "", grep("--file=", args, value = TRUE)[1]))
root <- tryCatch(normalizePath(file.path(here, "..", "..", "..")), error = function(e) getwd())
if (!dir.exists(file.path(root, "out"))) root <- getwd()
thesis <- file.path(root, "docs", "thesis"); ana <- file.path(root, "out", "analysis")
figdir <- file.path(thesis, "figures"); bwdir <- file.path(figdir, "bw"); tabdir <- file.path(thesis, "tables")
for (d in c(figdir, bwdir, tabdir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
# v1.1 is the sole reported version: prefer the _v11 twin (audit-fixed inference,
# first-ties, v8.2) so the tables match the inline \Sexpr numbers; fall back to the
# unversioned JSON only for artefacts without a _v11 build.
J <- function(f) {
  v11 <- sub("\\.json$", "_min.json", f)
  p   <- if (file.exists(file.path(ana, v11))) v11 else f
  fromJSON(file.path(ana, p), simplifyVector = TRUE)
}

# ---- formatting -----------------------------------------------------
fnum <- function(x, d = 2) ifelse(is.na(x), "", sprintf(paste0("%.", d, "f"), x))
fpct <- function(x, d = 2) fnum(100 * x, d)
star <- function(t) { a <- abs(t); ifelse(is.na(t), "", ifelse(a >= 2.58, "***", ifelse(a >= 1.96, "**", ifelse(a >= 1.64, "*", "")))) }
cellp <- function(coef, t, d = 2) paste0(fpct(coef, d), star(t))        # percent + stars
cellpt<- function(coef, t, d = 2) paste0("(", fnum(t, d), ")")
# combined "coef* (t)" cell, BOLD when significant at 5% -- makes significance unmissable
fcell <- function(coef, t, d = 2) {
  v <- paste0(fpct(coef, d), star(t)); tt <- paste0("(", fnum(t, 2), ")")
  if (!is.na(t) && abs(t) >= 1.96) sprintf("\\textbf{%s}~\\textbf{%s}", v, tt) else sprintf("%s~%s", v, tt) }
NICE  <- c(GeoExposure = "GeoExposure", GeoExposureTFIDF = "GeoExposure (TF-IDF)",
           GeoRisk = "GeoRisk", GeoSentiment = "GeoSentiment",
           GeoExposure_pr = "GeoExposure (pruned)", GeoExposureTFIDF_pr = "GeoExposure (TF-IDF, pruned)")
nice  <- function(m) ifelse(m %in% names(NICE), NICE[m], m)

writetab <- function(file, lines) writeLines(lines, file.path(tabdir, file))
tabular  <- function(colspec, header_lines, body_rows)
  c(paste0("\\begin{tabular}{", colspec, "}"), "\\toprule", header_lines, "\\midrule",
    body_rows, "\\bottomrule", "\\end{tabular}")

# =====================================================================
# FIGURE THEME + PALETTES (pastel colour  +  grayscale print)
# =====================================================================
theme_thesis <- function(base = 11)
  theme_minimal(base_size = base, base_family = "serif") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = base),
        legend.position = "bottom", legend.title = element_blank(),
        legend.key.width = unit(1.1, "lines"))
# Categorical palette: blues and greys only. Chosen for PRINT -- the series separate
# by lightness, not by hue, so the figure survives a black-and-white printer, a
# photocopier and every form of colour blindness. The earlier pastel set failed on
# contrast (1.6-2.1 against white, where 3 is the floor); these steps clear it.
# Order is fixed, so a series keeps its slot across every figure.
PASTEL  <- c("#1F4E79", "#6FA8D0", "#767676", "#BDBDBD", "#3E7CB1", "#9A9A9A")  # deep blue / light blue / grey
PASTEL5 <- c("#BcD7EE", "#9FC2E0", "#6FA8D0", "#3E7CB1", "#1F4E79")             # pastel->deep blue (quintiles)
GREY5   <- c("#cfcfcf", "#a0a0a0", "#707070", "#404040", "#000000")
# Prefer cairo_pdf (nicer fonts; the cluster/audited path). Where the cairo device
# cannot load at runtime (e.g. a Mac without XQuartz/libXrender), fall back to the
# base pdf() device so figures still regenerate -- same data, plainer fonts.
PDF_DEV <- local({
  ok <- tryCatch({ tmp <- tempfile(fileext = ".pdf"); grDevices::cairo_pdf(tmp); grDevices::dev.off(); unlink(tmp); TRUE },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (ok) grDevices::cairo_pdf else { message("cairo_pdf unavailable; using base pdf() for figures."); grDevices::pdf }
})
save_both <- function(name, p_col, p_bw, w = 6.3, h = 3.6) {
  ggsave(file.path(figdir, name), p_col, width = w, height = h, device = PDF_DEV)
  ggsave(file.path(bwdir,  name), p_bw,  width = w, height = h, device = PDF_DEV)
}

# =====================================================================
# 1. numbers.tex  (headline inline macros, from the CORRECT sources)
# =====================================================================
q1   <- J("realization_q1.json"); v1 <- J("volatility_q1.json")$results
cm   <- J("crsp_monthly.json"); mq2 <- J("monthly_q2.json")$q2; asp <- J("augmented_spanning.json")
r_exp <- q1$results[q1$results$measure == "GeoExposure", ]
v_exp <- v1[v1$measure == "GeoExposure", ]
ccrisk_m <- cm$q3[cm$q3$measure == "GeoRisk", ]                      # headline: monthly CRSP, t=2.76
sent_m   <- mq2[mq2$measure == "GeoSentiment", ]
dict_h <- tryCatch(nrow(fread(file.path(root, "pipeline/config/dictionary_geoeconomic.csv"))), error = function(e) 9650)
dict_p <- tryCatch(nrow(fread(file.path(root, "pipeline/config/dictionary_geoeconomic_pruned.csv"))), error = function(e) 4586)
fmtN <- function(n) formatC(n, big.mark = "{,}", format = "d")
macros <- c("% Auto-generated by docs/thesis/R/build_thesis_artifacts.R -- do not edit.",
  sprintf("\\newcommand{\\GeoDropCoef}{%s}",  fpct(r_exp$ret_contemp$coef)),
  sprintf("\\newcommand{\\GeoDropT}{%s}",     fnum(r_exp$ret_contemp$t)),
  sprintf("\\newcommand{\\IvolCoefT}{%s}",    fnum(v_exp$t)),                 # -3.29
  sprintf("\\newcommand{\\SentFMt}{%s}",      fnum(sent_m$fm_t)),
  sprintf("\\newcommand{\\SentFEt}{%s}",      fnum(sent_m$fe_t)),
  sprintf("\\newcommand{\\GeoRiskAlpha}{%s}",  fpct(ccrisk_m$ew_alpha)),       # 0.32 (monthly %)
  sprintf("\\newcommand{\\GeoRiskAlphaT}{%s}", fnum(ccrisk_m$ew_t)),           # 2.76
  sprintf("\\newcommand{\\GeoRiskAlphaAnn}{%s}", fnum(100 * 12 * ccrisk_m$ew_alpha, 1)), # ~3.9 %/yr
  sprintf("\\newcommand{\\SpanAlpha}{%s}",    fpct(asp$crsp$models$M3_augmented$alpha)),
  sprintf("\\newcommand{\\SpanAlphaT}{%s}",   fnum(asp$crsp$models$M3_augmented$t)),
  sprintf("\\newcommand{\\NRealize}{%s}",     fmtN(q1$n)),
  sprintf("\\newcommand{\\NEvents}{%s}",      fmtN(J("event_study.json")$n_events)),
  sprintf("\\newcommand{\\DictHeadline}{%s}", fmtN(dict_h)),
  sprintf("\\newcommand{\\DictPruned}{%s}",   fmtN(dict_p)))
writeLines(macros, file.path(thesis, "numbers.tex")); cat("numbers.tex OK\n")

# =====================================================================
# 2. TABLES (all RQs + robustness)
# =====================================================================
M4 <- c("GeoExposure", "GeoExposureTFIDF", "GeoRisk", "GeoSentiment")

## Q1 -- realization: contemp return (%) + IVOL (bps) + TVOL (bps, systematic-risk check)
local({
  rr <- q1$results; tv <- J("volatility_q1.json")$tvol
  bpsc <- function(coef, t) paste0(sprintf("%.1f", 10000*coef), star(t))   # coef in bps + stars
  rows <- vapply(M4, function(m) {
    a <- rr[rr$measure == m, ]; b <- v1[v1$measure == m, ]; d <- tv[tv$measure == m, ]
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", nice(m),
            cellp(a$ret_contemp$coef, a$ret_contemp$t), cellpt(0, a$ret_contemp$t),
            bpsc(b$coef, b$t), cellpt(0, b$t),
            bpsc(d$coef, d$t), cellpt(0, d$t)) }, character(1))
  writetab("tab_q1_realization.tex", tabular("lcccccc",
    c(" & \\multicolumn{2}{c}{Contemp.\\ return} & \\multicolumn{2}{c}{Idiosyncratic vol.} & \\multicolumn{2}{c}{Total vol.}\\\\",
      "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
      "Measure & coef (\\%) & $t$ & coef (bps) & $t$ & coef (bps) & $t$\\\\"),
    rows)) })

## Q1 event study -- Q5-Q1 CAAR (%) by window, four measures (market model)
local({
  es  <- J("event_study.json")$results
  mm  <- c("GeoExposure","GeoExposureTFIDF","dCC","GeoRisk","GeoSentiment")
  lab <- c(GeoExposure="GeoExposure", GeoExposureTFIDF="GeoExposure (TF-IDF)", dCC="$\\Delta$GeoExposure", GeoRisk="GeoRisk", GeoSentiment="GeoSentiment")
  ord <- c("full","pre","event","post")
  cell <- function(w) paste0(fpct(w$q5_minus_q1), star(w$t), "~(", fnum(w$t), ")")
  rows <- vapply(mm, function(m){ W <- es[[m]]$windows$market_model
    sprintf("%s & %s \\\\", lab[m], paste(vapply(ord, function(k) cell(W[[k]]), character(1)), collapse=" & ")) }, character(1))
  writetab("tab_q1_event.tex", tabular("lcccc",
    c(" & \\multicolumn{4}{c}{Q5$-$Q1 CAAR (\\%), market model, with $t$}\\\\",
      "\\cmidrule(lr){2-5}",
      "Measure & $[-20,+20]$ & $[-20,-2]$ & $[-1,+1]$ & $[+2,+20]$\\\\"), rows)) })

## Q2 PRICING -- one table per (sample x frequency); bold = significant ----------
# LSEG global, QUARTERLY: FM univariate | FM + controls | panel-FE + controls
local({
  fm <- J("fama_macbeth_ric.json")$results; pf <- J("panel_fe_ric.json")$results
  rows <- vapply(M4, function(m) { u <- fm$univariate[fm$measure==m,]; c2 <- fm$with_controls[fm$measure==m,]
    fe <- pf$fe_plus_controls[pf$measure==m,]
    sprintf("%s & %s & %s & %s \\\\", nice(m), fcell(u$lambda_q,u$nw_t), fcell(c2$lambda_q,c2$nw_t), fcell(fe$coef,fe$t)) }, character(1))
  writetab("tab_q2_lseg_quarterly.tex", tabular("lccc",
    c("Measure & FM univariate & FM + controls & Panel FE + controls\\\\",
      " & $\\lambda$~$(t)$ & $\\lambda$~$(t)$ & coef~$(t)$\\\\"), rows)) })
# LSEG global, MONTHLY: FM | panel-FE
local({
  mq <- J("monthly_q2.json")$q2
  rows <- vapply(M4, function(m) { r <- mq[mq$measure==m,]
    sprintf("%s & %s & %s \\\\", nice(m), fcell(r$fm_lambda,r$fm_t), fcell(r$fe_coef,r$fe_t)) }, character(1))
  writetab("tab_q2_lseg_monthly.tex", tabular("lcc",
    c("Measure & FM $\\lambda$~$(t)$ & Panel FE coef~$(t)$\\\\"), rows)) })
# CRSP US, QUARTERLY FM
local({
  cq <- J("crsp_analysis.json")$q2
  rows <- vapply(M4, function(m) { r <- cq[cq$measure==m,]; sprintf("%s & %s \\\\", nice(m), fcell(r$fm_lambda,r$fm_t)) }, character(1))
  writetab("tab_q2_crsp_quarterly.tex", tabular("lc", c("Measure & FM $\\lambda$~$(t)$\\\\"), rows)) })
# CRSP US, MONTHLY FM
local({
  c2 <- cm$q2
  rows <- vapply(M4, function(m) { r <- c2[c2$measure==m,]; sprintf("%s & %s \\\\", nice(m), fcell(r$fm_lambda,r$fm_t)) }, character(1))
  writetab("tab_q2_crsp_monthly.tex", tabular("lc", c("Measure & FM $\\lambda$~$(t)$\\\\"), rows)) })
# LSEG US-only, QUARTERLY (appendix): FM univariate | FM + controls
local({
  fu <- J("fama_macbeth_ric_us.json")$results
  rows <- vapply(M4, function(m) { u <- fu$univariate[fu$measure==m,]; c2 <- fu$with_controls[fu$measure==m,]
    sprintf("%s & %s & %s \\\\", nice(m), fcell(u$lambda_q,u$nw_t), fcell(c2$lambda_q,c2$nw_t)) }, character(1))
  writetab("tab_q2_lseg_us_quarterly.tex", tabular("lcc",
    c("Measure & FM univariate $\\lambda$~$(t)$ & FM + controls $\\lambda$~$(t)$\\\\"), rows)) })

## Q3 STRATEGY -- long-short FF5 alpha, one table per (sample x frequency) -------
hdr3 <- c("Measure & EW $\\alpha$~$(t)$ & VW $\\alpha$~$(t)$\\\\")
rows_crsp <- function(df) vapply(M4, function(m) { r <- df[df$measure==m,]
  sprintf("%s & %s & %s \\\\", nice(m), fcell(r$ew_alpha,r$ew_t), fcell(r$vw_alpha,r$vw_t)) }, character(1))
# LSEG global, QUARTERLY
local({
  ps <- J("portfolio_sorts_ric.json")$results
  rows <- vapply(M4, function(m) { e <- ps$ew_ls_ff5[ps$measure==m,]; v <- ps$vw_ls_ff5[ps$measure==m,]
    sprintf("%s & %s & %s \\\\", nice(m), fcell(e$alpha_q,e$nw_t), fcell(v$alpha_q,v$nw_t)) }, character(1))
  writetab("tab_q3_lseg_quarterly.tex", tabular("lcc", hdr3, rows)) })
# LSEG global, MONTHLY (from the sort grid, 5 bins)
local({
  g <- J("monthly_q3_grid.json")$grid; g <- g[g$universe=="global" & g$nbin==5,]
  rows <- vapply(M4, function(m) { e <- g[g$measure==m & g$weight=="EW",]; v <- g[g$measure==m & g$weight=="VW",]
    sprintf("%s & %s & %s \\\\", nice(m), fcell(e$alpha_m,e$t), fcell(v$alpha_m,v$t)) }, character(1))
  writetab("tab_q3_lseg_monthly.tex", tabular("lcc", hdr3, rows)) })
# CRSP US, QUARTERLY
local({ writetab("tab_q3_crsp_quarterly.tex", tabular("lcc", hdr3, rows_crsp(J("crsp_analysis.json")$q3))) })
# CRSP US, MONTHLY (headline)
local({ writetab("tab_q3_crsp_monthly.tex", tabular("lcc", hdr3, rows_crsp(cm$q3))) })
# Winsorisation robustness -- the headline EW alpha with monthly returns winsorised at
# 0.5/99.5 (the main specification) against raw returns. Built here rather than typed:
# the hand-maintained version had drifted, showing 0.26** where the generated CRSP
# table shows 0.26*** for the same estimate (|t| = 2.74 clears the 1% cutoff).
local({
  w <- cm$q3; r <- J("crsp_monthly_rawrb.json")$q3
  g <- function(df, m, col) df[[col]][df$measure == m]
  rows <- vapply(c("GeoRisk", "GeoSentiment"), function(m)
    sprintf("%s & %s & %s \\\\", nice(m),
            fcell(g(w, m, "ew_alpha"), g(w, m, "ew_t")),
            fcell(g(r, m, "ew_alpha"), g(r, m, "ew_t"))), character(1))
  writetab("tab_winsor_robustness.tex",
           tabular("lcc", c(" & Winsorised (0.5/99.5) & Raw \\\\"), rows)) })
# LSEG US-only, QUARTERLY (appendix)
local({
  pu <- J("portfolio_sorts_ric_us.json")$results
  rows <- vapply(M4, function(m) { e <- pu$ew_ls_ff5[pu$measure==m,]; v <- pu$vw_ls_ff5[pu$measure==m,]
    sprintf("%s & %s & %s \\\\", nice(m), fcell(e$alpha_q,e$nw_t), fcell(v$alpha_q,v$nw_t)) }, character(1))
  writetab("tab_q3_lseg_us_quarterly.tex", tabular("lcc", hdr3, rows)) })

## Robustness -- GeoRisk across splits (appendix)
local({
  rb <- J("robustness_q3.json"); splits <- c("quintile","decile","size_neutral","pre2018","post2018","sic2_adj","fic300_adj")
  lab <- c("Quintile","Decile","Size-neutral","Pre-2018","Post-2018","Industry-adj.","FIC300-adj.")
  rows <- vapply(splits, function(s) { x <- rb$GeoRisk[[s]]
    sprintf("%s & %s~%s \\\\", lab[match(s, splits)], cellp(x[1], x[2]), cellpt(0, x[2])) }, character(1))
  writetab("tab_robustness_ccrisk.tex", tabular("lc",
    c("Specification & GeoRisk L/S $\\alpha$~$(t)$\\\\"), rows)) })

## Augmented spanning -- GeoRisk alpha across factor models + macro loadings (CRSP)
local({
  mo <- asp$crsp$models; ld <- asp$crsp$loadings
  mods <- c(M0_FF5 = "FF5", M1_FF5_UMD = "FF5 + UMD", M2_FF5_GPR_EPU = "FF5 + GPR + EPU", M3_augmented = "Augmented (8-factor)")
  rows <- vapply(names(mods), function(k)
    sprintf("%s & %s~%s \\\\", mods[k], cellp(mo[[k]]$alpha, mo[[k]]$t), cellpt(0, mo[[k]]$t)), character(1))
  writetab("tab_spanning.tex", tabular("lc",
    c("Factor model & GeoRisk L/S $\\alpha$~$(t)$\\\\"), rows))
  lds <- asp$GeoSentiment$crsp$loadings
  lrows <- vapply(names(ld), function(k) {
    s <- if (!is.null(lds[[k]])) sprintf("%s & %s", fnum(lds[[k]]$beta, 4), cellpt(0, lds[[k]]$t)) else "-- & --"
    sprintf("%s & %s & %s & %s \\\\", k, fnum(ld[[k]]$beta, 4), cellpt(0, ld[[k]]$t), s) }, character(1))
  writetab("tab_spanning_loadings.tex", tabular("lcccc",
    c(" & \\multicolumn{2}{c}{GeoRisk L/S} & \\multicolumn{2}{c}{GeoSentiment L/S}\\\\",
      "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}Factor & $\\beta$ & $(t)$ & $\\beta$ & $(t)$\\\\"), lrows)) })

## gvkey (firm-level) robustness -- GeoRisk + GeoSentiment
local({
  gv <- J("crsp_gvkey.json")
  cell2 <- function(q, m) sprintf("%s~%s", cellp(q$ew_alpha[q$measure==m], q$ew_t[q$measure==m]),
                                  cellpt(0, q$ew_t[q$measure==m]))
  rows <- c(sprintf("Monthly (firm) & %s & %s \\\\",
              cell2(gv$monthly$q3, "GeoRisk"),   cell2(gv$monthly$q3, "GeoSentiment")),
            sprintf("Quarterly (firm) & %s & %s \\\\",
              cell2(gv$quarterly$q3, "GeoRisk"), cell2(gv$quarterly$q3, "GeoSentiment")))
  writetab("tab_gvkey.tex", tabular("lcc",
    c("Aggregation & GeoRisk EW L/S $\\alpha$~$(t)$ & GeoSentiment EW L/S $\\alpha$~$(t)$\\\\"), rows)) })
## HP controls -- geo coefficient ~unchanged with Hoberg-Phillips controls (monthly)
local({
  hp <- J("hp_controls.json")$monthly
  rows <- vapply(M4, function(m) { a <- hp$q2[hp$q2$measure==m,]; b <- hp$panel_fe[hp$panel_fe$measure==m,]
    sprintf("%s & %s & %s & %s & %s \\\\", nice(m),
      fcell(a$base_lambda,a$base_t), fcell(a$hp_lambda,a$hp_t),
      fcell(b$base_coef,b$base_t), fcell(b$hp_coef,b$hp_t)) }, character(1))
  writetab("tab_hp_controls.tex", tabular("lcccc",
    c(" & \\multicolumn{2}{c}{Fama--MacBeth} & \\multicolumn{2}{c}{Panel FE}\\\\",
      "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5} & base & + HP & base & + HP\\\\",
      "Measure & $\\lambda$~$(t)$ & $\\lambda$~$(t)$ & coef~$(t)$ & coef~$(t)$\\\\"), rows)) })

## TNIC product-market spillover -- own vs peer geoeconomic exposure
local({
  tn <- J("tnic_spillover.json")$results
  p <- function(v) { v <- as.numeric(unlist(v)); fcell(v[1], v[2]) }
  rows <- vapply(seq_len(nrow(tn)), function(i)
    sprintf("%s & %s & %s & %s & %s \\\\", nice(tn$measure[i]),
      p(tn$peer_only[[i]]), p(tn$own_only[[i]]), p(tn$own_vs_peer_OWN[[i]]), p(tn$own_vs_peer_PEER[[i]])), character(1))
  writetab("tab_tnic.tex", tabular("lcccc",
    c(" & Peer only & Own only & \\multicolumn{2}{c}{Own + peer jointly}\\\\",
      "\\cmidrule(lr){4-5} & coef~$(t)$ & coef~$(t)$ & own~$(t)$ & peer~$(t)$\\\\",
      "Measure & & & & \\\\"), rows)) })

## Robustness -- GeoRisk L/S across all measures x specifications
local({
  rb <- J("robustness_q3.json"); meas <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
  splits <- c("quintile","decile","size_neutral","pre2018","post2018","sic2_adj","fic300_adj")
  lab <- c("Quintile","Decile","Size-neutral","Pre-2018","Post-2018","Industry-adj.","FIC300-adj.")
  rows <- vapply(seq_along(splits), function(j) {
    cells <- vapply(meas, function(m) { x <- rb[[m]][[splits[j]]]; fcell(x[1], x[2]) }, character(1))
    sprintf("%s & %s \\\\", lab[j], paste(cells, collapse = " & ")) }, character(1))
  writetab("tab_robustness_all.tex", tabular("lcccc",
    c("Specification & GeoExposure & GeoExposure (TF-IDF) & GeoRisk & GeoSentiment\\\\",
      " & $\\alpha$~$(t)$ & $\\alpha$~$(t)$ & $\\alpha$~$(t)$ & $\\alpha$~$(t)$\\\\"), rows)) })

## Full monthly sort grid -- DISABLED for v2: tab_q3_grid.tex is built CRSP-only and kept static (do not clobber)
if (FALSE) local({
  g <- J("monthly_q3_grid.json")$grid
  g <- g[order(g$universe, match(g$measure, names(NICE)), g$nbin, g$weight), ]
  rows <- vapply(seq_len(nrow(g)), function(i)
    sprintf("%s & %s & %d & %s & %s \\\\",
      ifelse(g$universe[i]=="global","Global","US"), nice(g$measure[i]), g$nbin[i], g$weight[i],
      fcell(g$alpha_m[i], g$t[i])), character(1))
  hd <- "Universe & Measure & Bins & Wt. & $\\alpha$~$(t)$\\\\"
  writetab("tab_q3_grid.tex", c("\\begin{longtable}{llccc}",
    "\\caption[Full monthly long--short sort grid]{Full monthly long--short sort grid: universe (global/US)",
    "$\\times$ measure $\\times$ number of bins $\\times$ weighting. Newey--West $t$;",
    "\\textbf{bold} = significant at 5\\%.}\\label{tab:q3_grid}\\\\",
    "\\toprule", hd, "\\midrule", "\\endfirsthead",
    "\\toprule", hd, "\\midrule", "\\endhead", "\\midrule", "\\endfoot", "\\bottomrule", "\\endlastfoot",
    rows, "\\end{longtable}")) })

## Q1 monthly contemporaneous return (robustness on the realization result)
local({
  m1 <- J("monthly_q1.json")$q1
  rows <- vapply(M4, function(m) { r <- m1[m1$measure==m,]; sprintf("%s & %s \\\\", nice(m), fcell(r$coef,r$t)) }, character(1))
  writetab("tab_q1_monthly.tex", tabular("lc", c("Measure & Contemp.\\ return coef~$(t)$\\\\"), rows)) })

cat("tables OK\n")

# =====================================================================
# 3. FIGURES (pastel + grayscale)
# =====================================================================
## Fig 1 -- all four measures over time (z-scored so their common profile is comparable)
local({
  ex <- readRDS(file.path(root, "out", "exposure", "exposure_firmquarter_ric.rds")); setDT(ex)
  M4v <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
  ts <- ex[!is.na(year) & !is.na(quarter), lapply(.SD, mean, na.rm=TRUE), .SDcols=M4v,
           by = .(date = as.Date(sprintf("%d-%02d-01", year, (quarter-1)*3+1)))][order(date)]
  tsl <- melt(ts, id.vars="date", variable.name="series", value.name="val")
  # the four measures live on very different scales; z-score each to its own mean 0 / sd 1
  tsl[, z := (val - mean(val, na.rm=TRUE)) / sd(val, na.rm=TRUE), by=series]
  tsl[, series := factor(as.vector(nice(as.character(series))), levels=as.vector(nice(M4v)))]
  base <- ggplot(tsl, aes(date, z)) +
    geom_vline(xintercept=as.Date(c("2018-01-01","2020-01-01","2022-01-01")), linetype="dotted", colour="grey60") +
    geom_hline(yintercept=0, colour="grey85") +
    labs(x=NULL, y="Standardized quarterly mean (z-score)") + theme_thesis()
  pc <- base + geom_line(aes(colour=series), linewidth=0.6) + scale_colour_manual(values=PASTEL[1:4])
  pb <- base + geom_line(aes(linetype=series), colour="black", linewidth=0.5)
  save_both("fig_exposure_series.pdf", pc, pb) })

## Fig 1b -- same four measures over time, faceted (raw levels, own y-scale per panel)
local({
  ex <- readRDS(file.path(root, "out", "exposure", "exposure_firmquarter_ric.rds")); setDT(ex)
  M4v <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
  ts <- ex[!is.na(year) & !is.na(quarter), lapply(.SD, mean, na.rm=TRUE), .SDcols=M4v,
           by = .(date = as.Date(sprintf("%d-%02d-01", year, (quarter-1)*3+1)))][order(date)]
  tsl <- melt(ts, id.vars="date", variable.name="series", value.name="val")
  tsl[, series := factor(as.vector(nice(as.character(series))), levels=as.vector(nice(M4v)))]
  scols <- setNames(PASTEL[1:4], as.vector(nice(M4v)))   # same colour per measure as the combined plots
  base <- ggplot(tsl, aes(date, val)) + facet_wrap(~series, scales="free_y", nrow=2) +
    geom_vline(xintercept=as.Date(c("2018-01-01","2020-01-01","2022-01-01")), linetype="dotted", colour="grey60") +
    labs(x=NULL, y="Cross-firm quarterly mean (raw level)") + theme_thesis() + theme(legend.position="none")
  pc <- base + geom_line(aes(colour=series), linewidth=0.6) + scale_colour_manual(values=scols)
  pb <- base + geom_line(colour="black", linewidth=0.5)
  save_both("fig_exposure_series_facet.pdf", pc, pb, h=4.2) })

## Fig 2 -- event-study CAR by exposure quintile (GeoRisk, market model)
local({
  es <- J("event_study.json"); d <- es$results$GeoRisk$car_mm; setDT(d)
  d[, Quintile := factor(q, levels=1:5, labels=c("Q1 (low)","Q2","Q3","Q4","Q5 (high)"))]
  base <- ggplot(d, aes(k, car*100, group=Quintile)) +
    geom_hline(yintercept=0, colour="grey75") + geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
    labs(x="Trading days relative to the earnings call", y="CAR (%)") + theme_thesis()
  pc <- base + geom_line(aes(colour=Quintile), linewidth=0.7) + scale_colour_manual(values=PASTEL5)
  pb <- base + geom_line(aes(linetype=Quintile), colour="black", linewidth=0.5)
  save_both("fig_event_study_car.pdf", pc, pb) })

## Fig 2b -- CAAR grid: four measures, high (Q5) vs low (Q1) quintile, market model.
## Count/risk high quintile drifts down; GeoSentiment's high quintile rises (good news read at the call).
local({
  es  <- J("event_study.json")$results
  mm  <- c("GeoExposure","dCC","GeoRisk","GeoSentiment")
  lab <- c(GeoExposure="GeoExposure", dCC="Change in GeoExposure", GeoRisk="GeoRisk", GeoSentiment="GeoSentiment")
  d <- rbindlist(lapply(mm, function(m){ x <- as.data.table(es[[m]]$car_mm); x <- x[q %in% c(1,5)]
    x[, measure := factor(lab[m], levels=unname(lab))]
    x[, grp := factor(ifelse(q==5,"Q5 (high)","Q1 (low)"), levels=c("Q5 (high)","Q1 (low)"))]; x }))
  base <- ggplot(d, aes(k, 100*car, colour=grp)) + facet_wrap(~measure, scales="free_y", nrow=2) +
    geom_hline(yintercept=0, colour="grey80") + geom_vline(xintercept=0, linetype="dotted", colour="grey60") +
    labs(x="Trading days from the call", y="CAAR (%)") + theme_thesis()
  pc <- base + geom_line(linewidth=0.75) + scale_colour_manual(values=c("Q5 (high)"="#4A88C7","Q1 (low)"="#B0B0B0"), name=NULL)
  pb <- base + geom_line(aes(linetype=grp), colour="black", linewidth=0.5) +
    scale_linetype_manual(values=c("Q5 (high)"="solid","Q1 (low)"="22"), name=NULL)
  save_both("fig_q1_caar_grid.pdf", pc, pb, w=7, h=3.6) })   # short aspect: table+figure share one page

## Fig 3 -- GeoRisk FF5 alpha across factor models, with 95% CI (spanning)
local({
  mo <- asp$crsp$models
  df <- rbindlist(lapply(names(mo), function(k) data.table(
    model = k, alpha = mo[[k]]$alpha, t = mo[[k]]$t)))
  df[, se := abs(alpha / t)][, `:=`(lo = alpha - 1.96*se, hi = alpha + 1.96*se)]
  df[, model := factor(model, levels = names(mo),
        labels = c("FF5","FF5+UMD","FF5+GPR+EPU","Augmented"))]
  base <- ggplot(df, aes(model, alpha*100)) + geom_hline(yintercept=0, colour="grey75") +
    geom_errorbar(aes(ymin=lo*100, ymax=hi*100), width=0.15, colour="grey45") +
    labs(x=NULL, y=expression(paste("Monthly ", alpha, " (%)"))) + theme_thesis() +
    theme(legend.position="none")
  pc <- base + geom_point(size=3, colour=PASTEL[4])
  pb <- base + geom_point(size=3, colour="black")
  save_both("fig_spanning_alpha.pdf", pc, pb, h=3.2) })

# =====================================================================
# 3b. RQ1 EXTRA FIGURES -- richer realization visuals from the firm-quarter panel
#     (contemporaneous return = QuarterlyRet_W, winsorised, matches tab:q1)
# =====================================================================
PN <- tryCatch(as.data.table(readRDS(file.path(ana, "panel_ric_min.rds"))), error=function(e) NULL)
if (is.null(PN)) cat("panel_ric_min.rds not found -- skipping RQ1 extra figures\n") else {
  setorder(PN, Ticker, Quarter)
  PN[, dGeoExposure  := GeoExposure  - shift(GeoExposure),  by = Ticker]  # within-firm change in geo-talk
  RET <- "QuarterlyRet_W"
  # 1-digit SIC division names (readable sector buckets)
  sic_div <- function(s){ s <- as.integer(s)
    data.table::fifelse(is.na(s), NA_character_,
    data.table::fifelse(s<1000,"Agriculture",
    data.table::fifelse(s<1500,"Mining",
    data.table::fifelse(s<1800,"Construction",
    data.table::fifelse(s<4000,"Manufacturing",
    data.table::fifelse(s<5000,"Transport/Utilities",
    data.table::fifelse(s<5200,"Wholesale",
    data.table::fifelse(s<6000,"Retail",
    data.table::fifelse(s<6800,"Finance",
    data.table::fifelse(s<9000,"Services","Public Admin.")))))))))) }

  ## Fig R1 -- dose-response: contemporaneous return across change-in-talk deciles
  local({
    d <- PN[is.finite(dGeoExposure) & is.finite(get(RET))]
    d[, dec := ceiling(frank(dGeoExposure, ties.method="first")/.N*10)]
    g <- d[, .(ret = 100*mean(get(RET), na.rm=TRUE)), by=dec][order(dec)]
    base <- ggplot(g, aes(dec, ret)) + geom_hline(yintercept=0, colour="grey75") +
      scale_x_continuous(breaks=1:10) +
      labs(x="Decile of change in geoeconomic talk (low to high)",
           y="Mean contemporaneous return (%)") + theme_thesis() + theme(legend.position="none")
    pc <- base + geom_col(fill=PASTEL[1], width=0.8)
    pb <- base + geom_col(fill="grey65", width=0.8)
    save_both("fig_q1_dose_response.pdf", pc, pb, h=3.4) })

  ## Fig R2 -- the sign flip: good-news vs bad-news geoeconomic tone at the call
  local({
    mk <- function(col, lab){ d <- PN[is.finite(get(col)) & is.finite(get(RET)) & get(col)>0]
      d[, dec := ceiling(frank(get(col), ties.method="first")/.N*10)]
      d[, .(ret=100*mean(get(RET),na.rm=TRUE), leg=lab), by=dec][order(dec)] }
    g <- rbind(mk("GeoSentimentPos","Positive-sentiment geo content"),
               mk("GeoSentimentNeg","Negative-sentiment geo content"))
    base <- ggplot(g, aes(dec, ret, group=leg)) + geom_hline(yintercept=0, colour="grey75") +
      scale_x_continuous(breaks=1:10) +
      labs(x="Decile of tone-specific geoeconomic content (low to high)",
           y="Mean contemporaneous return (%)") + theme_thesis()
    pc <- base + geom_line(aes(colour=leg), linewidth=0.8) + geom_point(aes(colour=leg), size=1.6) +
      scale_colour_manual(values=c("Positive-sentiment geo content"=PASTEL[3], "Negative-sentiment geo content"=PASTEL[2]))
    pb <- base + geom_line(aes(linetype=leg), colour="black", linewidth=0.6) + geom_point(colour="black", size=1.2)
    save_both("fig_q1_tone_split.pdf", pc, pb, h=3.6) })

  ## Fig R3 -- RQ1 coefficient forest (contemp return + IVOL + total vol), four measures, 95% CI
  local({
    rr <- q1$results; vv <- v1; tv <- J("volatility_q1.json")$tvol
    df <- rbindlist(lapply(M4, function(m){ a<-rr[rr$measure==m,]; b<-vv[vv$measure==m,]; d<-tv[tv$measure==m,]
      rbind(data.table(measure=as.vector(nice(m)), panel="Contemp. return",     coef=a$ret_contemp$coef, t=a$ret_contemp$t),
            data.table(measure=as.vector(nice(m)), panel="Idiosyncratic vol.",  coef=b$coef,             t=b$t),
            data.table(measure=as.vector(nice(m)), panel="Total vol.",          coef=d$coef,             t=d$t)) }))
    df[, se := abs(coef/t)][, `:=`(lo=coef-1.96*se, hi=coef+1.96*se)]
    df[, measure := factor(measure, levels=rev(as.vector(nice(M4))))]
    df[, panel := factor(panel, levels=c("Contemp. return","Idiosyncratic vol.","Total vol."))]
    base <- ggplot(df, aes(100*coef, measure)) + facet_wrap(~panel, scales="free_x") +
      geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
      geom_errorbarh(aes(xmin=100*lo, xmax=100*hi), height=0.15, colour="grey45") +
      labs(x="Coefficient (%, per one-SD change)", y=NULL) + theme_thesis() + theme(legend.position="none")
    pc <- base + geom_point(size=2.6, colour=PASTEL[4])
    pb <- base + geom_point(size=2.6, colour="black")
    save_both("fig_q1_coef_forest.pdf", pc, pb, w=7.4, h=3.2) })

  ## Fig R4 -- heatmap of each measure: SIC division x year (x1000). Sequential blue for the
  ## non-negative measures; diverging (red/blue around 0) for GeoSentiment, which can be negative.
  heat_measure <- function(measure, fname, label, diverging=FALSE){
    d <- PN[is.finite(get(measure)) & !is.na(siccd)]; d[, div := sic_div(siccd)]; d <- d[!is.na(div)]
    keep <- d[, .N, by=div][N >= 500L, div]; d <- d[div %in% keep]   # drop divisions <500 firm-quarters (as the sector table does)
    h <- d[, .(v = 1000*mean(get(measure), na.rm=TRUE)), by=.(div, year)]
    ord <- h[, .(m=mean(v, na.rm=TRUE)), by=div][order(m)]$div
    h[, div := factor(div, levels=ord)]
    base <- ggplot(h, aes(year, div, fill=v)) + geom_tile(colour="white", linewidth=0.3) +
      labs(x=NULL, y=NULL) + theme_thesis() +
      theme(panel.grid=element_blank(), legend.position="right", legend.title=element_text(size=9))
    if (diverging) {
      pc <- base + scale_fill_gradient2(low="#767676", mid="#F7F7F7", high="#1F4E79", midpoint=0, name=label)
      pb <- base + scale_fill_gradient2(low="grey35", mid="grey96", high="black",   midpoint=0, name=label)
    } else {
      pc <- base + scale_fill_gradient(low="#EAF2FB", high="#1F4E79", name=label)
      pb <- base + scale_fill_gradient(low="grey92",  high="grey15",  name=label)
    }
    save_both(fname, pc, pb, w=6.8, h=3.8) }
  heat_measure("GeoExposure",      "fig_q1_exposure_heatmap.pdf",  "Mean\nGeoExposure")
  heat_measure("GeoExposureTFIDF", "fig_q1_tfidf_heatmap.pdf",     "Mean\nGeoExpo.\n(TF-IDF)")
  heat_measure("GeoRisk",          "fig_q1_sector_heatmap.pdf",    "Mean\nGeoRisk")
  heat_measure("GeoSentiment",     "fig_q1_sentiment_heatmap.pdf", "Mean\nGeoSentiment", diverging=TRUE)

  ## Fig R5 -- the realized drop by sector: high-minus-low change-in-talk contemp return
  local({
    d <- PN[is.finite(dGeoExposure) & is.finite(get(RET)) & !is.na(siccd)]
    d[, div := sic_div(siccd)]; d <- d[!is.na(div)]
    qh <- quantile(d$dGeoExposure, .8, na.rm=TRUE); ql <- quantile(d$dGeoExposure, .2, na.rm=TRUE)
    d[, grp := data.table::fifelse(dGeoExposure>=qh, "high", data.table::fifelse(dGeoExposure<=ql, "low", NA_character_))]
    ndiv <- d[, .(Ntot=.N), by=div]                                  # drop thin/noisy divisions
    dd <- d[!is.na(grp), .(ret=mean(get(RET), na.rm=TRUE), n=.N), by=.(div, grp)]
    w <- merge(dcast(dd, div~grp, value.var="ret"), ndiv, by="div"); w[, drop := 100*(high - low)]
    w <- w[is.finite(drop) & Ntot >= 500][order(drop)]; w[, div := factor(div, levels=div)]
    base <- ggplot(w, aes(drop, div)) + geom_vline(xintercept=0, colour="grey70") +
      labs(x="High-minus-low change-in-talk contemporaneous return (%)", y=NULL) +
      theme_thesis() + theme(legend.position="none")
    pc <- base + geom_col(fill=PASTEL[2], width=0.7)
    pb <- base + geom_col(fill="grey55", width=0.7)
    save_both("fig_q1_drop_by_sector.pdf", pc, pb, h=3.8) })

  ## Fig R6 -- binscatter behind the headline coefficient (raw relationship, 20 bins)
  local({
    d <- PN[is.finite(dGeoExposure) & is.finite(get(RET))]
    d[, b := ceiling(frank(dGeoExposure, ties.method="first")/.N*20)]
    g <- d[, .(x=mean(dGeoExposure, na.rm=TRUE), y=100*mean(get(RET), na.rm=TRUE)), by=b][order(b)]
    base <- ggplot(g, aes(x, y)) + geom_hline(yintercept=0, colour="grey80") +
      geom_smooth(method="lm", se=FALSE, colour="grey45", linewidth=0.6) +
      labs(x="Change in geoeconomic talk (binned)", y="Mean contemporaneous return (%)") +
      theme_thesis() + theme(legend.position="none")
    pc <- base + geom_point(size=2, colour=PASTEL[1])
    pb <- base + geom_point(size=2, colour="black")
    save_both("fig_q1_binscatter.pdf", pc, pb, h=3.4) })

  cat("RQ1 extra figures OK\n")

  # ---- Descriptive-statistics artefacts (Section 4.1) -----------------
  ## Table -- summary statistics of the four measures (Sautner Table I style; x1000)
  local({
    SC <- 1000
    st <- rbindlist(lapply(M4, function(m){ x <- PN[[m]]; x <- x[is.finite(x)]
      data.table(Measure=as.vector(nice(m)), N=length(x), Mean=SC*mean(x), SD=SC*sd(x),
                 P10=SC*quantile(x,.10), Med=SC*median(x), P90=SC*quantile(x,.90), NZ=100*mean(x>0)) }))
    rows <- st[, sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\", Measure,
        formatC(N, big.mark=",", format="d"), fnum(Mean,2), fnum(SD,2), fnum(P10,2), fnum(Med,2), fnum(P90,2), fnum(NZ,1))]
    writetab("tab_summary.tex", tabular("lccccccc",
      c("Measure & $N$ & Mean & SD & p10 & Median & p90 & \\% $>0$\\\\"), rows)) })
    # NB: the Section 4.1 inline summary numbers are computed live in the .Rnw setup
    # chunk (SUM$...) and spliced via \Sexpr, matching how GPR_RHO etc. work here;
    # numbers.tex is not \input by the thesis, so no macros are written for them.

  ## Table -- geoeconomic exposure by sector (1-digit SIC division), ALL FOUR measures, time-averaged (x1000)
  st_sector <- function(minN, file){
    d <- PN[!is.na(siccd)]; d[, div := sic_div(siccd)]; d <- d[!is.na(div)]
    s <- d[, .(N=.N, Exp=1000*mean(GeoExposure, na.rm=TRUE), Tf=1000*mean(GeoExposureTFIDF, na.rm=TRUE),
               Rk=1000*mean(GeoRisk, na.rm=TRUE), St=1000*mean(GeoSentiment, na.rm=TRUE)),
           by=div][N>=minN][order(-Exp)]
    rows <- s[, sprintf("%s & %s & %s & %s & %s & %s \\\\", div,
        formatC(N, big.mark=",", format="d"), fnum(Exp,2), fnum(Tf,2), fnum(Rk,2), fnum(St,2))]
    writetab(file, tabular("lccccc",
      c("Sector (SIC division) & $N$ & GeoExposure & GeoExposure (TF-IDF) & GeoRisk & GeoSentiment\\\\"), rows)) }
  st_sector(0,   "tab_exposure_by_sector.tex")
  st_sector(500, "tab_exposure_by_sector_maj.tex")

  ## Figure -- distribution (density) of the four measures, log scale, positive values
  local({
    dd <- melt(PN[, c("Ticker", M4), with=FALSE], id.vars="Ticker", variable.name="measure", value.name="val")
    dd <- dd[is.finite(val) & val>0]; dd[, measure := factor(as.vector(nice(as.character(measure))), levels=as.vector(nice(M4)))]
    pc <- ggplot(dd, aes(val, fill=measure, colour=measure)) + geom_density(alpha=0.35, linewidth=0.4) +
      scale_x_log10() + scale_fill_manual(values=PASTEL[1:4]) + scale_colour_manual(values=PASTEL[1:4]) +
      labs(x="Measure value (log scale, positive firm-quarters)", y="Density") + theme_thesis()
    pb <- ggplot(dd, aes(val, linetype=measure)) + geom_density(colour="black", linewidth=0.5) +
      scale_x_log10() + labs(x="Measure value (log scale, positive firm-quarters)", y="Density") + theme_thesis()
    save_both("fig_distribution.pdf", pc, pb, h=3.4) })

  ## Figure -- same distributions in RAW LEVELS, one panel per measure (shows the true
  ## right-skew, GeoRisk's zero pile-up, and GeoSentiment's negative mass; aligns with the text)
  local({
    dd <- melt(PN[, c("Ticker", M4), with=FALSE], id.vars="Ticker", variable.name="measure", value.name="val")
    dd <- dd[is.finite(val)]; dd[, val := 1000*val]
    dd[, measure := factor(as.vector(nice(as.character(measure))), levels=as.vector(nice(M4)))]
    mcols <- setNames(PASTEL[1:4], as.vector(nice(M4)))   # same colour per measure as the combined density
    base <- ggplot(dd, aes(val)) + facet_wrap(~measure, scales="free", nrow=2) +
      geom_vline(xintercept=0, colour="grey80", linewidth=0.3) +
      labs(x=expression(paste("Measure value (", {}%*%{}, 10^3, ", all firm-quarters)")), y="Density") +
      theme_thesis() + theme(legend.position="none")
    pc <- base + geom_density(aes(fill=measure, colour=measure), alpha=0.5, linewidth=0.4) +
      scale_fill_manual(values=mcols) + scale_colour_manual(values=mcols)
    pb <- base + geom_density(fill="grey80", colour="black", alpha=0.6, linewidth=0.4)
    save_both("fig_distribution_facet.pdf", pc, pb, h=4.0) })

  cat("descriptive artefacts OK\n")
}

# =====================================================================
# 4. RQ2 ARTEFACTS (pricing): FM forest, quintile bars, tone-legs fig; not-PEAD + tone-decomp tables
# =====================================================================
local({
  ## Fig q2 forest -- FM price of risk by measure (LSEG quarterly univariate); only GeoSentiment clears 0
  fm <- J("fama_macbeth_ric.json")$results
  df <- rbindlist(lapply(M4, function(m){ u <- fm$univariate[fm$measure==m,]
    data.table(measure=as.vector(nice(m)), lam=u$lambda_q, t=u$nw_t) }))
  df[, se := abs(lam/t)][, `:=`(lo=lam-1.96*se, hi=lam+1.96*se)][, clears := (lo*hi)>0]
  df[, measure := factor(measure, levels=rev(as.vector(nice(M4))))]
  mcols <- setNames(PASTEL[1:4], as.vector(nice(M4)))
  base <- ggplot(df, aes(100*lam, measure, colour=measure)) + geom_vline(xintercept=0, linetype="dashed", colour="grey55") +
    geom_errorbarh(aes(xmin=100*lo, xmax=100*hi), height=0.16, linewidth=0.9) +
    geom_point(size=3) +
    labs(x=expression(paste(lambda, "  (%/quarter, LSEG)")), y=NULL) + theme_thesis() + theme(legend.position="none")
  save_both("fig_q2_forest.pdf",
    base + scale_colour_manual(values=mcols),
    base + scale_colour_manual(values=setNames(GREY5[1:4], as.vector(nice(M4)))), h=2.2)

  ## Fig q2 quintile -- next-quarter return by exposure quintile, all four measures (raw sorts)
  PNq <- as.data.table(readRDS(file.path(ana, "panel_ric_min.rds")))
  qd <- rbindlist(lapply(M4, function(m){ d <- PNq[is.finite(get(m)) & is.finite(Ret_lead)]
    d[, q := ceiling(frank(get(m), ties.method="first")/.N*5), by=Quarter]
    qm <- d[, .(r=mean(Ret_lead, na.rm=TRUE)), by=.(Quarter,q)][, .(ret=100*mean(r, na.rm=TRUE)), by=q][order(q)]
    data.table(measure=factor(as.vector(nice(m)), levels=as.vector(nice(M4))),
               Q=factor(paste0("Q",qm$q), levels=paste0("Q",1:5)), ret=qm$ret) }))
  baseq <- ggplot(qd, aes(Q, ret, fill=Q)) + geom_col(width=0.78) + facet_wrap(~measure, nrow=2) +
    labs(x="Exposure quintile", y="Mean next-quarter return (%)") + theme_thesis() + theme(legend.position="none")
  save_both("fig_q2_quintile.pdf", baseq + scale_fill_manual(values=PASTEL5),
                                    baseq + scale_fill_manual(values=GREY5), w=6.4, h=5.0)

  ## Fig q2 tone legs -- GeoSentiment / Pos / Neg forward t, Fama-MacBeth vs panel FE (the underreaction split)
  sd <- J("sentiment_decomp.json")$results
  legs <- c(GeoSentiment="GeoSentiment", GeoSentimentPos="Positive sentiment", GeoSentimentNeg="Negative sentiment")
  td <- rbindlist(lapply(names(legs), function(k){ x <- sd[[k]]
    rbind(data.table(leg=legs[k], est="Fama--MacBeth", t=x$fm_q_lseg$t),
          data.table(leg=legs[k], est="Panel FE",      t=x$fe_q_lseg$t)) }))
  td[, leg := factor(leg, levels=unname(legs))]
  baset <- ggplot(td, aes(leg, t, fill=est)) + geom_col(position="dodge", width=0.7) +
    geom_hline(yintercept=c(-1.96,1.96), linetype="dashed", colour="grey55") + geom_hline(yintercept=0, colour="grey70") +
    labs(x=NULL, y="t-statistic (next-quarter return)") + theme_thesis()
  save_both("fig_q2_tone_legs.pdf",
    baset + scale_fill_manual(values=c("Fama--MacBeth"=PASTEL[4], "Panel FE"=PASTEL[1]), name=NULL),
    baset + scale_fill_manual(values=c("Fama--MacBeth"="grey25", "Panel FE"="grey65"), name=NULL), h=3.4)
  ## FM-only version for the main text (the panel-FE comparison lives in the appendix)
  basef <- ggplot(td[est=="Fama--MacBeth"], aes(leg, t)) +
    geom_hline(yintercept=c(-1.96,1.96), linetype="dashed", colour="grey55") + geom_hline(yintercept=0, colour="grey70") +
    labs(x=NULL, y="Fama--MacBeth t-statistic (next-quarter return)") + theme_thesis()
  save_both("fig_q2_tone_legs_fm.pdf",
    basef + geom_col(fill=PASTEL[4], width=0.62),
    basef + geom_col(fill="grey45",  width=0.62), h=3.2)

  ## Table q2 not-PEAD (A10): t(GeoSentiment) under +announcement / +reversal / +both
  pcd <- J("pead_control.json"); cols <- c("baseline","plus_announcement","plus_reversal","plus_both")
  rfe <- sprintf("Panel FE & %s \\\\", paste(vapply(cols, function(c) fnum(pcd$sentiment_fe[[c]]$t), character(1)), collapse=" & "))
  rfm <- sprintf("Fama--MacBeth & %s \\\\", paste(vapply(cols, function(c) fnum(pcd$sentiment_fm[[c]]$t), character(1)), collapse=" & "))
  writetab("tab_q2_pead.tex", tabular("lcccc",
    c("$t$(GeoSentiment) & Baseline & $+$ ann.\\ return & $+$ reversal & $+$ both\\\\"), c(rfe, rfm)))

  ## Table q2 tone-decomp (A10b): FM lambda + panel-FE coef for GeoSentiment / Pos / Neg
  legs2 <- c(GeoSentiment="GeoSentiment $=$ Pos $-$ Neg", GeoSentimentPos="Positive-sentiment leg", GeoSentimentNeg="Negative-sentiment leg")
  rows2 <- vapply(names(legs2), function(k){ x <- sd[[k]]
    sprintf("%s & %s & %s \\\\", legs2[k], fcell(x$fm_q_lseg$lambda, x$fm_q_lseg$t), fcell(x$fe_q_lseg$coef, x$fe_q_lseg$t)) }, character(1))
  writetab("tab_q2_tone_decomp.tex", tabular("lcc",
    c("Leg & FM $\\lambda$~$(t)$ & Panel FE coef~$(t)$\\\\"), rows2))
  ## FM-only tone decomp for the main text (the FM+FE version above is the appendix table)
  rows2f <- vapply(names(legs2), function(k){ x <- sd[[k]]
    sprintf("%s & %s \\\\", legs2[k], fcell(x$fm_q_lseg$lambda, x$fm_q_lseg$t)) }, character(1))
  writetab("tab_q2_tone_decomp_fm.tex", tabular("lc", c("Leg & FM $\\lambda$~$(t)$\\\\"), rows2f))
  ## Table q2 size: GeoSentiment forward premium, small vs large firms (underreaction test)
  sz <- J("defense_cuts.json")$sentiment_underreaction_by_size
  szlab <- c(GeoSentiment="GeoSentiment (net)", Pos="Positive-sentiment leg")
  rows_sz <- vapply(names(szlab), function(k){ x <- sz[[k]]
    sprintf("%s & %s & %s \\\\", szlab[k], fcell(x$small$lambda, x$small$t), fcell(x$large$lambda, x$large$t)) }, character(1))
  writetab("tab_q2_size.tex", tabular("lcc",
    c("Measure & Small firms & Large firms\\\\", " & $\\lambda$~$(t)$ & $\\lambda$~$(t)$\\\\"), rows_sz))
  cat("RQ2 artefacts OK\n")
})

# =====================================================================
# 5. RQ3 / ROBUSTNESS / OOS ARTEFACTS (loadings, characteristics, region; illiquidity table; OOS bars)
# =====================================================================
local({
  M <- J("results_master.json"); asp <- J("augmented_spanning.json")
  ## Fig -- macro-factor loadings of the GeoRisk L/S (A13): no macro factor spans the alpha
  ld <- asp$crsp$loadings
  flab <- c(dGPR="GPR", dGPRT="GPR threats", dEPU="EPU", dVIX="VIX", oilret="Oil", usdret="USD", UMD="Momentum")
  gett <- function(z){ z<-unlist(z); if(!is.null(names(z)) && "t" %in% names(z)) as.numeric(z["t"]) else as.numeric(z)[2] }
  lddt <- rbindlist(lapply(intersect(names(ld), names(flab)), function(fn) data.table(fac=flab[fn], t=gett(ld[[fn]]))))
  lddt[, fac:=factor(fac, levels=flab)][, sig:=abs(t)>=1.96]
  bl <- ggplot(lddt, aes(fac, t, fill=sig)) + geom_col(width=0.7) +
    geom_hline(yintercept=c(-1.96,1.96), linetype="dashed", colour="grey55") + geom_hline(yintercept=0, colour="grey70") +
    labs(x=NULL, y="t-statistic of loading") + theme_thesis() + theme(legend.position="none")
  save_both("fig_q3_macro_loadings.pdf",
    bl + scale_fill_manual(values=c(`FALSE`="grey72", `TRUE`=PASTEL[4])),
    bl + scale_fill_manual(values=c(`FALSE`="grey72", `TRUE`="black")), h=3.0)

  ## Fig -- GeoRisk + GeoSentiment L/S FF5 alpha within size / B-M / momentum terciles (A14)
  clab <- c(size="Size", bm="Book-to-Market", momentum="Momentum"); tlab <- c(T1="Low", T2="Mid", T3="High")
  chr <- rbindlist(lapply(c("GeoRisk","GeoSentiment"), function(mm) rbindlist(lapply(names(clab), function(ch) rbindlist(lapply(names(tlab), function(g){
    x <- as.numeric(unlist(M$characteristics[[ch]][[mm]][[g]]))
    data.table(measure=mm, char=clab[ch], ter=tlab[g], alpha=x[1], t=x[2]) }))))))
  chr <- chr[is.finite(alpha)]
  chr[, char:=factor(char, levels=clab)][, ter:=factor(ter, levels=tlab)][, measure:=factor(measure, levels=c("GeoRisk","GeoSentiment"))]
  bc <- ggplot(chr, aes(ter, 100*alpha, fill=char)) + geom_col(position="dodge", width=0.72) +
    facet_wrap(~measure, ncol=1) +
    geom_hline(yintercept=0, colour="grey60") + labs(x="Characteristic tercile (low to high)", y="FF5 alpha (%/quarter)") + theme_thesis()
  save_both("fig_q3_characteristics.pdf",
    bc + scale_fill_manual(values=PASTEL[1:3], name=NULL),
    bc + scale_fill_manual(values=c("grey30","grey55","grey78"), name=NULL), h=5.0)

  ## Fig -- GeoRisk quintile next-quarter return by region (A15)
  rlab <- c(NorthAmerica="North America", Europe="Europe", AsiaPacific_DM="Asia-Pacific (DM)", EM="Emerging Markets")
  rg <- as.data.table(M$region$GeoRisk$means); rg <- rg[Region %in% names(rlab)]
  rg[, Region:=factor(rlab[Region], levels=rlab)][, Q:=factor(paste0("Q",q), levels=paste0("Q",1:5))]
  br <- ggplot(rg, aes(Q, 100*ew, fill=Q)) + geom_col(width=0.78) + facet_wrap(~Region, nrow=2) +
    labs(x="Risk-exposure quintile", y="Mean next-quarter return (%)") + theme_thesis() + theme(legend.position="none")
  save_both("fig_q3_region.pdf", br + scale_fill_manual(values=PASTEL5), br + scale_fill_manual(values=GREY5), w=6.4, h=4.6)

  ## Table -- illiquidity + reversal absorb much of the alpha (A16)
  ef <- J("extra_factors.json"); mods <- c(M0_FF5="FF5", M1_FF5_UMD="$+$ UMD", M2_FF5_BAB_QMJ="$+$ BAB, QMJ", M3_FF5_ILLIQ_REV="$+$ illiq, rev", M4_all="$+$ all")
  cellm <- function(v){ v<-as.numeric(v); fcell(v[1], v[2]) }
  rC <- sprintf("US (CRSP) & %s \\\\",     paste(vapply(names(mods), function(k) cellm(ef$crsp[[k]]), character(1)), collapse=" & "))
  rL <- sprintf("Global (LSEG) & %s \\\\", paste(vapply(names(mods), function(k) cellm(ef$lseg[[k]]), character(1)), collapse=" & "))
  writetab("tab_q3_illiq.tex", tabular("lccccc",
    c(" & \\multicolumn{5}{c}{GeoRisk L/S alpha (\\%/month), $t$}\\\\", "\\cmidrule(lr){2-6}",
      paste("Sample &", paste(unname(mods), collapse=" & "), "\\\\")), c(rC, rL)))

  ## Fig -- OOS: in-sample vs walk-forward OOS (fixed dictionary), GeoRisk + GeoSentiment
  rt <- J("realtime_dict_oos.json")$crsp_realtime
  oo <- rbindlist(lapply(c("GeoRisk","GeoSentiment"), function(m) rbind(
    data.table(measure=m, kind="In-sample", t=rt[[m]]$fixed$t),
    data.table(measure=m, kind="Walk-forward OOS", t=rt[[m]]$fixed$oos_t))))
  oo[, kind:=factor(kind, levels=c("In-sample","Walk-forward OOS"))]
  bo <- ggplot(oo, aes(measure, t, fill=kind)) + geom_col(position="dodge", width=0.68) +
    geom_hline(yintercept=1.96, linetype="dashed", colour="grey55") + geom_hline(yintercept=0, colour="grey70") +
    labs(x=NULL, y="t-statistic of FF5 alpha (CRSP)") + theme_thesis()
  save_both("fig_q3_oos.pdf",
    bo + scale_fill_manual(values=c("In-sample"=PASTEL[4], "Walk-forward OOS"=PASTEL[1]), name=NULL),
    bo + scale_fill_manual(values=c("In-sample"="grey25", "Walk-forward OOS"="grey65"), name=NULL), h=3.2)

  ## Fig -- real-time vs full-sample dictionary (look-ahead): GeoSentiment survives, GeoRisk sensitive
  dd <- rbindlist(lapply(c("GeoRisk","GeoSentiment"), function(m) rbind(
    data.table(measure=m, dict="Full-sample dict.", t=rt[[m]]$fixed$t),
    data.table(measure=m, dict="Real-time dict.",   t=rt[[m]]$realtime$t))))
  dd[, dict:=factor(dict, levels=c("Full-sample dict.","Real-time dict."))]
  bd <- ggplot(dd, aes(measure, t, fill=dict)) + geom_col(position="dodge", width=0.68) +
    geom_hline(yintercept=1.96, linetype="dashed", colour="grey55") + geom_hline(yintercept=0, colour="grey70") +
    labs(x=NULL, y="t-statistic of FF5 alpha (CRSP)") + theme_thesis()
  save_both("fig_q3_realtime.pdf",
    bd + scale_fill_manual(values=c("Full-sample dict."=PASTEL[4], "Real-time dict."=PASTEL[2]), name=NULL),
    bd + scale_fill_manual(values=c("Full-sample dict."="grey25", "Real-time dict."="grey65"), name=NULL), h=3.2)
  ## Table -- OOS summary: all three questions, headline in-sample vs the SAME strict test used in the main text
  ## (re-learned dictionary for the RQ2/RQ3 strategies; split-half for the contemporaneous RQ1). Consistent
  ## with Tables 4.8/4.9: in-sample column is the headline t, so no walk-forward-only figures appear here.
  os <- J("oos_strategy.json"); rtd <- J("realtime_dict_oos.json")$crsp_realtime
  sp <- os$rq1_realization_split; f2 <- os$rq2_fm_split$GeoSentiment
  inT <- function(meas) { r <- cm$q3[cm$q3$measure == meas, ]; r$ew_t }   # headline in-sample t (Table 4.8)
  lb <- c(GeoExposure="GeoExposure", GeoExposureTFIDF="GeoExposure (TF-IDF)",
          GeoRisk="GeoRisk", GeoSentiment="GeoSentiment")
  supL <- function(x) paste0(x, "$^{\\mathrm{L}}$"); supC <- function(x) paste0(x, "$^{\\mathrm{C}}$")
  rows_oos <- c(
    vapply(names(lb), function(m) sprintf("RQ1 realisation, %s & split-half & %s & %s \\\\",
           lb[m], supL(fnum(sp[[m]]$H1$t)), supL(fnum(sp[[m]]$H2$t))), character(1)),
    "\\midrule",
    sprintf("RQ2 tone pricing, FM $\\lambda$ (GeoSentiment) & split-half & %s & %s \\\\",
            supL(fnum(f2$H1$t)), supL(fnum(f2$H2$t))),
    "\\midrule",
    sprintf("RQ3 GeoRisk L/S (FF5 $\\alpha$) & re-learned dict.\\ & %s & %s \\\\", supC(fnum(inT("GeoRisk"))), supC(fnum(rtd$GeoRisk$realtime$t))),
    sprintf("RQ3 GeoSentiment L/S (FF5 $\\alpha$) & re-learned dict.\\ & %s & %s \\\\", supC(fnum(inT("GeoSentiment"))), supC(fnum(rtd$GeoSentiment$realtime$t))))
  writetab("tab_oos.tex", tabular("llcc",
    c(" & & \\multicolumn{2}{c}{$t$-statistic}\\\\", "\\cmidrule(lr){3-4}",
      "Research question & Design & In-sample~/~H1 & OOS~/~H2\\\\"), rows_oos))
  ## Table -- RQ3 rediscovery OOS: headline in-sample (same source as Table 4.8) vs re-learned dictionary, EW L/S, CRSP
  rd  <- J("realtime_dict_oos.json")$crsp_realtime
  inEW <- function(meas) { r <- cm$q3[cm$q3$measure == meas, ]; fcell(r$ew_alpha, r$ew_t) }  # identical to Table 4.8
  rows_rd <- c(
    sprintf("GeoRisk & %s & %s \\\\", inEW("GeoRisk"), fcell(rd$GeoRisk$realtime$alpha, rd$GeoRisk$realtime$t)),
    sprintf("GeoSentiment & %s & %s \\\\", inEW("GeoSentiment"), fcell(rd$GeoSentiment$realtime$alpha, rd$GeoSentiment$realtime$t)))
  writetab("tab_q3_rediscovery.tex", tabular("lcc",
    c("Measure & In-sample $\\alpha$~$(t)$ & OOS, re-learned dictionary $\\alpha$~$(t)$\\\\"), rows_rd))
  cat("RQ3/robustness/OOS artefacts OK\n")
})

# =====================================================================
# 6. VALIDATION + WHOLE-PICTURE FIGURES (appendix)
# =====================================================================

## GPR external validation -- quarterly means of ALL FOUR measures vs the Caldara-
## Iacoviello GPR threats index, everything z-scored on its own history. The GPR
## series is drawn as a shaded background band so the four measure lines stay
## readable; each legend entry carries its own level correlation with GPR threats.
## Same file as the GPR_RHO the prose quotes (out/exposure_min/gpr_quarterly.csv).
local({
  gp <- tryCatch(fread(file.path(root, "out/exposure_min/gpr_quarterly.csv")), error = function(e) NULL)
  if (is.null(gp) || !"GeoSentiment" %in% names(gp)) {
    cat("min gpr_quarterly.csv missing/stale -- fig_gpr_validation skipped\n"); return(invisible()) }
  z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  rho <- function(m) sprintf("%s (r = %.2f)", nice(m), cor(gp[[m]], gp$GPRT, use = "complete.obs"))
  d <- data.table(date = gp$year + (gp$quarter - 1) / 4, GPRthreats = z(gp$GPRT))
  for (m in M4) d[, (m) := z(gp[[m]])]
  labs4 <- sapply(M4, rho)
  dl <- melt(d, id.vars = c("date", "GPRthreats"), variable.name = "series", value.name = "v")
  dl[, series := factor(series, levels = M4, labels = labs4)]
  base <- ggplot(dl, aes(date, v)) +
    geom_ribbon(aes(ymin = pmin(GPRthreats, 0), ymax = pmax(GPRthreats, 0), fill = "GPR threats (shaded)"),
                alpha = 0.55, colour = NA) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_line(aes(colour = series, linetype = series, linewidth = series)) +
    scale_linewidth_manual(values = setNames(c(1.15, 0.5, 0.6, 0.6), labs4), name = NULL) +
    labs(x = NULL, y = "z-score") + theme_thesis() +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(t = -4),
          legend.key.width = grid::unit(1.6, "lines")) +
    guides(colour = guide_legend(nrow = 2, order = 1), linetype = guide_legend(nrow = 2, order = 1),
           linewidth = guide_legend(nrow = 2, order = 1), fill = guide_legend(order = 2))
  save_both("fig_gpr_validation.pdf",
    base + scale_colour_manual(values = c("#1F4E79", "#6FA8D0", "#767676", "#BDBDBD"), name = NULL) +
      scale_linetype_manual(values = c("solid", "solid", "22", "42"), name = NULL) +
      scale_fill_manual(values = c("GPR threats (shaded)" = "grey85"), name = NULL),
    base + scale_colour_manual(values = c("black", "grey35", "grey55", "black"), name = NULL) +
      scale_linetype_manual(values = c("solid", "solid", "22", "42"), name = NULL) +
      scale_fill_manual(values = c("GPR threats (shaded)" = "grey88"), name = NULL),
    h = 3.9) })

## t-statistic heatmap -- the whole picture on one panel: every headline test (rows)
## by the four measures (columns), fill = t-statistic, value printed in the cell.
local({
  MM <- fread(file.path(ana, "RESULTS_MASTER_min.csv"))
  spec <- list(
    c("Q1_realization","contemp_ret","LSEG","quarterly","RQ1 return at the call (LSEG)"),
    c("Q1_ivol","ivol_coef","LSEG","quarterly","RQ1 idiosyncratic vol (LSEG)"),
    c("Q1_tvol","tvol_coef","LSEG","quarterly","RQ1 total vol (LSEG)"),
    c("Q2_pricing_FM","lambda","LSEG","quarterly","RQ2 Fama–MacBeth (LSEG)"),
    c("Q2_pricing_FE","coef","LSEG","quarterly","RQ2 panel FE (LSEG)"),
    c("Q3_LS_FF5","ls_ew_mean","LSEG","quarterly","RQ3 L/S mean spread (LSEG, EW)"),
    c("Q3_LS_FF5","ew_alpha","CRSP","monthly","RQ3 L/S FF5 alpha (CRSP, EW)"))
  d <- rbindlist(lapply(spec, function(s) {
    x <- MM[question == s[1] & stat == s[2] & sample == s[3] & freq == s[4] & measure %in% M4]
    data.table(label = s[5], measure = x$measure, t = as.numeric(x$t)) }))
  ## RQ2 Fama-MacBeth on genuine CRSP returns, QUARTERLY -- from crsp_analysis, not
  ## RESULTS_MASTER. Quarterly is deliberate: the LSEG RQ2 rows above are quarterly too,
  ## so the row is like-for-like. Table C.3 reports the MONTHLY CRSP version separately,
  ## which is why its t-stats differ; the label states the frequency to avoid confusion.
  cq2 <- J("crsp_analysis.json")$q2
  d <- rbind(d, data.table(label = "RQ2 Fama–MacBeth (CRSP, quarterly)",
                           measure = cq2$measure[cq2$measure %in% M4],
                           t = as.numeric(cq2$fm_t[cq2$measure %in% M4])))
  labs_o <- c(sapply(spec[1:5], `[`, 5), "RQ2 Fama–MacBeth (CRSP, quarterly)", sapply(spec[6:7], `[`, 5))
  d[, measure := factor(nice(measure), levels = nice(M4))]
  d[, label := factor(label, levels = rev(labs_o))]
  base <- ggplot(d, aes(measure, label, fill = t)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.1f", t)), size = 3, family = "serif") +
    labs(x = NULL, y = NULL) + theme_thesis() +
    theme(panel.grid = element_blank(), legend.position = "none",
          axis.text.x = element_text(angle = 18, hjust = 1))
  pc <- base + scale_fill_gradient2(low = "#767676", mid = "white", high = "#1F4E79",
                                    limits = c(-6, 6), oob = scales::squish)
  pb <- base + scale_fill_gradient2(low = "grey55", mid = "white", high = "grey55",
                                    limits = c(-6, 6), oob = scales::squish)
  save_both("fig_t_heatmap.pdf", pc, pb, w = 6.8, h = 4.1) })

# ---------------------------------------------------------------------
# Word cloud of the top-100 discovered bigrams, sized by KEYNESS (G^2).
# Companion to Table C.1, which lists the same 100 terms sorted by raw
# frequency. Keyness is the actual selection criterion (Eq. 3.1), so the
# cloud shows what the algorithm ranked on rather than repeating the
# table's ordering. Font size is proportional to sqrt(G2) so that the
# printed AREA of a term scales with its keyness, and colour is a
# sequential light->dark bin of the same quantity (a second, redundant
# encoding of magnitude, so it survives greyscale printing).
# Layout is a deterministic Archimedean spiral with bounding-box
# collision rejection: no RNG, so the figure is byte-reproducible.
# ---------------------------------------------------------------------
local({
  f <- file.path(root, "pipeline/config/dictionary_geoeconomic.csv")
  d <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(d)) { cat("dictionary csv missing -- fig_top100_cloud skipped\n"); return(invisible()) }
  d <- d[d$origin == "discovered", ]
  d <- d[order(-d$G2), ][seq_len(min(100, nrow(d))), ]
  d$G2 <- as.numeric(d$G2)

  # size: area ~ G2  =>  linear font size ~ sqrt(G2), rescaled to a legible band
  s  <- sqrt(d$G2); SMIN <- 2.5; SMAX <- 6.0
  d$size <- SMIN + (SMAX - SMIN) * (s - min(s)) / (max(s) - min(s))
  d$bin  <- cut(d$G2, breaks = quantile(d$G2, probs = seq(0, 1, length.out = 6)),
                include.lowest = TRUE, labels = FALSE)

  # Deterministic spiral packing, largest term first. Text extents are MEASURED
  # on a null device (base strwidth at the matching point size) rather than
  # estimated from character count -- an estimate underestimates wide serif
  # glyphs and produces overlaps. Units are inches throughout; no RNG.
  grDevices::pdf(NULL, width = 7, height = 5); on.exit(grDevices::dev.off(), add = TRUE)
  par(family = "serif", ps = 12)
  pt   <- 2.845276                       # ggplot size (mm) -> points
  win  <- vapply(seq_len(nrow(d)), function(i)
            strwidth(d$bigram[i], units = "inches", cex = d$size[i] * pt / 12), numeric(1))
  win  <- win * 1.10   # cairo's serif runs wider than the measuring device's; safety factor
  hin  <- d$size * pt / 72 * 1.0         # cap-height box
  n <- nrow(d); x <- y <- numeric(n)
  PADX <- 0.040; PADY <- 0.026           # inches of clear space around each term
  hit <- function(i, xi, yi) {
    if (i == 1L) return(FALSE)
    j <- seq_len(i - 1L)
    any(abs(xi - x[j]) < (win[i] + win[j]) / 2 + PADX &
        abs(yi - y[j]) < (hin[i] + hin[j]) / 2 + PADY)
  }
  for (i in seq_len(n)) {
    if (i == 1L) { x[i] <- 0; y[i] <- 0; next }
    t <- 0; placed <- FALSE; xi <- 0; yi <- 0
    while (!placed && t < 60000) {
      t  <- t + 1
      th <- t * 0.045
      r  <- 0.055 * th
      xi <- r * cos(th) * 1.85; yi <- r * sin(th)     # wide ellipse (page is landscape)
      if (!hit(i, xi, yi)) placed <- TRUE
    }
    x[i] <- xi; y[i] <- yi
  }
  d$x <- x; d$y <- y

  base <- ggplot(d, aes(x, y, label = bigram, size = size)) +
    scale_size_identity() +
    scale_x_continuous(expand = expansion(add = max(win) / 2 + 0.05)) +
    scale_y_continuous(expand = expansion(add = max(hin) / 2 + 0.05)) +
    theme_void(base_family = "serif") +
    theme(legend.position = "none", plot.margin = margin(2, 2, 2, 2))
  # sequential one-hue ramp, light->dark, but floored so the palest terms stay
  # legible on paper (the two lightest PASTEL5 steps wash out when printed).
  CLOUD5 <- c("#9FC2E0", "#7FAED4", "#5B93C4", "#3E7CB1", "#1F4E79")
  CLOUDB <- c("#9a9a9a", "#7a7a7a", "#585858", "#333333", "#000000")
  pc <- base + geom_text(aes(colour = factor(bin)), family = "serif", show.legend = FALSE) +
        scale_colour_manual(values = CLOUD5)
  pb <- base + geom_text(aes(colour = factor(bin)), family = "serif", show.legend = FALSE) +
        scale_colour_manual(values = CLOUDB)
  save_both("fig_top100_cloud.pdf", pc, pb, w = 6.3, h = 3.7)
  cat(sprintf("fig_top100_cloud: %d terms, G2 %s-%s\n", n,
              format(min(d$G2), big.mark = ","), format(max(d$G2), big.mark = ",")))
})

cat("figures (colour + bw) OK\n")
cat("DONE.\n  tables:", length(list.files(tabdir)), " figures:", length(list.files(figdir, pattern="pdf")),
    " bw:", length(list.files(bwdir)), "\n")
