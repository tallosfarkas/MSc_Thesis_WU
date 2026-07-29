#!/usr/bin/env Rscript
# =====================================================================
# verify_headline.R — check a reproduction run against the published thesis numbers.
#
#   Rscript verify/verify_headline.R [path/to/out]
#
# Default output root is ./out (as produced by the pipeline). Prints a PASS/FAIL
# table and exits non-zero if any headline number is off, so it can gate CI.
#
# Target: the v2 MINIMAL-CLEANING build (SW_BP=off, GEO_TAG=_min GEO_TIES=first,
# v8.2 CRSP map) — the configuration the submitted thesis reports from. See
# docs/EXPECTED_RESULTS.md for the full list and docs/REPRODUCE.md for the flags.
#
# Tolerances: t-stats +/- 0.05, coefficients +/- 0.02 (percentage points).
# =====================================================================
suppressWarnings(suppressMessages({ library(jsonlite) }))

OUT <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "out"
ANA <- file.path(OUT, "analysis")
if (!dir.exists(ANA)) { cat("FATAL: no", ANA, "- run the pipeline first (docs/REPRODUCE.md)\n"); quit(status = 2) }

# prefer the _min twin, exactly like the thesis (thesis_v2.Rnw, SUF="_min") does
J <- function(f) {
  v <- file.path(ANA, sub("[.]json$", "_min.json", f)); u <- file.path(ANA, f)
  p <- if (file.exists(v)) v else u
  if (!file.exists(p)) return(NULL)
  fromJSON(p)
}
res <- list(); add <- function(what, got, want, tol) {
  got <- suppressWarnings(as.numeric(got))
  if (length(got) != 1L) got <- NA_real_          # missing field -> record as MISSING, never crash
  ok <- is.finite(got) && abs(got - want) <= tol
  res[[length(res) + 1L]] <<- data.frame(check = what, expected = want,
                                         got = ifelse(is.finite(got), round(got, 3), NA_real_),
                                         status = if (!is.finite(got)) "MISSING" else if (ok) "PASS" else "FAIL",
                                         stringsAsFactors = FALSE)
}

## ---- dictionary -----------------------------------------------------
dict <- file.path("config", "dictionary_geoeconomic.csv")
if (file.exists(dict)) add("dictionary terms", length(readLines(dict)) - 1L, 9650, 0)

## ---- RESULTS_MASTER: RQ2 pricing + RQ3 alphas -----------------------
mp <- file.path(ANA, "RESULTS_MASTER_min.csv")
if (file.exists(mp)) {
  M <- read.csv(mp, stringsAsFactors = FALSE)
  pick <- function(q, m, s, fr, st) { r <- M[M$question == q & M$measure == m & M$sample == s &
                                             M$freq == fr & M$stat == st, ]; if (nrow(r)) r[1, ] else NULL }
  r <- pick("Q2_pricing_FM", "GeoSentiment", "LSEG", "quarterly", "lambda")
  if (!is.null(r)) { add("RQ2 GeoSentiment lambda (%/q)", 100 * r$value, 0.26, 0.02)
                     add("RQ2 GeoSentiment t",            r$t,            2.05, 0.05) }
  # RQ3 headline equal-weighted five-factor alphas (CRSP monthly)
  r <- pick("Q3_LS_FF5", "GeoRisk", "CRSP", "monthly", "ew_alpha")
  if (!is.null(r)) { add("RQ3 GeoRisk EW alpha (%/mo)", 100 * r$value, 0.26, 0.02)
                     add("RQ3 GeoRisk EW t",            r$t,            2.74, 0.05) }
  r <- pick("Q3_LS_FF5", "GeoSentiment", "CRSP", "monthly", "ew_alpha")
  if (!is.null(r)) { add("RQ3 GeoSentiment EW alpha (%/mo)", 100 * r$value, 0.19, 0.02)
                     add("RQ3 GeoSentiment EW t (in-sample null)", r$t,      1.80, 0.05) }
} else cat("NOTE: RESULTS_MASTER_min.csv missing - run 06_results_master.R with GEO_TAG=_min\n")

## ---- out-of-sample: real-time dictionary re-discovery (v8.2 map) -----
rt <- J("realtime_dict_oos.json")
if (!is.null(rt$crsp_realtime)) {
  cr <- rt$crsp_realtime
  # the split that carries the thesis: tone survives re-discovery, risk-count dies
  add("OOS rediscovery GeoSentiment t (survives)", cr$GeoSentiment$realtime$t,  2.58, 0.05)
  add("OOS rediscovery GeoRisk t (dies)",          cr$GeoRisk$realtime$t,      -0.06, 0.05)
  # version guard: the fixed-dictionary in-sample t must be the minimal 2.74,
  # NOT the v1.1 frozen 2.87 (that means you are reading _v11 artifacts).
  add("minimal-build guard (GeoRisk fixed t = 2.74, not 2.87)", cr$GeoRisk$fixed$t, 2.74, 0.03)
} else cat("NOTE: realtime_dict_oos*.json missing - run 14_realtime_dict_oos.R (v8.2 map)\n")

## ---- RQ1 realization ------------------------------------------------
q1 <- J("realization_q1.json")$results
if (!is.null(q1) && "ret_contemp" %in% names(q1)) {
  # $results is a data.frame whose ret_contemp column is itself a data.frame (coef/t/n)
  gv <- function(m, f) { i <- which(q1$measure == m); if (length(i)) q1$ret_contemp[[f]][i[1]] else NA_real_ }
  add("RQ1 GeoExposure return (%)",  100 * gv("GeoExposure",  "coef"), -0.53, 0.02)
  add("RQ1 GeoExposure t",           gv("GeoExposure",  "t"),          -5.49, 0.05)
  add("RQ1 GeoSentiment return (%)", 100 * gv("GeoSentiment", "coef"),  1.42, 0.02)
  add("RQ1 GeoSentiment t",          gv("GeoSentiment", "t"),          13.79, 0.05)
} else cat("NOTE: realization_q1*.json missing - run 05m_realization_q1.R\n")

## ---- report ---------------------------------------------------------
if (!length(res)) { cat("\nNothing could be checked. Is the pipeline output present?\n"); quit(status = 2) }
tab <- do.call(rbind, res)
cat("\n============ HEADLINE VERIFICATION (v2 minimal build) ============\n")
print(tab, row.names = FALSE)
nf <- sum(tab$status == "FAIL"); nm <- sum(tab$status == "MISSING")
cat(sprintf("\n%d passed, %d failed, %d missing (of %d).\n", sum(tab$status=="PASS"), nf, nm, nrow(tab)))
if (nf) {
  cat("\nIf RQ3 GeoRisk reads 0.32 / t=2.87 and GeoSentiment t=2.13, or the OOS numbers read\n",
      "2.02 / 0.57, you are checking the FROZEN v1.1 artifacts (_v11), not the minimal v2 build.\n",
      "Re-run the pipeline with SW_BP=off GEO_TAG=_min GEO_TIES=first (see docs/REPRODUCE.md).\n", sep = "")
  quit(status = 1)
}
if (nm) cat("Some checks could not run (missing artifacts) - see NOTEs above.\n") else cat("All headline numbers match the thesis (v2 minimal build).\n")
