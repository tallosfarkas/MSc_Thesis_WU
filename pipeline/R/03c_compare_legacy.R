# ==============================================================================
# pipeline/R/03c_compare_legacy.R   (optional diagnostic — run LOCALLY after the
# new firm-quarter panel is pulled back; does NOT touch the cluster run)
#
# How much did the dictionary rebuild move the cross-section? Joins the NEW
# call-level exposure (Stage 3) to the LEGACY panel on the call Id and reports
# the correlation between the new GeoExposure variants and the old Exposure.
# Pearson = level agreement; Spearman = ranking agreement (the one that matters,
# since both measures are used as cross-sectional ranks). Low agreement is fine /
# expected — the legacy dict was contaminated; this just quantifies the shift.
#
#   Rscript pipeline/R/03c_compare_legacy.R
# Needs: out/exposure/exposure_calls.rds (pull from cluster first) and the local
# legacy panel data/processed/FINAL_FIRM_EXPOSURE.rds.
# ==============================================================================
suppressPackageStartupMessages(library(data.table))
# Run from the project root.
NEW <- "out/exposure/exposure_calls.rds"
OLD <- "data/processed/FINAL_FIRM_EXPOSURE.rds"
stopifnot(file.exists(NEW), file.exists(OLD))

new <- as.data.table(readRDS(NEW))[, .(Id, GeoExposure, GeoExposure_pr, GeoExposureTFIDF)]
old <- as.data.table(readRDS(OLD))[, .(Id, Exposure_old = Exposure)]
m <- merge(new, old, by = "Id")
cat(sprintf("matched calls: %s of new %s / old %s\n",
            format(nrow(m), big.mark=","), format(nrow(new), big.mark=","), format(nrow(old), big.mark=",")))

rep1 <- function(lbl, v) {
  ok <- is.finite(v) & is.finite(m$Exposure_old)
  cat(sprintf("  %-18s  Pearson=%+.3f  Spearman=%+.3f  (n=%s)\n", lbl,
              cor(v[ok], m$Exposure_old[ok]),
              cor(v[ok], m$Exposure_old[ok], method = "spearman"),
              format(sum(ok), big.mark=",")))
}
cat("new vs legacy Exposure (call level):\n")
rep1("GeoExposure",       m$GeoExposure)
rep1("GeoExposure_pruned",m$GeoExposure_pr)
rep1("GeoExposureTFIDF",  m$GeoExposureTFIDF)
cat("\nReading: Spearman is the rank agreement. Expect MODERATE (the rebuild fixed",
    "\ncontamination, so some reshuffle is desired); near-1 would mean the cleanup",
    "\nchanged little, near-0 a complete reshuffle.\n")
