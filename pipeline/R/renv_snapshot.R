#!/usr/bin/env Rscript
# ==============================================================================
# pipeline/R/renv_snapshot.R
#
# Reproducibility helper for the R side. Two modes:
#
#   Rscript pipeline/R/renv_snapshot.R --versions-only
#       Just print/refresh the installed versions of the pipeline packages
#       (writes pipeline/R/PACKAGE_VERSIONS.md). Non-invasive, read-only.
#
#   Rscript pipeline/R/renv_snapshot.R --full
#       Initialise renv for this project and write a proper renv.lock pinning the
#       full dependency tree. Run this in a CLEAN cluster session (no pipeline
#       jobs running) — renv::init restructures the project library.
#
# Run on the CLUSTER (`module load r`), where the pipeline packages are installed.
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
mode <- if ("--full" %in% args) "full" else "versions"

pkgs <- c("data.table","Matrix","quanteda","quanteda.textmodels","quanteda.textstats",
          "LiblineaR","ranger","stringr","stringi","jsonlite","arrow","cld2",
          "RcppParallel","yaml","readtext")

if (mode == "versions") {
  ip <- as.data.frame(installed.packages()[, c("Package","Version")], stringsAsFactors = FALSE)
  ip <- ip[ip$Package %in% pkgs, ]
  ip <- ip[order(ip$Package), ]
  out <- c("# R package versions (WU cluster, pipeline execution environment)",
           "",
           sprintf("Captured %s. **R version: %s**", Sys.Date(), as.character(getRversion())),
           "", "| Package | Version |", "|---|---|",
           sprintf("| %s | %s |", ip$Package, ip$Version))
  writeLines(out, "pipeline/R/PACKAGE_VERSIONS.md")
  cat("wrote pipeline/R/PACKAGE_VERSIONS.md\n")
} else {
  if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
  renv::init(bare = TRUE, restart = FALSE)
  renv::snapshot(packages = pkgs, prompt = FALSE)
  cat("wrote renv.lock\n")
}
