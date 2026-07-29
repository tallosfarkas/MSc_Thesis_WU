# ==============================================================================
# pipeline/R/util_rename_cc_to_geo.R   (one-time MIGRATION — rename CC* -> Geo*)
#
# Renames the Sautner-inherited CC (Climate Change) measure prefix to Geo (Geoeconomic)
# in the COLUMN NAMES of every stored data artefact. Pure relabel — values untouched.
#   GeoExposure -> GeoExposure, GeoRisk -> GeoRisk, GeoSentiment -> GeoSentiment,
#   GeoExposureTFIDF -> GeoExposureTFIDF (+ Pos/Neg + _pr/_exen/_small/_z suffixes ride along).
# RESULTS_MASTER.rds is intentionally skipped (rebuilt by 06_results_master after).
#
#   Rscript pipeline/R/util_rename_cc_to_geo.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); say <- function(...) cat(sprintf(...),"\n")

# ordered (longest-first) token replacement applied to a character vector of names
ren <- function(x){
  # FROM-prefix built as paste0("C","C",...) so a blanket CC->Geo text pass can't
  # silently turn these patterns into no-ops (which is exactly what happened once).
  cc <- function(s) paste0("C","C", s)
  x <- gsub(cc("ExposureTFIDF"), "GeoExposureTFIDF", x, fixed=TRUE)
  x <- gsub(cc("SentimentPos"),  "GeoSentimentPos",  x, fixed=TRUE)
  x <- gsub(cc("SentimentNeg"),  "GeoSentimentNeg",  x, fixed=TRUE)
  x <- gsub(cc("Exposure"),      "GeoExposure",      x, fixed=TRUE)
  x <- gsub(cc("Sentiment"),     "GeoSentiment",     x, fixed=TRUE)
  x <- gsub(cc("Risk"),          "GeoRisk",          x, fixed=TRUE)
  x }

rds <- list.files(file.path(ROOT,"out"), pattern="\\.rds$", recursive=TRUE, full.names=TRUE)
rds <- rds[!grepl("RESULTS_MASTER\\.rds$", rds)]
pq  <- list.files(file.path(ROOT,"out"), pattern="\\.parquet$", recursive=TRUE, full.names=TRUE)

n_rds <- 0L
for (f in rds) {
  obj <- tryCatch(readRDS(f), error=function(e) NULL); if (is.null(obj)) next
  if (is.data.frame(obj)) { nm<-names(obj); nn<-ren(nm)
    if (!identical(nm,nn)) { setDT(obj); setnames(obj, nm, nn); saveRDS(obj, f); n_rds<-n_rds+1L
      say("  [rds] %-46s renamed %d cols", basename(f), sum(nm!=nn)) } }
}
n_pq <- 0L
for (f in pq) {
  d <- tryCatch(as.data.table(read_parquet(f)), error=function(e) NULL); if (is.null(d)) next
  nm<-names(d); nn<-ren(nm)
  if (!identical(nm,nn)) { setnames(d, nm, nn); write_parquet(d, f); n_pq<-n_pq+1L
    say("  [parquet] %-42s renamed %d cols", basename(f), sum(nm!=nn)) }
}
say("[done] migrated %d rds + %d parquet files (RESULTS_MASTER.rds left to rebuild)", n_rds, n_pq)
