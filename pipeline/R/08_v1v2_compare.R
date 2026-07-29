# ==============================================================================
# pipeline/R/08_v1v2_compare.R   (V2 correctness track — the v1-vs-v2 reconciliation)
#
# Reads RESULTS_MASTER.csv (v1, frozen current version) and RESULTS_MASTER_v2.csv
# (corrected pipeline) and emits one side-by-side table of every headline row:
# question | measure | sample | freq | stat | v1 value (t) | v2 value (t) | delta t.
# Also compares the exposure-level audits (mean GeoExposure per year v1 vs v2) so
# the tokenizer effect is visible at the source.
#
#   Rscript pipeline/R/08_v1v2_compare.R
# Writes: out/analysis/v1v2_comparison.csv + v1v2_comparison.json (+ console table)
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis"); say <- function(...) cat(sprintf(...),"\n")

v1 <- fread(file.path(ANA,"RESULTS_MASTER.csv"))
v2p <- file.path(ANA,"RESULTS_MASTER_v2.csv")
if (!file.exists(v2p)) stop("RESULTS_MASTER_v2.csv not built yet — run the v2 chain first.")
v2 <- fread(v2p)

key <- c("question","measure","sample","freq","stat")
cmp <- merge(v1[, c(key,"value","t"), with=FALSE], v2[, c(key,"value","t"), with=FALSE],
             by = key, all = TRUE, suffixes = c("_v1","_v2"))
cmp[, dt := round(t_v2 - t_v1, 2)]
setorder(cmp, question, measure, sample, freq, stat)

say("=== v1 vs v2 — all master rows (%d v1 / %d v2 / %d matched) ===",
    nrow(v1), nrow(v2), sum(!is.na(cmp$t_v1) & !is.na(cmp$t_v2)))
hl <- cmp[question %in% c("Q1_realization","Q2_pricing_FM","Q2_pricing_FE","Q3_LS_FF5","Q3_augmented_spanning")]
for (i in seq_len(nrow(hl))) with(hl[i],
  say("  %-22s %-18s %-10s %-9s %-12s v1 %8.4f (t %6.2f) | v2 %8.4f (t %6.2f) | dt %+.2f",
      question, measure, sample, freq, stat,
      ifelse(is.na(value_v1), NA, value_v1), ifelse(is.na(t_v1), NA, t_v1),
      ifelse(is.na(value_v2), NA, value_v2), ifelse(is.na(t_v2), NA, t_v2),
      ifelse(is.na(dt), NA, dt)))

# exposure-level effect of the tokenizer (per-year audit means)
num1 <- function(x) if (is.null(x) || length(x) != 1L || !is.numeric(x)) NA_real_ else as.numeric(x)
yr_cmp <- rbindlist(lapply(2002:2025, function(y){
  a1 <- file.path(ROOT,"out/exposure",   sprintf("audit_%d.json", y))
  a2 <- file.path(ROOT,"out/exposure_v2",sprintf("audit_%d.json", y))
  if (!file.exists(a1) || !file.exists(a2)) return(NULL)
  j1 <- tryCatch(fromJSON(a1), error=function(e) NULL); j2 <- tryCatch(fromJSON(a2), error=function(e) NULL)
  if (is.null(j1) || is.null(j2)) return(NULL)
  me <- function(j) if (!is.null(j$mean_GeoExposure)) j$mean_GeoExposure else j$mean_CCExposure  # pre-rename audits
  data.table(year=y, mean_exp_v1=num1(me(j1)), mean_exp_v2=num1(me(j2)),
             hit_v1=num1(j1$pct_calls_with_hit), hit_v2=num1(j2$pct_calls_with_hit))
}), fill=TRUE)
if (nrow(yr_cmp) && "mean_exp_v1" %in% names(yr_cmp)) {
  yr_cmp[, ratio := mean_exp_v2/mean_exp_v1]
  say("\n=== tokenizer effect at the source: mean GeoExposure v2/v1 ratio by year ===")
  say("  mean ratio %.3f | range [%.3f, %.3f] | %% calls with hit: v1 %.1f -> v2 %.1f (means)",
      mean(yr_cmp$ratio), min(yr_cmp$ratio), max(yr_cmp$ratio), mean(yr_cmp$hit_v1), mean(yr_cmp$hit_v2))
}

fwrite(cmp, file.path(ANA,"v1v2_comparison.csv"))
write_json(list(headline=hl, all=cmp, exposure_by_year=yr_cmp,
                built=format(Sys.time(),"%Y-%m-%d %H:%M:%S")),
           file.path(ANA,"v1v2_comparison.json"), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("\nwrote out/analysis/v1v2_comparison.{csv,json}")
