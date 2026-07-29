# ==============================================================================
# pipeline/R/04b_perturbation_corr.R  (Stage 4, after the perturbation array)
# Sautner's leave-one-seed robustness check: each dropped-seed dictionary should
# agree strongly with the full locked dictionary. Reports the top-K OVERLAP
# coefficient (|A intersect B| / min(|A|,|B|)) per dropped seed; Sautner's bar is >85%.
# (The measure-level correlation, the faithful Sautner test, lives in 04g/04j.)
#   Rscript pipeline/R/04b_perturbation_corr.R
# ==============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root()
PERT <- file.path(ROOT, "out", "dict", "perturb")
full <- fread(file.path(ROOT, "pipeline", "config", "dictionary_geoeconomic.csv"))
full_disc <- full[origin == "discovered", bigram]
K <- length(full_disc); full_set <- full_disc

f <- sort(list.files(PERT, pattern = "^dict_drop_\\d+\\.csv$", full.names = TRUE))
if (!length(f)) stop("no dict_drop_*.csv in ", PERT, " — run the perturbation array first.")
res <- rbindlist(lapply(f, function(p){
  i <- as.integer(gsub("\\D", "", basename(p)))
  d <- fread(p)
  ov <- length(intersect(full_set, d$bigram)) / min(K, nrow(d))   # overlap coefficient = |A^B|/min(|A|,|B|) (NOT Jaccard)
  data.table(seed_idx = i, n = nrow(d), overlap_pct = round(100*ov, 1))
}))[order(seed_idx)]
cat(sprintf("Leave-one-seed perturbation vs locked dictionary (top-%d discovered):\n", K))
print(res)
cat(sprintf("\n  seeds tested: %d / 50 | overlap mean=%.1f%%  min=%.1f%%  (Sautner bar >85%%)\n",
            nrow(res), mean(res$overlap_pct), min(res$overlap_pct)))
cat(sprintf("  PASS (all >85%%): %s\n", all(res$overlap_pct > 85)))
write_json(list(k = K, n_seeds = nrow(res), mean = mean(res$overlap_pct),
                min = min(res$overlap_pct), pass = all(res$overlap_pct > 85),
                per_seed = res), file.path(PERT, "perturbation_summary.json"),
           pretty = TRUE, auto_unbox = TRUE)
