# ==============================================================================
# pipeline/R/02a_tokenise.R
#
# STAGE 2a — tokenise ONE year. Designed to run as a SLURM array job, one
# task per year (2002-2025). Each task is independent — no shared state.
#
# Input  : out/corpus/sentences_YYYY.rds   (from Stage 1)
# Output : out/dict/dfm_YYYY.rds           sparse bigram DFM, sklearn-parity
#          out/dict/meta_YYYY.rds          (year, Id, sentence_id) per row
#
# Memory: ~8-16 GB per task (peak), well under the 32 GB SLURM request.
# Runtime: ~30 sec for 2002, ~5 min for 2024 (largest year).
#
# Usage:
#   Rscript pipeline/R/02a_tokenise.R 2002
#   sbatch  pipeline/cluster/slurm_tokenise.sh   # array over all 24 years
# ==============================================================================
options(stringsAsFactors = FALSE)

# Parse year arg (from CLI or SLURM_ARRAY_TASK_ID 1..24 -> 2002..2025)
args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args) >= 1) as.integer(args[1]) else {
  sa <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  if (nzchar(sa)) 2001L + as.integer(sa) else
    stop("Pass year as arg or set SLURM_ARRAY_TASK_ID (1=2002, ..., 24=2025).")
}
stopifnot(year >= 2002L, year <= 2025L)

# Thread env for any internal parallelism quanteda uses
N_CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", parallel::detectCores()))
Sys.setenv(OMP_NUM_THREADS         = as.character(N_CORES))
Sys.setenv(OPENBLAS_NUM_THREADS    = as.character(N_CORES))
Sys.setenv(RCPP_PARALLEL_NUM_THREADS = as.character(N_CORES))

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(stringi)
  library(quanteda)
})

.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
source(file.path(ROOT, "pipeline", "R", "lib", "utils.R"))
source(file.path(ROOT, "pipeline", "R", "lib", "sklearn_tokenizer.R"))

DICT_OUT <- v2_dir(file.path(ROOT, "out", "dict"))   # GEOV2=1 -> out/dict_v2 (exact-sklearn tokenizer)
if (!dir.exists(DICT_OUT)) dir.create(DICT_OUT, recursive = TRUE)
if (is_v2()) log_step("V2 correctness track: exact-sklearn tokenizer -> %s", DICT_OUT)

IN_SENT  <- file.path(ROOT, "out", "corpus", sprintf("sentences_%d.rds", year))
OUT_DFM  <- file.path(DICT_OUT, sprintf("dfm_%d.rds",  year))
OUT_META <- file.path(DICT_OUT, sprintf("meta_%d.rds", year))

if (!file.exists(IN_SENT)) stop("Missing: ", IN_SENT)

log_step("STAGE 2a — Tokenise year %d", year)
log_step("  cores=%d  in=%s", N_CORES, IN_SENT)

# Load
t0 <- Sys.time()
s <- readRDS(IN_SENT)
log_step("  loaded %s sentences (%.1f sec)",
         format(nrow(s), big.mark = ","),
         as.numeric(Sys.time() - t0, units = "secs"))

# Tokenise + DFM
t0 <- Sys.time()
toks <- tokenize_sklearn(s$text)
d <- dfm(toks)
rm(toks)
log_step("  tokenised + DFM: %s docs x %s feat (%.1f min)",
         format(ndoc(d),  big.mark = ","),
         format(nfeat(d), big.mark = ","),
         as.numeric(Sys.time() - t0, units = "mins"))

# Meta (year, Id, sentence_id) so we can trace rows after rbind in Stage 2b
meta <- data.table(year = year, Id = s$Id, sentence_id = s$sentence_id)
rm(s); gc(verbose = FALSE)

# Save
saveRDS(d,    OUT_DFM,  compress = "gzip")
saveRDS(meta, OUT_META, compress = "gzip")
log_step("  wrote %s (%.1f MB)", OUT_DFM,
         file.info(OUT_DFM)$size / 1024^2)
log_step("  wrote %s (%.1f MB)", OUT_META,
         file.info(OUT_META)$size / 1024^2)
log_step("=== DONE — year %d ===", year)
