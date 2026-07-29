# ==============================================================================
# pipeline/R/02_klr_discovery.R
#
# STAGE 2b — Sautner-faithful KLR keyword discovery.
# Reference: Sautner, van Lent, Vilkov, Zhang (2023) JF Online Appendix p. 2-3.
#
# Algorithm:
#   1. Load all per-year DFMs from Stage 2a (out/dict/dfm_YYYY.rds).
#   2. rbind into one big sparse DFM (166M sentences x ~500k bigrams after trim).
#   3. Label R = sentences whose DFM row contains any seed bigram (set membership).
#      Label S = the rest.
#   4. Sample 100,000 sentences from S as the negative training set.
#   5. Fit MultinomialNB + LinearSVC + RandomForest on R ∪ sample(S, 100k).
#   6. Predict P(R | sentence) for every sentence in S using each classifier.
#   7. Ensemble vote: T = { s ∈ S : max_j P_j(R|s) > 0.8 }   (Sautner p. 3)
#   8. Mine bigrams: rank by LR keyness in (R ∪ T) vs (S \ T). Top-K for curation.
#
# Pipeline split (refactor 2026-05-31 after several failed approaches):
#   Stage 2a (slurm_tokenise.sh + 02a_tokenise.R): per-year SLURM array, each
#   task tokenises one year independently and saves dfm_YYYY.rds + meta_YYYY.rds.
#   Stage 2b (this script + slurm_klr.sh): single job, runs after 2a via
#   --dependency=afterok:. Loads all per-year DFMs and runs KLR.
#
# Why this split: prior attempts (mclapply over 64 chunks, sequential one-job)
# either OOM'd or stalled on quanteda single-threading. Per-year SLURM array
# gives clean parallelism without fork-bomb risk.
#
# Run from project root on cluster:
#     module load r
#     Rscript pipeline/R/02_klr_discovery.R
# ==============================================================================

options(stringsAsFactors = FALSE)

# ---- Setup -------------------------------------------------------------------
# Set thread env vars FIRST, before loading anything that might cache thread count
N_CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", parallel::detectCores()))
Sys.setenv(OMP_NUM_THREADS         = as.character(N_CORES))
Sys.setenv(MKL_NUM_THREADS         = as.character(N_CORES))
Sys.setenv(OPENBLAS_NUM_THREADS    = as.character(N_CORES))
Sys.setenv(RCPP_PARALLEL_NUM_THREADS = as.character(N_CORES))

suppressPackageStartupMessages({
  library(parallel)
  library(RcppParallel)
  RcppParallel::setThreadOptions(numThreads = N_CORES)
  library(data.table)
  library(Matrix)
  library(stringr)
  library(stringi)
  library(quanteda)
  library(quanteda.textmodels)
  library(quanteda.textstats)
  library(LiblineaR)
  library(ranger)
  library(jsonlite)
})
quanteda_options(threads = N_CORES)

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

# Hyperparameters from params.yml
P <- load_params()
SEED               <- P$klr$seed
P_THRESHOLD        <- P$klr$p_threshold              # 0.8
REF_SAMPLE_SIZE    <- P$klr$reference_sample_size    # 100,000
CURATE_TOP_K       <- P$klr$curate_top_k             # 500
CV_FOLDS           <- P$klr$cv_folds                 # 5
MODEL_VOCAB_SIZE   <- P$klr$model_vocab_size         # RF-only feature cap (NB+LR use full vocab)
# Env override for sensitivity runs: RF_VOCAB_SIZE=50000 Rscript ...02b...
.envcap <- Sys.getenv("RF_VOCAB_SIZE")
if (nzchar(.envcap)) MODEL_VOCAB_SIZE <- as.integer(.envcap)
TRIM_MIN_DOCFREQ   <- if (!is.null(P$klr$trim_min_docfreq)) P$klr$trim_min_docfreq else 5L

DICT_OUT  <- file.path(ROOT, "out", "dict")
if (!dir.exists(DICT_OUT)) dir.create(DICT_OUT, recursive = TRUE)

# ---- EXPANDING-WINDOW VINTAGE MODE (additive OOS-on-discovery analysis) -------
# GEO_CUTOFF=YYYY  -> discover the dictionary using ONLY per-year DFMs with year
# <= YYYY (a real-time, point-in-time dictionary). Output is written to
# out/dict/vintage/ with a _vintage_<YYYY> suffix; the full-sample run (no env)
# is untouched. Lets us test whether a trader who only had pre-t text would have
# discovered a materially different dictionary (Randl's look-ahead question).
CUTOFF <- suppressWarnings(as.integer(Sys.getenv("GEO_CUTOFF")))
VTAG   <- if (!is.na(CUTOFF)) sprintf("_vintage_%d", CUTOFF) else ""
VDIR   <- if (!is.na(CUTOFF)) file.path(DICT_OUT, "vintage") else DICT_OUT
if (!is.na(CUTOFF) && !dir.exists(VDIR)) dir.create(VDIR, recursive = TRUE)
if (!is.na(CUTOFF)) log_step("  [VINTAGE] cutoff year = %d -> writing %s/*%s.*", CUTOFF, VDIR, VTAG)

log_step("STAGE 2 — Sautner KLR keyword discovery")
log_step("  cores=%d  P_threshold=%.2f  ref_sample=%d  CV_folds=%d  top_K=%d",
         N_CORES, P_THRESHOLD, REF_SAMPLE_SIZE, CV_FOLDS, CURATE_TOP_K)
log_step("  RcppParallel threads = %d", RcppParallel::defaultNumThreads())

audit <- list(
  hyperparams = list(seed = SEED, p_threshold = P_THRESHOLD,
                     ref_sample_size = REF_SAMPLE_SIZE,
                     cv_folds = CV_FOLDS, curate_top_k = CURATE_TOP_K),
  cores = N_CORES,
  started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# ============================================================================
# 1+2. LOAD + rbind + trim the per-year DFMs from Stage 2a.
#    The rbind over 24 years takes ~3h (74M-feature union), so we CACHE the
#    trimmed dfm_all + meta. Re-runs (iterating on seeds, thresholds, cutoff)
#    skip straight to labelling. Delete the cache if Stage 2a DFMs change.
# ============================================================================
# Vintage runs get their own cache (different corpus subset per cutoff).
CACHE_DFM  <- file.path(DICT_OUT, sprintf("_cache_dfm_all_trimmed%s.rds", VTAG))
CACHE_META <- file.path(DICT_OUT, sprintf("_cache_meta%s.rds", VTAG))

if (file.exists(CACHE_DFM) && file.exists(CACHE_META)) {
  log_step("[1+2/9] Loading cached dfm_all + meta (skipping rbind) ...")
  t0 <- Sys.time()
  dfm_all <- readRDS(CACHE_DFM)
  meta    <- readRDS(CACHE_META)
  N_TOTAL <- ndoc(dfm_all)
  audit$dfm_n_docs <- N_TOTAL
  audit$dfm_n_features_trimmed <- nfeat(dfm_all)
  audit$used_cache <- TRUE
  log_step("    cache: %s docs x %s feat (%.1f min)",
           format(N_TOTAL, big.mark = ","),
           format(nfeat(dfm_all), big.mark = ","),
           as.numeric(Sys.time() - t0, units = "mins"))
} else {
  log_step("[1/9] Loading pre-tokenised per-year DFMs from Stage 2a ...")
  files_dfm  <- sort(list.files(DICT_OUT, pattern = "^dfm_\\d{4}\\.rds$",
                                full.names = TRUE))
  files_meta <- sort(list.files(DICT_OUT, pattern = "^meta_\\d{4}\\.rds$",
                                full.names = TRUE))
  if (length(files_dfm) == 0)
    stop("No dfm_YYYY.rds files in ", DICT_OUT,
         ". Run Stage 2a first (sbatch pipeline/cluster/slurm_tokenise.sh)")
  if (length(files_dfm) != length(files_meta))
    stop("DFM/meta file count mismatch — re-run Stage 2a")
  # VINTAGE: keep only year-DFMs at or before the cutoff (point-in-time corpus).
  if (!is.na(CUTOFF)) {
    yrd <- as.integer(sub(".*dfm_(\\d{4})\\.rds$",  "\\1", files_dfm))
    yrm <- as.integer(sub(".*meta_(\\d{4})\\.rds$", "\\1", files_meta))
    files_dfm  <- files_dfm[yrd <= CUTOFF]
    files_meta <- files_meta[yrm <= CUTOFF]
    audit$vintage_cutoff <- CUTOFF
    audit$vintage_n_years <- length(files_dfm)
    log_step("    [VINTAGE] cutoff=%d -> using %d year-DFMs (<= %d)", CUTOFF, length(files_dfm), CUTOFF)
    if (length(files_dfm) == 0) stop("No year-DFMs at or before cutoff ", CUTOFF)
  }
  log_step("    found %d per-year DFM files", length(files_dfm))

  dfm_list  <- vector("list", length(files_dfm))
  meta_list <- vector("list", length(files_meta))
  N_TOTAL <- 0L

  for (i in seq_along(files_dfm)) {
    t0 <- Sys.time()
    d  <- readRDS(files_dfm[i])
    m  <- readRDS(files_meta[i])
    dfm_list[[i]]  <- d
    meta_list[[i]] <- m
    N_TOTAL <- N_TOTAL + ndoc(d)
    log_step("    %s: %s docs x %s feat (%.1f sec)",
             basename(files_dfm[i]),
             format(ndoc(d),  big.mark = ","),
             format(nfeat(d), big.mark = ","),
             as.numeric(Sys.time() - t0, units = "secs"))
    rm(d, m); gc(verbose = FALSE)
  }

  log_step("    TOTAL sentences: %s", format(N_TOTAL, big.mark = ","))
  audit$n_sentences_total <- N_TOTAL

  log_step("[2/9] Rbinding %d per-year DFMs ...", length(dfm_list))
  t0 <- Sys.time()
  dfm_all <- do.call(rbind, dfm_list)
  rm(dfm_list); gc(verbose = FALSE)
  meta <- rbindlist(meta_list)
  rm(meta_list); gc(verbose = FALSE)
  audit$dfm_n_docs     <- ndoc(dfm_all)
  audit$dfm_n_features <- nfeat(dfm_all)
  log_step("    DFM: %s docs x %s features (rbind: %.1f min)",
           format(ndoc(dfm_all),  big.mark = ","),
           format(nfeat(dfm_all), big.mark = ","),
           as.numeric(Sys.time() - t0, units = "mins"))

  log_step("    trimming bigrams with docfreq < %d ...", TRIM_MIN_DOCFREQ)
  t0 <- Sys.time()
  dfm_all <- dfm_trim(dfm_all, min_docfreq = TRIM_MIN_DOCFREQ)
  audit$dfm_n_features_trimmed <- nfeat(dfm_all)
  log_step("    DFM trimmed: %s features (%.1f sec)",
           format(nfeat(dfm_all), big.mark = ","),
           as.numeric(Sys.time() - t0, units = "secs"))

  log_step("    caching trimmed dfm_all + meta for re-runs ...")
  saveRDS(dfm_all, CACHE_DFM)
  saveRDS(meta,    CACHE_META)
  audit$used_cache <- FALSE
}
N_TOTAL <- ndoc(dfm_all)

# ============================================================================
# 3. LABEL R (target) vs S (search). Set-membership lookup in DFM columns —
#    near-free vs the old regex-on-raw-text approach.
# ============================================================================
log_step("[3/9] Labelling target sentences (R) via DFM set-membership ...")
seeds <- load_seeds()
audit$n_seeds <- length(seeds)

# Sklearn-style normalisation of seeds (must match what tokenize_sklearn does
# to call text). Lowercase + accent strip = the only transforms.
seeds_norm <- tolower(stri_trans_general(seeds, "Latin-ASCII"))

# Guard: every seed MUST be a bigram (exactly 2 whitespace-separated tokens).
# Unigrams ("nearshoring") and trigrams ("supply chain disruption") can never
# match a bigram feature and would be silently dead. Warn loudly.
n_tokens <- lengths(strsplit(trimws(seeds_norm), "\\s+"))
non_bigram <- seeds_norm[n_tokens != 2L]
if (length(non_bigram) > 0) {
  log_step("    WARNING: %d seed(s) are NOT bigrams and cannot match — fix seeds.yml:",
           length(non_bigram))
  log_step("      %s", paste(non_bigram, collapse = " | "))
}
audit$n_seeds_non_bigram <- length(non_bigram)

present_seeds <- intersect(seeds_norm, featnames(dfm_all))
missing_seeds <- setdiff(seeds_norm, featnames(dfm_all))
audit$n_seeds_in_dfm <- length(present_seeds)
audit$missing_seeds  <- missing_seeds
log_step("    %d seeds total, %d present in DFM, %d missing",
         length(seeds), length(present_seeds), length(missing_seeds))
if (length(missing_seeds) > 0)
  log_step("    missing: %s", paste(missing_seeds, collapse = " | "))

# is_R = any seed bigram present in the sentence
seed_dfm <- dfm_select(dfm_all, pattern = present_seeds, valuetype = "fixed")
is_R <- as.logical(rowSums(seed_dfm) > 0)
n_R <- sum(is_R); n_S <- N_TOTAL - n_R
audit$n_R <- n_R; audit$n_S <- n_S
audit$R_share_pct <- round(100 * n_R / N_TOTAL, 4)
log_step("    R (target)  : %s  (%.4f%%)", format(n_R, big.mark = ","), audit$R_share_pct)
log_step("    S (search)  : %s",           format(n_S, big.mark = ","))

# ============================================================================
# 4. SAMPLE 100k from S as negative training set (Sautner OA p. 2)
# ============================================================================
log_step("[4/9] Sampling %d sentences from S for training ...", REF_SAMPLE_SIZE)
set.seed(SEED)
r_idx       <- which(is_R)
s_idx_all   <- which(!is_R)
s_idx_train <- sample(s_idx_all, min(REF_SAMPLE_SIZE, length(s_idx_all)))
train_idx   <- c(r_idx, s_idx_train)
train_labels <- factor(c(rep("R", length(r_idx)),
                         rep("S", length(s_idx_train))),
                       levels = c("S", "R"))
audit$n_train_R <- length(r_idx)
audit$n_train_S <- length(s_idx_train)
log_step("    training set: %d (R=%d, S_sample=%d)",
         length(train_idx), length(r_idx), length(s_idx_train))

# ============================================================================
# 4b. MODEL VOCABULARY — bounded feature space for ALL THREE classifiers.
#
# RandomForest (ranger) needs dense input; LiblineaR (logit) also densifies
# internally at fit time. Both blow past memory on the full multi-million-feature
# vocabulary. So all three classifiers share a bounded top-K vocabulary. This
# does NOT limit discovery: step 8 mines bigrams over the FULL dfm_all. The cap
# only affects which sentences the classifiers pull into the target set T.
# The MODEL_VOCAB_SIZE cap is a sensitivity knob (env RF_VOCAB_SIZE). Seeds forced in.
# ============================================================================
log_step("[4b/9] Building model vocabulary (top %s bigrams) ...",
         format(MODEL_VOCAB_SIZE, big.mark = ","))
t0 <- Sys.time()
feat_freq <- colSums(dfm_all)
top_feats <- names(sort(feat_freq, decreasing = TRUE))[seq_len(min(MODEL_VOCAB_SIZE, length(feat_freq)))]
model_vocab <- union(top_feats, present_seeds)
dfm_model <- dfm_match(dfm_all, features = model_vocab)
audit$model_vocab_size <- length(model_vocab)
audit$full_vocab_size  <- nfeat(dfm_all)
log_step("    model vocab: %s features (%d seeds forced in) | full vocab %s (%.1f sec)",
         format(length(model_vocab), big.mark = ","),
         length(present_seeds),
         format(nfeat(dfm_all), big.mark = ","),
         as.numeric(Sys.time() - t0, units = "secs"))

dfm_train <- dfm_model[train_idx, ]

# ============================================================================
# 5. FIT three classifiers (all on the bounded model vocabulary)
# ============================================================================
log_step("[5/9] Fitting three classifiers on training set ...")

log_step("    [5a] Multinomial NB ...")
t0 <- Sys.time()
m_nb <- textmodel_nb(dfm_train, y = train_labels,
                     distribution = "multinomial", smooth = 1)
log_step("        NB fitted (%.1f sec)",
         as.numeric(Sys.time() - t0, units = "secs"))

# 5b. Linear classifier. King-Lam-Roberts (2017) originally use Naive Bayes +
# logit; Sautner swap logit for LinearSVC. We use L2-regularised LOGISTIC
# REGRESSION (LiblineaR type=0): same linear family but, unlike SVM margins,
# yields calibrated class probabilities — required for the P(R)>0.8 rule. Closer
# to the original KLR than Sautner's SVC.
log_step("    [5b] Logistic regression (LiblineaR type=0, cost=1) ...")
t0 <- Sys.time()
X_train <- as(dfm_train, "CsparseMatrix")
m_lr <- LiblineaR(data = X_train, target = train_labels,
                  type = 0L, cost = 1, bias = 1)
log_step("        LR fitted (%.1f sec)",
         as.numeric(Sys.time() - t0, units = "secs"))

log_step("    [5c] Random Forest (ranger num.trees=100, dense %s x %s) ...",
         format(length(train_idx), big.mark = ","),
         format(length(model_vocab), big.mark = ","))
t0 <- Sys.time()
m_rf <- ranger(x = as.matrix(X_train),
               y = train_labels,
               num.trees = 100L,
               num.threads = N_CORES,
               probability = TRUE,
               classification = TRUE,
               seed = SEED,
               verbose = FALSE)
log_step("        RF fitted (%.1f min)",
         as.numeric(Sys.time() - t0, units = "mins"))
rm(X_train); gc(verbose = FALSE)

# ============================================================================
# 6. PREDICT P(R) for every sentence in S (all on the capped dfm_model).
# ============================================================================
log_step("[6/9] Predicting P(R) on all %s sentences in S ...",
         format(n_S, big.mark = ","))
dfm_predict <- dfm_model[s_idx_all, ]

log_step("    [6a] NB predict ...")
t0 <- Sys.time()
p_nb <- predict(m_nb, newdata = dfm_predict, type = "probability")[, "R"]
log_step("        NB predict: %.1f min",
         as.numeric(Sys.time() - t0, units = "mins"))

# --- 6b. LR predict via SPARSE linear algebra (no densification) ----------
# Logistic regression is linear: decision = X·w + b, P = sigmoid(decision).
# One sparse matrix-vector product — seconds, not the 18.7h the chunked-dense
# version took. Verified against LiblineaR::predict on a 2000-row sample.
log_step("    [6b] LR predict (sparse matrix-vector product) ...")
t0 <- Sys.time()
W <- m_lr$W                                  # 1 x (nfeat + Bias)
wnames <- colnames(W)
bias_w <- if ("Bias" %in% wnames) W[1, "Bias"] else 0
feat_w <- setNames(numeric(length(model_vocab)), featnames(dfm_predict))
common <- intersect(featnames(dfm_predict), wnames)
feat_w[common] <- W[1, common]
class1 <- m_lr$ClassNames[1]
Xs <- as(dfm_predict, "CsparseMatrix")
dvals <- as.numeric(Xs %*% feat_w) + bias_w
p_class1 <- 1 / (1 + exp(-dvals))
p_lr <- if (as.character(class1) == "R") p_class1 else 1 - p_class1

# Self-check: reproduce LiblineaR::predict on a small sample.
set.seed(SEED)
chk <- sample(seq_len(nrow(Xs)), min(2000L, nrow(Xs)))
pr_chk <- predict(m_lr, as.matrix(dfm_predict[chk, ]), proba = TRUE)$probabilities
rcol <- if ("R" %in% colnames(pr_chk)) "R" else ncol(pr_chk)
max_diff <- max(abs(p_lr[chk] - pr_chk[, rcol]))
if (max_diff > 1e-4)
  stop(sprintf("LR sparse-vs-LiblineaR mismatch: max abs diff %.6g — check weight alignment/sign", max_diff))
log_step("        LR predict: %.2f min (self-check max diff %.2e)",
         as.numeric(Sys.time() - t0, units = "mins"), max_diff)
rm(Xs); gc(verbose = FALSE)

# --- 6c. RF predict, parallel dense chunks --------------------------------
# RF (ranger) needs dense per-row input, so we chunk over dfm_model and run the
# chunks in PARALLEL via mclapply: 16 workers x 20k-row dense chunk (~1.6 GB
# each) -> ~26 GB peak, ~16x faster than sequential.
PRED_CHUNK   <- 20000L
PRED_WORKERS <- min(16L, N_CORES)
chunk_starts <- seq(1L, length(s_idx_all), by = PRED_CHUNK)
log_step("    [6c] RF predict (%d dense chunks of %s, %d parallel workers) ...",
         length(chunk_starts), format(PRED_CHUNK, big.mark = ","), PRED_WORKERS)
t0 <- Sys.time()
rf_chunk <- function(k) {
  rng <- k:min(k + PRED_CHUNK - 1L, length(s_idx_all))
  X_chunk <- as.matrix(dfm_predict[rng, ])
  # ranger single-thread per worker (we parallelise across chunks instead)
  predict(m_rf, data = X_chunk, num.threads = 1L)$predictions[, "R"]
}
rf_parts <- mclapply(chunk_starts, rf_chunk,
                     mc.cores = PRED_WORKERS, mc.preschedule = FALSE)
err <- which(vapply(rf_parts, function(x) inherits(x, "try-error"), logical(1)))
if (length(err) > 0)
  stop(sprintf("%d RF predict chunks failed (first: %s)",
               length(err), as.character(rf_parts[[err[1]]])))
p_rf <- unlist(rf_parts, use.names = FALSE)
rm(rf_parts); gc(verbose = FALSE)
log_step("        RF predict: %.1f min",
         as.numeric(Sys.time() - t0, units = "mins"))

# ============================================================================
# 7. ENSEMBLE vote -> T
# ============================================================================
log_step("[7/9] Ensemble vote — sentence enters T if max P_j(R) > %.2f ...",
         P_THRESHOLD)
in_T <- (p_nb > P_THRESHOLD) | (p_lr > P_THRESHOLD) | (p_rf > P_THRESHOLD)
n_T <- sum(in_T)
audit$n_T <- n_T
audit$T_share_of_S_pct <- round(100 * n_T / n_S, 4)
audit$n_T_nb  <- sum(p_nb > P_THRESHOLD)
audit$n_T_lr  <- sum(p_lr > P_THRESHOLD)
audit$n_T_rf  <- sum(p_rf > P_THRESHOLD)
log_step("    T: %s sentences (%.4f%% of S)",
         format(n_T, big.mark = ","), audit$T_share_of_S_pct)
log_step("    By classifier (any > %.2f):", P_THRESHOLD)
log_step("        NB :  %s", format(audit$n_T_nb, big.mark = ","))
log_step("        LR :  %s", format(audit$n_T_lr, big.mark = ","))
log_step("        RF :  %s", format(audit$n_T_rf, big.mark = ","))

target_idx <- c(r_idx, s_idx_all[in_T])
audit$n_target_total <- length(target_idx)
log_step("    R ∪ T = %s target sentences", format(length(target_idx), big.mark = ","))

# ============================================================================
# 8. MINE bigrams: LR keyness (target vs non-target)
# ============================================================================
log_step("[8/9] Mining bigrams: target (R∪T) vs non-target (S\\T) ...")
dfm_target    <- dfm_all[target_idx, ]
dfm_nontarget <- dfm_all[-target_idx, ]

sums_target    <- colSums(dfm_target)
sums_nontarget <- colSums(dfm_nontarget)

mtx <- rbind(target = sums_target, nontarget = sums_nontarget)
dfm_2 <- as.dfm(mtx)
keyness <- textstat_keyness(dfm_2, target = "target", measure = "lr")
setDT(keyness)

docfreq_target    <- docfreq(dfm_target)
docfreq_nontarget <- docfreq(dfm_nontarget)
keyness[, target_freq    := sums_target[feature]]
keyness[, nontarget_freq := sums_nontarget[feature]]
keyness[, target_docfreq := docfreq_target[feature]]
keyness[, nontarget_docfreq := docfreq_nontarget[feature]]
keyness[, target_share   := target_freq / (target_freq + nontarget_freq)]

keyness <- keyness[target_freq > 0]
setorder(keyness, -G2)

audit$n_candidate_bigrams <- nrow(keyness)
log_step("    %s candidate bigrams (target_freq > 0)",
         format(nrow(keyness), big.mark = ","))

# Significance flags so the cutoff can be decided from the data (not a fixed
# top-K). Bonferroni-style threshold on the LR keyness G2 (chi-sq 1 df) at the
# candidate count, plus Sautner's "appears only in T" rule.
audit$n_sig_bonferroni <- sum(keyness$p < 0.05 / nrow(keyness))
audit$n_target_only    <- sum(keyness$nontarget_freq == 0)
log_step("    significant (Bonferroni p<0.05/n): %s",
         format(audit$n_sig_bonferroni, big.mark = ","))
log_step("    target-only bigrams (Sautner rule): %s",
         format(audit$n_target_only, big.mark = ","))

# The default v1 dictionary still uses top-K for convenience, but the FULL
# ranked candidate table is saved so the cutoff is a curation decision.
top_k <- head(keyness, CURATE_TOP_K)

seed_dict <- data.table(bigram = seeds, origin = "seed",
                        target_freq = NA_real_, nontarget_freq = NA_real_,
                        G2 = NA_real_, p = NA_real_, target_share = NA_real_)
disc_dict <- data.table(bigram = top_k$feature, origin = "discovered",
                        target_freq    = top_k$target_freq,
                        nontarget_freq = top_k$nontarget_freq,
                        G2 = top_k$G2, p = top_k$p,
                        target_share = top_k$target_share)
dictionary <- unique(rbind(seed_dict, disc_dict), by = "bigram")

# ============================================================================
# 9. SAVE outputs
# ============================================================================
log_step("[9/9] Writing outputs ...")
audit$ended_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Headline-format dictionary = seeds + ALL Bonferroni-significant discovered
# bigrams (Sautner no-pruning rule; same construction as the 9,650-term
# config dictionary). Columns match pipeline/config/dictionary_geoeconomic.csv.
bonf <- keyness[p < 0.05 / nrow(keyness)]
dict_full <- unique(rbind(
  data.table(bigram = seeds,        origin = "seed",       G2 = NA_real_, target_freq = NA_real_, target_share = NA_real_),
  data.table(bigram = bonf$feature, origin = "discovered", G2 = bonf$G2,  target_freq = bonf$target_freq, target_share = bonf$target_share)
), by = "bigram")

if (!is.na(CUTOFF)) {
  # ---- VINTAGE run: point-in-time dictionary only (headline format) ----------
  fwrite(dict_full, file.path(VDIR, sprintf("dictionary%s.csv", VTAG)))
  fwrite(keyness,   file.path(VDIR, sprintf("keyness%s.csv",    VTAG)))
  write_json(audit, file.path(VDIR, sprintf("klr_audit%s.json", VTAG)), pretty = TRUE, auto_unbox = TRUE)
  log_step("    [VINTAGE %d] wrote %s", CUTOFF, file.path(VDIR, sprintf("dictionary%s.csv", VTAG)))
  log_step("    [VINTAGE %d] %d terms = %d seed + %d discovered (Bonferroni p<0.05/n)",
           CUTOFF, nrow(dict_full), length(seeds), nrow(bonf))
  log_step("=== STAGE 2 VINTAGE %d DONE ===", CUTOFF)
} else {
  # ---- Full-sample run: unchanged authoritative artefacts --------------------
  saveRDS(keyness, file.path(DICT_OUT, "keyness_all.rds"))
  fwrite(keyness,  file.path(DICT_OUT, "keyness_all.csv"))
  fwrite(head(keyness, 2000L), file.path(DICT_OUT, "top_candidates_browse.csv"))
  saveRDS(dictionary, file.path(DICT_OUT, "dictionary_v1.rds"))
  fwrite(dictionary, file.path(DICT_OUT, "dictionary_v1.csv"))
  # headline-format full dictionary too (seeds + all Bonferroni-sig discovered)
  fwrite(dict_full, file.path(DICT_OUT, "dictionary_full_bonferroni.csv"))
  write_json(audit,  file.path(DICT_OUT, "klr_audit.json"), pretty = TRUE, auto_unbox = TRUE)
  preds <- data.table(
    year        = meta$year[s_idx_all],
    Id          = meta$Id[s_idx_all],
    sentence_id = meta$sentence_id[s_idx_all],
    p_nb        = p_nb,
    p_lr        = p_lr,
    p_rf        = p_rf,
    in_T        = in_T
  )
  saveRDS(preds, file.path(DICT_OUT, "classifier_predictions.rds"))
  saveRDS(meta,  file.path(DICT_OUT, "sentence_meta.rds"))
}

log_step("    %s  (full ranked candidates)", file.path(DICT_OUT, "keyness_all.rds"))
log_step("    %s  (top-2000 browse)", file.path(DICT_OUT, "top_candidates_browse.csv"))
log_step("    %s  (seeds + top-%d default)", file.path(DICT_OUT, "dictionary_v1.csv"), CURATE_TOP_K)
log_step("    %s", file.path(DICT_OUT, "klr_audit.json"))
log_step("=== STAGE 2 DONE ===")
log_step("Candidates: %s | Bonferroni-sig: %s | target-only: %s | default dict: %d",
         format(audit$n_candidate_bigrams, big.mark = ","),
         format(audit$n_sig_bonferroni, big.mark = ","),
         format(audit$n_target_only, big.mark = ","),
         nrow(dictionary))
