# ==============================================================================
# pipeline/R/04g_snippet_audit_sample.R   (Stage 4 — human-reading snippet audit, Sautner A.2)
#
# Builds a decile-stratified snippet sample for a HUMAN audit of GeoExposure, following
# Sautner et al. (2023) Appendix A.2 (themselves following Baker-Bloom-Davis 2016,
# Hassan et al. 2019):
#   - sort transcripts with NONZERO GeoExposure into deciles;
#   - snippet = the 10 sentences around the geoeconomic bigram with the highest text
#     frequency in that transcript;
#   - for GeoExposure=0 transcripts, a random 10 consecutive sentences;
#   - sample K snippets per decile + K from the zero bin.
# The rater later codes CCAudit (1 = clear evidence of geoeconomic exposure, else 0) and
# Coding Confidence (3 high .. 1 hard call). The decile/score is held in a SEPARATE key
# so the coding workbook is BLIND. Goal: the share of CCAudit=1 should rise ~monotonically
# with the GeoExposure decile (Sautner Figure 1).
#
# RUN ON THE CLUSTER (needs the full out/corpus/sentences_*.rds + exposure_calls.rds):
#   module load r && Rscript pipeline/R/04g_snippet_audit_sample.R
# Output: out/validation/snippet_audit_pool.csv  (pull back to the Mac)
# Env: AUDIT_K (snippets per stratum, default 20), AUDIT_SEED (default 42), AUDIT_WIN (10).
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(quanteda) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); setwd(ROOT)
suppressPackageStartupMessages(library(stringi))
source(file.path(ROOT,"pipeline/R/lib/utils.R"))            # load_stopwords_sklearn (needed by the tokenizer)
source(file.path(ROOT,"pipeline/R/lib/sklearn_tokenizer.R"))
CORP <- file.path(ROOT,"out","corpus"); EXP <- file.path(ROOT,"out","exposure"); VAL <- file.path(ROOT,"out","validation")
dir.create(VAL, showWarnings=FALSE, recursive=TRUE); say <- function(...) cat(sprintf(...),"\n")
K     <- as.integer(Sys.getenv("AUDIT_K", "20"))
SEED  <- as.integer(Sys.getenv("AUDIT_SEED", "42"))
WIN   <- as.integer(Sys.getenv("AUDIT_WIN", "10"))
HALF  <- WIN %/% 2L

# ---- dictionary bigrams (primary 9,650) ------------------------------------
dict <- fread(file.path(ROOT,"pipeline/config/dictionary_geoeconomic.csv"))
norm_bigrams <- function(bg) tolower(stri_trans_general(trimws(bg), "Latin-ASCII"))  # match the tokenizer (= Stage 3)
dict_bigrams <- unique(norm_bigrams(dict$bigram))
say("[dict] %s primary geoeconomic bigrams", format(length(dict_bigrams),big.mark=","))

# ---- 1. Sautner stratification on GeoExposure --------------------------------
exp <- as.data.table(readRDS(file.path(EXP,"exposure_calls.rds")))[, .(Id, year, GeoExposure, n_dict_hits)]
exp <- unique(exp, by="Id")                              # one row per call (cross-year dups identical)
nz <- exp[GeoExposure > 0 & n_dict_hits > 0]
z  <- exp[GeoExposure == 0 | n_dict_hits == 0]
brk <- quantile(nz$GeoExposure, probs=(0:10)/10, na.rm=TRUE); brk[1] <- -Inf; brk[11] <- Inf
nz[, decile := as.integer(cut(GeoExposure, unique(brk), labels=FALSE, include.lowest=TRUE))]
say("[1] nonzero calls %s (deciles 1-10) | zero-exposure calls %s", format(nrow(nz),big.mark=","), format(nrow(z),big.mark=","))

set.seed(SEED)
samp <- rbind(
  nz[, .SD[sample(.N, min(K,.N))], by=decile],
  z[sample(.N, min(K,nrow(z)))][, decile := 0L])
samp <- samp[, .(Id, year, decile, GeoExposure, n_dict_hits)]
setorder(samp, decile)
say("[2] sampled %s snippets across %s strata (K=%d/stratum)", nrow(samp), uniqueN(samp$decile), K)
print(samp[, .N, by=decile][order(decile)])

# ---- 2. snippet extraction (load sentences year-by-year) --------------------
build_snippet <- function(sents, is_nonzero){
  # sents: data.table(sentence_id, text) ordered; returns list(text, top_bigram, ctr, nse)
  ns <- nrow(sents); if (ns == 0) return(NULL)
  idx <- seq_len(ns)
  if (is_nonzero) {
    topb <- NA_character_; ctr <- NA_integer_
    toks <- tryCatch(tokenize_sklearn(sents$text), error=function(e) NULL)
    if (!is.null(toks)) {
      m <- dfm(toks); keep <- intersect(featnames(m), dict_bigrams)
      if (length(keep) > 0) {
        md <- dfm_match(m, keep)
        topb <- names(which.max(colSums(md)))                        # highest-text-frequency dict bigram
        persent <- as.numeric(md[, topb])                            # count of the TOP bigram per sentence
        if (any(persent > 0)) ctr <- which.max(persent)             # sentence where the top bigram is densest
        else { rs <- rowSums(md); if (any(rs > 0)) ctr <- which.max(rs) }
      }
    }
    if (is.na(ctr)) ctr <- sample(idx, 1)                            # fallback (no dict match found)
  } else {
    topb <- NA_character_
    start_max <- max(1L, ns - WIN + 1L); ctr <- if (ns <= WIN) ceiling(ns/2) else (sample(seq_len(start_max),1) + HALF)
  }
  lo <- max(1L, ctr - (HALF-1L)); hi <- min(ns, lo + WIN - 1L); lo <- max(1L, hi - WIN + 1L)
  list(text = paste(sents$text[lo:hi], collapse=" "), top_bigram = topb, nse = hi-lo+1L)
}

pool <- vector("list", nrow(samp))
for (yr in sort(unique(samp$year))) {
  sp <- file.path(CORP, sprintf("sentences_%d.rds", yr)); if (!file.exists(sp)) { say("  [warn] missing %s", sp); next }
  S <- as.data.table(readRDS(sp)); setkey(S, Id, sentence_id)
  ids <- samp[year==yr, Id]
  Ssub <- S[J(unique(ids))]; rm(S); gc(FALSE)
  for (i in which(samp$year==yr)) {
    row <- samp[i]; ss <- Ssub[Id==row$Id][order(sentence_id), .(sentence_id, text)]
    sn <- build_snippet(ss, is_nonzero = row$decile > 0)
    if (is.null(sn)) next
    pool[[i]] <- data.table(Id=row$Id, year=row$year, decile=row$decile, GeoExposure=row$GeoExposure,
                            n_dict_hits=row$n_dict_hits, top_bigram=sn$top_bigram,
                            n_snippet_sentences=sn$nse, snippet_text=sn$text)
  }
  say("  [year %d] built %d snippets", yr, length(ids))
}
POOL <- rbindlist(Filter(Negate(is.null), pool))
setorder(POOL, decile, year)
nz_bigram <- POOL[decile>0, mean(!is.na(top_bigram) & nzchar(top_bigram))]
say("[check] nonzero-decile snippets centered on a real dict bigram: %.0f%% (should be ~100%%)", 100*nz_bigram)
POOL[, snippet_id := sprintf("S%03d", .I)]
setcolorder(POOL, c("snippet_id","Id","year","decile","GeoExposure","n_dict_hits","top_bigram","n_snippet_sentences","snippet_text"))
fwrite(POOL, file.path(VAL,"snippet_audit_pool.csv"))
say("\n[done] wrote out/validation/snippet_audit_pool.csv : %s snippets, %s with text",
    nrow(POOL), sum(nzchar(POOL$snippet_text)))
say("  median snippet length: %.0f chars | strata: %s", median(nchar(POOL$snippet_text)), paste(sort(unique(POOL$decile)),collapse=","))
