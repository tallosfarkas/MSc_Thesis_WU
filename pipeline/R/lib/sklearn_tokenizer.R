# ==============================================================================
# pipeline/R/lib/sklearn_tokenizer.R
#
# THE parity anchor for the whole pipeline. Mirrors sklearn's:
#   CountVectorizer(analyzer='word',
#                   strip_accents='unicode',
#                   ngram_range=(2,2),
#                   lowercase=True,
#                   stop_words='english')
# exactly. Used by every downstream stage so the bigram space is identical to
# Sautner's published replication code (SvLVZ Do File Table 2 and IA Table 9,11.py
# lines 38-40).
#
# Parity decisions:
#   - lowercase    : tokens_tolower() — quanteda default; sklearn lowercase=True
#   - strip accents: stringi::stri_trans_general(x, "Latin-ASCII") BEFORE
#                    tokenization. sklearn strip_accents='unicode' uses
#                    unicodedata.normalize('NFKD') then strips combining marks.
#                    Latin-ASCII via ICU produces equivalent results for the
#                    Latin script that dominates earnings calls.
#   - token regex  : '\\b[[:alnum:]]{2,}\\b' — sklearn default is
#                    '(?u)\\b\\w\\w+\\b'. \w in Python (?u) = [\p{L}\p{Nd}_].
#                    R's [[:alnum:]] in PCRE is similar (letters + digits, plus
#                    underscore via \w). The +2 length is the same.
#   - stopwords    : the FROZEN 318-word sklearn ENGLISH_STOP_WORDS vendored at
#                    pipeline/config/stopwords_sklearn.yml. NOT quanteda's
#                    stopwords("en") (Snowball, 175 words). NOT optional.
#   - ngrams       : tokens_ngrams(n=2L) — only bigrams, never unigrams.
#   - concatenator : " " (single space), matching sklearn's join behaviour for
#                    n-gram strings.
#
# Verification (audit 2026-06-12): pipeline/verify/tokenizer_parity_check.{R,py}
# diffs bag-of-bigrams against genuine sklearn on 1,000 sampled sentences.
# Measured on 2019: V1 (this ICU path) = 60.9% byte-identical (hyphen/possessive
# deletion + the on/off/no stopword YAML bug); V2 (tokenize_sklearn_v2, GEOV2=1)
# = 100.0% byte-identical. The locked dictionary came from genuine sklearn, so
# V2 is the parity-correct scoring path; V1 is frozen for reproducibility.
# ==============================================================================

suppressPackageStartupMessages({
  library(stringi)
  library(quanteda)
})

# ---- Internal: cache the sklearn stopword list once per session --------------
.sklearn_stopwords_cache <- NULL

.get_sklearn_stopwords <- function() {
  if (is.null(.sklearn_stopwords_cache)) {
    .sklearn_stopwords_cache <<- load_stopwords_sklearn()
    if (length(.sklearn_stopwords_cache) != 318L) {
      stop(sprintf(
        "stopwords_sklearn.yml has %d words but sklearn ENGLISH_STOP_WORDS has 318. ",
        length(.sklearn_stopwords_cache)),
        "Regenerate with pipeline/verify/extract_sklearn_stopwords.py.")
    }
  }
  .sklearn_stopwords_cache
}

# ==============================================================================
# tokenize_sklearn
#
# Take a character vector of texts and produce a quanteda `tokens` object
# of bigrams, byte-equivalent to sklearn CountVectorizer.
#
# @param texts character vector. One element per document (or sentence).
# @return     `tokens` object. Bigrams only, lowercase, accent-stripped,
#             sklearn-stopwords removed.
# ==============================================================================
tokenize_sklearn <- function(texts) {
  stopifnot(is.character(texts))
  if (exists("is_v2") && is_v2()) return(tokenize_sklearn_v2(texts))

  # Step 1: accent strip (Latin-ASCII via ICU). Done BEFORE tokenisation so
  # umlauts etc. collapse to their ASCII equivalents identically to sklearn.
  texts <- stri_trans_general(texts, "Latin-ASCII")

  # Step 2: build corpus + tokenise on word boundaries.
  # quanteda's tokens(what="word") uses ICU word boundaries — close to but
  # not byte-identical to sklearn's regex. We post-filter with the exact
  # sklearn regex to enforce parity.
  # KNOWN V1 PARITY GAP (audit 2026-06-12): ICU keeps hyphenated compounds and
  # possessives as SINGLE tokens ("supply-chain", "China's"), which the keep
  # regex below then deletes whole, while sklearn's \b\w\w+\b extracts the
  # alphanumeric parts ("supply","chain","China"). The V2 path fixes this with
  # the exact regex extraction; V1 behavior is preserved for reproducibility.
  toks <- tokens(texts,
                 what            = "word",
                 remove_punct    = TRUE,
                 remove_symbols  = TRUE,
                 remove_numbers  = FALSE,  # sklearn \w includes digits
                 remove_url      = TRUE,
                 remove_separators = TRUE)

  # Step 3: keep only tokens matching sklearn's '\b\w\w+\b' — alphanumeric
  # (incl. underscore) of length >= 2.
  toks <- tokens_keep(toks, pattern = "^[[:alnum:]_]{2,}$", valuetype = "regex")

  # Step 4: lowercase (after pattern keep — pattern is case-insensitive in regex
  # for [[:alnum:]] but the keep is faster on cased tokens; either order is fine).
  toks <- tokens_tolower(toks)

  # Step 5: remove sklearn's frozen 318-word stoplist.
  toks <- tokens_remove(toks, pattern = .get_sklearn_stopwords(), valuetype = "fixed")

  # Step 6: form bigrams.
  toks <- tokens_ngrams(toks, n = 2L, concatenator = " ")

  toks
}

# ==============================================================================
# V2 EXACT tokenizers (correctness track, GEOV2=1)
#
# sklearn CountVectorizer's pipeline is: strip_accents -> lowercase ->
# re.findall(r'(?u)\b\w\w+\b', text). \w under (?u) = [\p{L}\p{N}_]. A regex
# \b\w\w+\b match is exactly a MAXIMAL run of word characters of length >= 2,
# so stri_extract_all_regex with "[\\p{L}\\p{N}_]{2,}" reproduces it EXACTLY —
# "supply-chain" -> supply, chain; "China's" -> china (the trailing "s" is a
# length-1 run and is dropped, as in sklearn). No ICU word-boundary heuristics.
# ==============================================================================
.sklearn_word_lists <- function(texts) {
  texts <- stri_trans_general(texts, "Latin-ASCII")
  texts <- stri_trans_tolower(texts)
  lst <- stri_extract_all_regex(texts, "[\\p{L}\\p{N}_]{2,}")
  lapply(lst, function(x) if (length(x) == 1L && is.na(x[1])) character(0) else x)
}

tokenize_sklearn_v2 <- function(texts) {
  stopifnot(is.character(texts))
  toks <- as.tokens(.sklearn_word_lists(texts))
  toks <- tokens_remove(toks, pattern = .get_sklearn_stopwords(), valuetype = "fixed")
  tokens_ngrams(toks, n = 2L, concatenator = " ")
}

tokenize_unigrams_v2 <- function(texts) {
  stopifnot(is.character(texts))
  as.tokens(.sklearn_word_lists(texts))
}

# ==============================================================================
# dfm_sklearn
#
# Convenience: tokenize_sklearn() -> dfm(). Returns a sparse document-feature
# matrix of bigram counts.
# ==============================================================================
dfm_sklearn <- function(texts) {
  dfm(tokenize_sklearn(texts))
}

# ==============================================================================
# tokenize_unigrams
#
# UNIGRAM tokeniser for Stage 3 Eq.2/Eq.3 sentence-level conditioning (detecting
# risk words and Loughran-McDonald tone words in a sentence). Runs the SAME
# normalisation as tokenize_sklearn steps 1-4 (Latin-ASCII strip, ICU word
# tokens, alnum>=2 keep, lowercase) so "risk", "concern" etc. tokenise
# identically on both sides — but deliberately OMITS stopword removal and
# bigramming. (Risk/tone words are not stopwords; we test set-membership.)
# @return `tokens` object of lowercase unigrams.
# ==============================================================================
tokenize_unigrams <- function(texts) {
  stopifnot(is.character(texts))
  if (exists("is_v2") && is_v2()) return(tokenize_unigrams_v2(texts))
  texts <- stri_trans_general(texts, "Latin-ASCII")
  toks <- tokens(texts, what = "word", remove_punct = TRUE, remove_symbols = TRUE,
                 remove_numbers = FALSE, remove_url = TRUE, remove_separators = TRUE)
  toks <- tokens_keep(toks, pattern = "^[[:alnum:]_]{2,}$", valuetype = "regex")
  tokens_tolower(toks)
}
