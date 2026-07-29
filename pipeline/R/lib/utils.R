# ==============================================================================
# pipeline/R/lib/utils.R
#
# Shared utilities: config loader, paths, logging.
# Pure helpers — no Sautner-specific logic.
# ==============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(data.table)
})

# ---- Project root ------------------------------------------------------------
# Returns the absolute path of the project root, regardless of where the
# calling script was invoked from. The project root contains `pipeline/`.
project_root <- function() {
  # If PIPELINE_ROOT is set explicitly, honour it.
  e <- Sys.getenv("PIPELINE_ROOT")
  if (nzchar(e)) return(normalizePath(e, mustWork = TRUE))
  # Otherwise, walk up from cwd until we find pipeline/config/params.yml
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root. Set PIPELINE_ROOT env var.")
}

# ---- Config loaders ----------------------------------------------------------
load_params <- function() {
  read_yaml(file.path(project_root(), "pipeline", "config", "params.yml"))
}

# yaml::read_yaml returns a list of single-element character vectors for
# YAML sequences. Downstream code (quanteda, stringi) wants flat character
# vectors. unlist() + as.character() guarantees that shape.
.flatten_chr <- function(x) as.character(unlist(x, use.names = FALSE))

load_seeds <- function() {
  .flatten_chr(read_yaml(file.path(project_root(), "pipeline", "config",
                                   "seeds.yml"))$seeds)
}

load_stopwords_sklearn <- function() {
  f <- file.path(project_root(), "pipeline", "config", "stopwords_sklearn.yml")
  if (is_v2()) {
    # V2 fix (audit 2026-06-12): YAML 1.1 parses the bare scalars on/off/no as
    # BOOLEANS, so the v1 loader silently turned three sklearn stopwords into
    # "TRUE"/"FALSE" — "on", "off", "no" were never removed in any R-path run.
    # Keep them as strings via type handlers. (v1 keeps the bug for byte-level
    # reproducibility of the frozen outputs; documented in the audit register.)
    return(.flatten_chr(read_yaml(f, handlers = list(
      "bool#yes" = function(v) v, "bool#no" = function(v) v))$stopwords))
  }
  .flatten_chr(read_yaml(f)$stopwords)
}

load_stopwords_domain <- function() {
  .flatten_chr(read_yaml(file.path(project_root(), "pipeline", "config",
                                   "stopwords_domain.yml"))$domain_stopword_bigrams)
}

load_risk_words <- function() {
  # V2 correctness track: use the Hassan/Sautner-reconciled list when GEOV2 is set
  # and the file exists (falls back to the original list otherwise).
  f <- file.path(project_root(), "pipeline", "config", "risk_words.yml")
  if (is_v2()) {
    f2 <- file.path(project_root(), "pipeline", "config", "risk_words_v2.yml")
    if (file.exists(f2)) f <- f2
  }
  # GEO_RISKWORDS override (robustness): a filename under pipeline/config or an
  # absolute path REPLACES the risk list. Used for the risk-word faithfulness
  # check (e.g. risk_words_clean.yml drops the circular 'exposure'/'exposed').
  rw <- Sys.getenv("GEO_RISKWORDS")
  if (nzchar(rw)) {
    f3 <- if (file.exists(rw)) rw else file.path(project_root(), "pipeline", "config", rw)
    if (file.exists(f3)) f <- f3 else stop("GEO_RISKWORDS file not found: ", rw)
  }
  .flatten_chr(read_yaml(f)$risk_words)
}

# ---- V2 correctness track (methodology audit 2026-06-12) -----------------------
# GEOV2=1 switches every stage to the corrected pipeline and _v2 output paths,
# leaving the default run byte-identical to the current (v1) version. V2 fixes:
# exact-sklearn tokenizer, reconciled risk words, raw-return portfolios, seeded
# quintile tie-breaking, region join on full RIC, lead returns without
# call-in-t+1 selection. Register: 30_results/Methodology_audit_full_2026-06-12.
is_v2   <- function() nzchar(Sys.getenv("GEOV2"))
v2_file <- function(path) if (is_v2()) sub("\\.([A-Za-z0-9]+)$", "_v2.\\1", path) else path
v2_dir  <- function(d) { d <- if (is_v2()) paste0(d, "_v2") else d
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
v2_seed <- 20250401L   # fixed seed for any v2 randomised step (tie-breaking)

# Exposure-directory router. Default = out/exposure (v1/v1.1 headline, UNCHANGED).
# GEOV2 or GEO_EXPO=v2 -> out/exposure_v2 (exact-tokenizer track, UNCHANGED).
# Any other GEO_EXPO=<x> -> out/exposure_<x> (additive robustness tracks, e.g.
# GEO_EXPO=rwclean for the risk-word faithfulness check). Never touches the DFM
# dir, so a robustness track reuses the locked dictionary unless it also sets GEOV2.
exp_dirname <- function() {
  e <- Sys.getenv("GEO_EXPO")
  if (nzchar(Sys.getenv("GEOV2")) || identical(e, "v2")) return("exposure_v2")
  if (nzchar(e)) return(paste0("exposure_", e))
  "exposure"
}
exp_dir <- function() {                       # absolute path, creates if missing
  d <- file.path(project_root(), "out", exp_dirname())
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE); d }

# Locked geoeconomic dictionary -> character vector of bigrams.
#   which = "primary" (unpruned 9,650, MAIN/headline, Sautner-faithful)
#         | "pruned"  (curated 4,586, ROBUSTNESS only)
#         | "exenergy" (energy-dropped robustness variant)
load_dictionary <- function(which = c("primary", "pruned", "exenergy")) {
  # GEO_DICT override: point-in-time / vintage dictionary for the expanding-window
  # OOS analysis. If set (abs path, or a name under pipeline/config), it REPLACES
  # the configured dictionary for every `which` (downstream we only use the
  # primary measures). Same contract: returns the bigram column.
  gd <- Sys.getenv("GEO_DICT")
  if (nzchar(gd)) {
    path <- if (file.exists(gd)) gd else file.path(project_root(), "pipeline", "config", gd)
    if (!file.exists(path)) stop("GEO_DICT not found: ", path)
    return(as.character(data.table::fread(path)$bigram))
  }
  which <- match.arg(which)
  P <- load_params()
  fname <- switch(which,
    primary  = if (!is.null(P$exposure$dictionary_primary)) P$exposure$dictionary_primary else "dictionary_geoeconomic.csv",
    pruned   = if (!is.null(P$exposure$dictionary_pruned))  P$exposure$dictionary_pruned  else "dictionary_geoeconomic_pruned.csv",
    exenergy = if (!is.null(P$exposure$dictionary_exenergy)) P$exposure$dictionary_exenergy else "dictionary_geoeconomic_exenergy.csv")
  path <- file.path(project_root(), "pipeline", "config", fname)
  if (!file.exists(path)) stop("Dictionary not found: ", path)
  as.character(data.table::fread(path)$bigram)
}

# Loughran-McDonald 2011 tone word lists for Eq.2 (GeoSentiment).
# Returns list(positive=<chr>, negative=<chr>) lowercased, or NULL (with a
# warning) if the master CSV is absent so Eq.2 can be skipped rather than crash.
# Membership = category column > 0 (a negative-signed year flags a removed word).
load_lm_words <- function() {
  P <- load_params()
  fname <- if (!is.null(P$exposure$lm_dictionary)) P$exposure$lm_dictionary else "lm_master_dictionary.csv"
  path <- file.path(project_root(), "pipeline", "config", fname)
  if (!file.exists(path)) {
    warning("Loughran-McDonald dictionary not found at ", path,
            " - Eq.2 (sentiment) will be SKIPPED. Download the Master Dictionary from ",
            "https://sraf.nd.edu/loughran-mcdonald-master-dictionary/ and place it there.")
    return(NULL)
  }
  lm <- data.table::fread(path, encoding = "Latin-1")
  list(positive = tolower(lm$Word[lm$Positive > 0]),
       negative = tolower(lm$Word[lm$Negative > 0]))
}

# ---- Paths -------------------------------------------------------------------
parsed_file <- function(year) {
  file.path(project_root(), "data", "parsed",
            sprintf("TParsed_%d.RData", as.integer(year)))
}

out_dir <- function(...) {
  d <- file.path(project_root(), "out", ...)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# ---- Logging -----------------------------------------------------------------
log_step <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", sprintf(...))
  cat(msg, "\n", sep = "")
  flush(stdout())
}
