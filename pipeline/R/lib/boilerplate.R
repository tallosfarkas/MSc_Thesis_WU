# ==============================================================================
# pipeline/R/lib/boilerplate.R
#
# Strip legal / safe-harbour / forward-looking boilerplate from earnings-call
# text BEFORE tokenisation.
#
# Lifted verbatim from prior RA work at:
#   ra_project/R/03_corpus.R:187-221  (remove_boilerplate + safe_harbor_pattern)
#
# This is load-bearing prior work — the regex was tuned by the user during the
# RA project for earnings-call transcripts and validated implicitly via the
# topic-modelling pipeline that followed. We keep it intact and credit it.
#
# Why this matters for Sautner:
#   The 2002 sample (out/sample_text_2002.txt) shows that ~30-40% of the
#   *Presentation*-section text in a typical call is the standard safe-harbour
#   disclosure ("forward-looking statements... actual results may differ...").
#   This text is identical across calls and inflates B_{i,t} in Sautner Eq. 1
#   without adding signal — both numerator and denominator suffer, but the
#   denominator more so. Stripping it improves the exposure ratio's
#   signal-to-noise.
#
# Sautner does not document this step explicitly, but it is a defensible
# extension that the field has converged on (Hassan et al. 2019 QJE strip
# similar boilerplate; HMP 2018 implicitly drop it via TF-IDF pruning).
#
# Usage:
#   source("pipeline/R/lib/boilerplate.R")
#   cleaned_text <- remove_boilerplate(raw_text)
# ==============================================================================

suppressPackageStartupMessages({
  library(stringr)
  library(stringi)
})

# Phrases that mark the START of a safe-harbour passage.
safe_harbor_pattern <- regex(
  paste0(
    "private securities litigation reform act|",
    "forward[- ]looking statements|safe harbor|",
    "actual results (may|could|might|should) differ|",
    "factors that could cause|refer to our (sec )?filings|",
    "form 10-k|cautionary statement"
  ),
  ignore_case = TRUE
)

# Phrases that mark the END of the safe-harbour passage (the call transitioning
# back to substantive content).
.boilerplate_end_patterns <- c(
  "with that,? (said|let me (turn|hand)|I'll turn|now)",
  "and now",
  "I will now turn",
  "let's begin",
  "moving to",
  "turning to"
)

#' Remove safe-harbour / forward-looking boilerplate from a single text string.
#'
#' If a known safe-harbour phrase is detected, the function:
#'   1. Searches for the earliest end-marker (e.g., "with that, let me turn").
#'   2. If found, returns everything from the end-marker onward.
#'   3. If no end-marker, falls back to the first paragraph break or sentence
#'      boundary after the safe-harbour phrase.
#'   4. If neither, returns the original text unchanged (conservative).
#'
#' @param text  character(1)  raw call text or speaker turn
#' @return      character(1)  text with boilerplate removed (or unchanged)
remove_boilerplate_once <- function(text) {
  if (is.na(text) || text == "") return(text)

  if (str_detect(text, safe_harbor_pattern)) {
    # Try end-markers first
    for (pattern in .boilerplate_end_patterns) {
      match <- str_locate(text, regex(pattern, ignore_case = TRUE))
      if (!is.na(match[1])) {
        return(str_sub(text, match[1], -1))
      }
    }
    # Fallback: skip to first paragraph break or sentence boundary
    boilerplate_match <- str_locate(text, safe_harbor_pattern)
    if (!is.na(boilerplate_match[2])) {
      next_break <- str_locate(
        str_sub(text, boilerplate_match[2], -1),
        "\\n\\n|\\. [A-Z]"
      )
      if (!is.na(next_break[1])) {
        return(str_sub(text, boilerplate_match[2] + next_break[1], -1))
      }
    }
  }
  text
}

#' Iterative wrapper: apply remove_boilerplate_once until the safe-harbour
#' regex no longer matches (or the text is empty). Capped at 20 iterations
#' to guarantee termination on pathological inputs.
#'
#' This is the default. Use when boilerplate spans multiple sentences (common
#' for forward-looking statements that list many risk factors).
remove_boilerplate <- function(text, max_iter = 20L) {
  if (is.na(text) || text == "") return(text)
  for (i in seq_len(max_iter)) {
    if (!str_detect(text, safe_harbor_pattern)) break
    new_text <- remove_boilerplate_once(text)
    if (identical(new_text, text)) break   # no progress -> stop
    text <- new_text
    if (nchar(text) == 0L) break
  }
  text
}

# ==============================================================================
# Transcript stage-direction / interpreter markers.
# LSEG/Refinitiv transcripts insert literal markers like "[Foreign Language]",
# "[Interpreted]", "[Inaudible]" when a segment is non-English, interpreted, or
# unclear. After tokenisation these become bigrams ("foreign language",
# "language interpreted", "language foreign") that the KLR classifier latches
# onto as *target-discriminative* — because interpreted segments cluster in
# non-US / international calls, which discuss trade and geopolitics more. They
# are pure metadata, not geoeconomic content, so they bias the measure toward
# "this firm had a foreign-language segment". We strip them before tokenisation.
# (Diagnosed 2026-06-03: "foreign language" occurred ~79k times in the corpus
# and ranked highly in every discovered dictionary.)
# ==============================================================================
transcript_marker_pattern <- regex(
  paste0(
    # bracketed or parenthesised stage directions
    "[\\[\\(]\\s*(speaking\\s+)?(in\\s+)?(foreign language|foreign languages|",
    "non[- ]?english|interpreted|interpretation|inaudible|indiscernible|",
    "audio gap|technical difficult(y|ies)|operator instructions|background noise|",
    "no audio|music playing|call to order)\\s*[\\]\\)]|",
    # bare residual forms (the markers above with brackets already removed)
    "\\bforeign language(s)?\\b|\\blanguage interpreted\\b|\\bspeaking foreign\\b"
  ),
  ignore_case = TRUE
)

#' Strip transcript stage-direction / interpreter markers from a text string.
#' @param text character(1)
#' @return character(1) with markers replaced by a space and whitespace squished.
strip_transcript_markers <- function(text) {
  if (is.na(text) || text == "") return(text)
  str_squish(str_replace_all(text, transcript_marker_pattern, " "))
}

# ==============================================================================
# Sentence-level filter — second pass.
# After remove_boilerplate (turn-level) and sentence-splitting, we DROP whole
# sentences that still contain any safe-harbour keyword. Catches residual
# legal-disclaimer sentences that the regex couldn't peel off cleanly.
# ==============================================================================

#' TRUE if the sentence contains any safe-harbour keyword.
#' Vectorised over `sentences`.
is_safeharbor_sentence <- function(sentences) {
  if (length(sentences) == 0L) return(logical(0))
  # Anchors are deliberately NARROW. Earlier this regex also matched bare
  # "risk factors" / "risks and uncertainties", which dropped substantive
  # geoeconomic-risk sentences ("the key risk factors here are tariffs and
  # export controls...") — exactly the target language KLR mines. Removed those
  # two loose phrases (pipeline-reviewer finding, 2026-06-03); keep only
  # unambiguous legal-disclaimer anchors.
  stri_detect_regex(sentences,
                    "(?i)private securities litigation|forward[- ]looking|safe.harbor|safe.harbour|actual results (may|could|might|should) differ|factors that could cause|refer to our (sec )?filings|form 10[- ]?k|cautionary statement")
}
