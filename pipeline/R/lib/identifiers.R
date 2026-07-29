# ==============================================================================
# pipeline/R/lib/identifiers.R  (Stage 3.5 — firm identifier canonicalisation)
#
# Two RIC cleaners for the dual mapping:
#   clean_ric()    — KEEP the exchange suffix, strip only the ^delisting marker.
#                    MDP.N^L21 -> MDP.N ; DSCP.O^B09 -> DSCP.O ; 000001.SZ -> 000001.SZ
#                    Matches the RIC form used by CLEAN_QUARTERLY_PRICING_v2$Ticker
#                    (e.g. A.N, AAP.N) -> for the GLOBAL/LSEG returns join.
#   clean_ticker() — strip exchange suffix AND markers (lifted verbatim from
#                    ra_project/mapping/R/clean_ticker.R) -> CRSP base ticker.
# ==============================================================================

# Lifted from ra_project/mapping/R/clean_ticker.R (credited prior work).
clean_ticker <- function(x) {
  x <- toupper(trimws(x))
  x <- sub("\\..*$", "", x)        # strip RIC exchange suffix: AAPL.O -> AAPL
  x <- sub("[*^~]+.*$", "", x)     # strip CRSP special-status / delisting markers
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

# RIC canonicaliser for the LSEG price join: keep the exchange suffix, drop only
# the trailing delisting marker (everything from the first caret onward).
clean_ric <- function(x) {
  x <- toupper(trimws(x))
  x <- sub("\\^.*$", "", x)        # MDP.N^L21 -> MDP.N  (keep .N)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}
