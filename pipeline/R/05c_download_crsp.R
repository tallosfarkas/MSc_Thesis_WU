# ==============================================================================
# pipeline/R/05c_download_crsp.R   (Stage 5 — CRSP returns for the US/CRSP layer)
#
# Pulls CRSP monthly stock returns (delisting-adjusted) for the permnos in our
# CRSP-mapped exposure panel and builds a quarterly return panel keyed by permno,
# so the US robustness analysis uses CRSP returns (not LSEG). Reuses the legacy
# WRDS connection pattern (scripts/00_download/06_download_bm_wrds.R).
#
# Credentials (NOT stored in the repo): set in ~/.Renviron, then restart R:
#     WRDS_USER=your_wrds_username
#     WRDS_PASSWORD=your_wrds_password
#
#   Rscript pipeline/R/05c_download_crsp.R
# Output: out/analysis/crsp_returns_quarterly.rds (permno, Quarter, RetQ, MCap_QEnd)
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(DBI); library(RPostgres) })
.find_root <- function() {
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") {
    if (file.exists(file.path(d, "pipeline", "config", "params.yml"))) return(d)
    d <- dirname(d) }
  stop("Could not locate project root.")
}
ROOT <- .find_root()
ANA <- file.path(ROOT, "out", "analysis"); if (!dir.exists(ANA)) dir.create(ANA, recursive = TRUE)
say <- function(...) cat(sprintf(...), "\n")

usr <- Sys.getenv("WRDS_USER"); pwd <- Sys.getenv("WRDS_PASSWORD")
if (usr == "" || pwd == "")
  stop("WRDS_USER / WRDS_PASSWORD not set. Add them to ~/.Renviron and restart R:\n",
       "  WRDS_USER=your_username\n  WRDS_PASSWORD=your_password")

# permnos to fetch = the CRSP-mapped exposure universe (keeps the pull small)
fq <- as.data.table(readRDS(file.path(ROOT, "out", "exposure", "exposure_firmquarter_crsp.rds")))
permnos <- sort(unique(as.integer(fq$permno))); permnos <- permnos[!is.na(permnos)]
say("[crsp] fetching %d permnos, 2002-2025", length(permnos))

con <- dbConnect(Postgres(), host = "wrds-pgdata.wharton.upenn.edu", port = 9737L,
                 dbname = "wrds", user = usr, password = pwd, sslmode = "require")
on.exit(dbDisconnect(con), add = TRUE)
plist <- paste(permnos, collapse = ",")

# CRSP monthly, NEW versioned table crsp.msf_v2 (CIZ format): mthret already incorporates
# the delisting return, mthcap is market cap -> no separate delisting merge. The legacy
# crsp.msf ends 2024-12 (CRSP vintage lag); msf_v2 reaches 2025-12 (matches the LSEG sample).
# CIZ common-stock filter mirrors the old shrcd IN(10,11) + exchcd IN(1,2,3).
msf <- setDT(dbGetQuery(con, sprintf("
  SELECT permno, mthcaldt AS date, mthret AS ret, mthcap AS mcap
  FROM crsp.msf_v2
  WHERE mthcaldt BETWEEN '2002-01-01' AND '2025-12-31'
    AND sharetype='NS' AND securitytype='EQTY' AND securitysubtype='COM'
    AND usincflg='Y' AND primaryexch IN ('N','A','Q')
    AND permno IN (%s)", plist)))
say("[crsp] msf_v2 rows=%s", format(nrow(msf), big.mark=","))

msf[, date := as.Date(date)]
msf[, ret := as.numeric(ret)]; msf[, mcap := as.numeric(mcap)]
msf <- msf[!is.na(ret)]                                                 # mthret is delisting-inclusive (CIZ)
msf[, Quarter := as.Date(ISOdate(year(date), (quarter(date) - 1L) * 3L + 1L, 1L))]

# monthly panel (msf is already one row per permno-month, delisting-adjusted)
msf[, Month := as.Date(ISOdate(year(date), month(date), 1L))]
crspm <- msf[order(permno, date), .(permno, Month, RetM = ret, MCap_MEnd = mcap)]
saveRDS(crspm, file.path(ANA, "crsp_returns_monthly.rds"))
say("[crsp] monthly panel: %s rows | %s permno -> crsp_returns_monthly.rds",
    format(nrow(crspm), big.mark=","), format(uniqueN(crspm$permno), big.mark=","))

crspq <- msf[order(permno, date),
  .(RetQ = prod(1 + ret) - 1, MCap_QEnd = last(mcap[!is.na(mcap)]), n_months = .N),
  by = .(permno, Quarter)]
crspq <- crspq[n_months >= 2]                                          # need a near-complete quarter
say("[crsp] quarterly panel: %s rows | %s permno | %s..%s",
    format(nrow(crspq), big.mark=","), format(uniqueN(crspq$permno), big.mark=","),
    min(crspq$Quarter), max(crspq$Quarter))

saveRDS(crspq, file.path(ANA, "crsp_returns_quarterly.rds"))
say("[crsp] wrote out/analysis/crsp_returns_quarterly.rds")
