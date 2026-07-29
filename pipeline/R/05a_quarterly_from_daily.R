# ==============================================================================
# pipeline/R/05a_quarterly_from_daily.R   (Stage 5 — derive quarterly from daily)
#
# Re-derives the quarterly returns panel by COMPOUNDING the daily LSEG panel, so
# daily / monthly (05g) / quarterly all come from ONE source and the augmented
# (topped-up) tickers flow through automatically. Matches the CLEAN_QUARTERLY_PRICING_v2
# schema (Ticker, Quarter, QuarterlyRet_W, MCap_QEnd) so 05_build_analysis_panel can
# use it as a drop-in. Run after a top-up + re-merge (LSEG_Final_Panel.parquet rebuilt).
#
#   Rscript pipeline/R/05a_quarterly_from_daily.R
# Output: out/analysis/quarterly_pricing_daily.rds
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(lubridate) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT, "out", "analysis"); if (!dir.exists(ANA)) dir.create(ANA, recursive=TRUE)
MIN_DAYS <- 40L; say <- function(...) cat(sprintf(...), "\n")

d <- as.data.table(read_parquet(file.path(ROOT, "data/processed/LSEG_Final_Panel.parquet"),
                                col_select = c("Date","Ticker","Ret","MCap")))
d <- d[is.finite(Ret)]; d[, Date := as.Date(Date)]
d[, Quarter := floor_date(Date, "quarter")]
say("daily rows: %s | tickers %s", format(nrow(d),big.mark=","), format(uniqueN(d$Ticker),big.mark=","))

q <- d[order(Ticker, Date),
       .(QuarterlyRet = prod(1 + Ret) - 1, MCap_QEnd = last(MCap[!is.na(MCap)]), n_days = .N),
       by = .(Ticker, Quarter)][n_days >= MIN_DAYS]
# winsorise returns within quarter (0.5/99.5), as in the monthly build
q[, QuarterlyRet_W := pmin(pmax(QuarterlyRet, quantile(QuarterlyRet,.005,na.rm=TRUE)),
                           quantile(QuarterlyRet,.995,na.rm=TRUE)), by = Quarter]
say("quarterly panel: %s rows | %s tickers | %s..%s",
    format(nrow(q),big.mark=","), format(uniqueN(q$Ticker),big.mark=","),
    as.character(min(q$Quarter)), as.character(max(q$Quarter)))
saveRDS(q[, .(Ticker, Quarter, QuarterlyRet, QuarterlyRet_W, MCap_QEnd, n_days)],
        file.path(ANA, "quarterly_pricing_daily.rds"))
say("wrote out/analysis/quarterly_pricing_daily.rds (drop-in for CLEAN_QUARTERLY_PRICING_v2)")
