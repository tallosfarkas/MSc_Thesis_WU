# ==============================================================================
# pipeline/R/05l_tnic_spillover.R   (Stage 5 — Hoberg-Phillips TNIC peer spillover)
#
# Novel angle: does a firm's PRODUCT-MARKET PEERS' geoeconomic exposure predict the
# firm's own returns? Peers + weights come from the Hoberg-Phillips TNIC-3 pairwise
# similarity network (text-based, time-varying). For each firm-year:
#   PeerExpo_i,y = sum_j score_ij * Expo_j,y / sum_j score_ij   (j = TNIC peers)
# Then Fama-MacBeth of next-quarter CRSP return on OWN and PEER exposure (+ size):
# is the peer term priced, and does own-exposure survive controlling for it?
#
#   Rscript pipeline/R/05l_tnic_spillover.R   ->  out/analysis/tnic_spillover.json
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.find_root <- function() { d <- normalizePath(getwd(), mustWork = TRUE)
  while (d != "/" && d != "") { if (file.exists(file.path(d,"pipeline/config/params.yml"))) return(d); d <- dirname(d) }
  stop("no root") }
ROOT <- .find_root(); ANA <- file.path(ROOT,"out","analysis"); say <- function(...) cat(sprintf(...),"\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment"); NW <- 4L
csz <- function(x){s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) NA_real_ else (x-mean(x,na.rm=TRUE))/s}

# ---- exposure by gvkey-year (from the CRSP-mapped panel) --------------------
exp_dirname <- function() { .e <- Sys.getenv("GEO_EXPO"); if (nzchar(Sys.getenv("GEOV2"))||identical(.e,"v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_",.e) else "exposure" }
fq <- as.data.table(readRDS(file.path(ROOT, "out", exp_dirname(), "exposure_firmquarter_crsp.rds")))
fq[, gv := as.integer(gvkey)]; fq <- fq[!is.na(gv)]
expo_gy <- fq[, lapply(.SD, mean, na.rm=TRUE), by=.(gv, year), .SDcols=MEAS]
say("[tnic] exposure gvkey-years: %s | gvkeys %s", format(nrow(expo_gy),big.mark=","), format(uniqueN(expo_gy$gv),big.mark=","))

# ---- TNIC peers -> score-weighted peer exposure ----------------------------
tnic <- fread(file.path(ROOT,"data/inputs/hp_raw/tnic3_data/tnic3_data.txt"))   # year gvkey1 gvkey2 score
setnames(tnic, c("year","gvkey1","gvkey2","score"))
# drop self-pairs (g1==g2, score NA in this file) + any NA scores -> real peers only;
# otherwise a firm whose only surviving row is the NA-score self-pair gets peer = NA
tnic <- tnic[year >= 2001 & year <= 2025 & gvkey1 != gvkey2 & is.finite(score)]
peers <- merge(tnic, expo_gy, by.x=c("gvkey2","year"), by.y=c("gv","year"))      # peers that have exposure
# score-weighted peer means — reference columns directly in j (NOT lapply, so the
# `score` weight stays group-aligned)
peer <- peers[, .(
  peer_GeoExposure      = weighted.mean(GeoExposure,      score, na.rm=TRUE),
  peer_GeoExposureTFIDF = weighted.mean(GeoExposureTFIDF, score, na.rm=TRUE),
  peer_GeoRisk          = weighted.mean(GeoRisk,          score, na.rm=TRUE),
  peer_GeoSentiment     = weighted.mean(GeoSentiment,     score, na.rm=TRUE),
  n_peers = .N), by=.(gvkey1, year)]
say("[tnic] firm-years with peer exposure: %s | median peers ~ %.0f",
    format(nrow(peer),big.mark=","), median(peer$n_peers, na.rm=TRUE))

# ---- firm-quarter panel: own + peer exposure + CRSP fwd return -------------
fq[, Quarter := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
p <- merge(fq, peer, by.x=c("gv","year"), by.y=c("gvkey1","year"))
ret <- as.data.table(readRDS(file.path(ANA,"crsp_returns_quarterly.rds"))); ret[, permno:=as.integer(permno)]; p[, permno:=as.integer(permno)]
p <- merge(p, ret[, .(permno, Quarter, RetQ, MCap_QEnd)], by=c("permno","Quarter"))
setorder(p, permno, Quarter); p[, Ret_lead := shift(RetQ,1L,type="lead"), by=permno]
p[, Qn := shift(Quarter,1L,type="lead"), by=permno]; p[is.na(Qn)|as.integer(Qn-Quarter)>95L, Ret_lead := NA_real_]; p[,Qn:=NULL]
p[, size := log(pmax(MCap_QEnd,1))]; p <- p[!is.na(Ret_lead)]
say("[tnic] spillover panel: %s firm-quarters | %s firms | %d quarters", format(nrow(p),big.mark=","),
    format(uniqueN(p$permno),big.mark=","), uniqueN(p$Quarter))

nw_t <- function(v){v<-v[is.finite(v)]; m<-lm(v~1); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=NW,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]; c(mean=mean(v),t=mean(v)/se)}
fm <- function(rhs, term){ lam <- p[, { d<-.SD[complete.cases(.SD)]; if(nrow(d)<30) NA_real_ else coef(lm(as.formula(paste("Ret_lead ~",rhs)),d))[term] },
    by=Quarter, .SDcols=c("Ret_lead", all.vars(as.formula(paste("~",rhs))))]$V1
  s<-nw_t(lam); c(lambda=unname(s["mean"]), t=unname(s["t"])) }

res <- lapply(MEAS, function(meas){
  p[, own := csz(get(meas)), by=Quarter]; p[, pe := csz(get(paste0("peer_",meas))), by=Quarter]; p[, sz := csz(size), by=Quarter]
  list(measure=meas,
       peer_only   = fm("pe + sz", "pe"),
       own_only    = fm("own + sz", "own"),
       own_vs_peer_OWN  = fm("own + pe + sz", "own"),
       own_vs_peer_PEER = fm("own + pe + sz", "pe")) })

say("\n=== TNIC peer spillover — Fama-MacBeth (lambda %%/q, t) ===")
for(r in res){ say("\n  %s:", r$measure)
  say("    peer-only            peer  %.4f%%  t=%.2f", 100*r$peer_only["lambda"], r$peer_only["t"])
  say("    own-only             own   %.4f%%  t=%.2f", 100*r$own_only["lambda"], r$own_only["t"])
  say("    own+peer (joint)     own   %.4f%%  t=%.2f", 100*r$own_vs_peer_OWN["lambda"], r$own_vs_peer_OWN["t"])
  say("    own+peer (joint)     peer  %.4f%%  t=%.2f", 100*r$own_vs_peer_PEER["lambda"], r$own_vs_peer_PEER["t"]) }
write_json(list(results=res, n_obs=nrow(p)), vS(file.path(ANA,"tnic_spillover.json")), pretty=TRUE, auto_unbox=TRUE)
say("\n  wrote out/analysis/tnic_spillover.json")
