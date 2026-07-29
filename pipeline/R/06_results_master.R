# ==============================================================================
# pipeline/R/06_results_master.R   (Stage 6 — ONE-STOP-SHOP results export)
#
# Consolidates every Stage 4/5 result into a single reproducible source of truth:
#   (1) ingest all out/analysis/*.json into one nested list;
#   (2) RECOMPUTE the cuts the JSONs lack, from panel_ric (LSEG) + a CRSP quarterly
#       panel: per-quintile mean next-quarter return by measure (EW+VW), by REGION
#       (4-region, REGION_MAP.rds), by SAMPLE (LSEG vs CRSP), and by CHARACTERISTICS
#       (size/BM/momentum tercile x exposure quintile, FF5 alpha on the Q5-Q1 leg);
#   (3) emit RESULTS_MASTER.rds (nested) + RESULTS_MASTER.csv (tidy long headline
#       table) + results_master.json.
# Verifies the recomputed Q5-Q1 quintile spread reconciles with portfolio_sorts_ric.
#
#   Rscript pipeline/R/06_results_master.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(lubridate) })
have_nw <- requireNamespace("sandwich", quietly = TRUE)
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); ANA <- file.path(ROOT,"out","analysis")
say <- function(...) cat(sprintf(...),"\n")
# ---- V2 correctness track (audit 2026-06-12): GEOV2=1 -> corrected pipeline, _v2 paths ----
FIX  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_FIX"))            # all audit timing/inference fixes
EXV2 <- nzchar(Sys.getenv("GEOV2")) || identical(Sys.getenv("GEO_EXPO"),"v2")   # exact-tokenizer exposure (out/exposure_v2)
RAW  <- nzchar(Sys.getenv("GEOV2")) || nzchar(Sys.getenv("GEO_RAW"))            # raw (un-winsorised) portfolio returns
TAG  <- Sys.getenv("GEO_TAG"); if (!nzchar(TAG)) TAG <- if (nzchar(Sys.getenv("GEOV2"))) "_v2" else ""
V2 <- FIX                                                                       # back-compat alias (scripts gate corrections on V2)
vS <- function(p) if (nzchar(TAG)) sub("\\.([A-Za-z0-9]+)$", paste0(TAG,".\\1"), p) else p
TIEM <- if (FIX) "random" else "first"; if (FIX) set.seed(20250401L); if (nzchar(Sys.getenv("GEO_TIES"))) TIEM <- Sys.getenv("GEO_TIES")
RP <- if (RAW) "RetM" else "RetM_w"
EXP <- file.path(ROOT, if (EXV2) "out/exposure_v2" else "out/exposure")

MEAS <- c("GeoExposure","GeoExposureTFIDF","GeoRisk","GeoSentiment")
MLAB <- c(GeoExposure="Count", GeoExposureTFIDF="TF-IDF", GeoRisk="Risk", GeoSentiment="Sentiment")

# ---- 0. ingest every result JSON --------------------------------------------
# V2: prefer the _v2 twin under its BASE name, fall back to v1 where no twin
# exists (and say which); v1 ignores _v2 files entirely.
jfiles <- list.files(ANA, pattern="\\.json$", full.names=TRUE)
jfiles <- jfiles[!grepl("results_master(_v2)?\\.json$", jfiles)]
allv <- jfiles[grepl("_v(2|3|11)\\.json$", jfiles)]            # any version twin
if (nzchar(TAG)) {
  mine <- jfiles[grepl(paste0(TAG,"\\.json$"), jfiles)]
  base <- sub(paste0(TAG,"\\.json$"), ".json", mine)
  v1f  <- setdiff(jfiles[!jfiles %in% allv], base)            # v1 base files lacking a twin for this version
  jfiles <- c(mine, v1f)
  jnames <- sub(paste0(TAG,"$"), "", sub("\\.json$", "", basename(jfiles)))
  say("[0] %s ingest: %d twins + %d v1 fallbacks (%s)", TAG, length(mine), length(v1f),
      paste(sub("\\.json$","",basename(v1f)), collapse=", "))
} else {
  jfiles <- jfiles[!jfiles %in% allv]
  jnames <- sub("\\.json$", "", basename(jfiles))
}
J <- setNames(lapply(jfiles, function(f) tryCatch(fromJSON(f), error=function(e) list(error=conditionMessage(e)))),
              jnames)
say("[0] ingested %d result JSONs: %s", length(J), paste(names(J), collapse=", "))

# ---- helpers ----------------------------------------------------------------
qbin <- function(x){ as.integer(ceiling(frank(x, ties.method=TIEM)/length(x)*5)) }   # 1..5, matches 05b
tbin <- function(x){ as.integer(ceiling(frank(x, ties.method=TIEM)/length(x)*3)) }   # terciles
nw_t <- function(v, lag=4L){ v<-v[is.finite(v)]; if(length(v)<5) return(c(mean=NA,t=NA))
  m<-lm(v~1); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=lag,prewhite=FALSE,adjust=TRUE)[1,1]) else summary(m)$coef[1,2]
  c(mean=mean(v), t=mean(v)/se) }
ff5q <- { f<-fread(file.path(ROOT,"data/inputs/ff5_factors_monthly.csv")); f[,Q:=floor_date(as.Date(Date),"quarter")]
  fc<-c("MktRF","SMB","HML","RMW","CMA","RF"); g<-f[,lapply(.SD,function(x)prod(1+x)-1),.SDcols=fc,by=Q]; setnames(g,"Q","Quarter"); g }
if (V2) ff5q[, Quarter := Quarter %m-% months(3)]   # audit fix: LS keyed by formation t holds the t+1 return
ff5_alpha <- function(ls){ d<-merge(ls,ff5q,by="Quarter"); d<-d[is.finite(LS)&is.finite(MktRF)]; if(nrow(d)<8) return(c(alpha=NA,t=NA))
  m<-lm(LS~MktRF+SMB+HML+RMW+CMA,d); se<-if(have_nw) sqrt(sandwich::NeweyWest(m,lag=4,prewhite=FALSE,adjust=TRUE)[1,1]) else sqrt(diag(vcov(m)))[1]
  c(alpha=unname(coef(m)[1]), t=unname(coef(m)[1]/se)) }

# per-quintile mean Ret_lead by measure on a panel with (Quarter, Ret_lead, MCap, measures)
quintile_means <- function(p, meas, wcol="MCap_QEnd"){
  d <- p[is.finite(get(meas)) & is.finite(Ret_lead)]
  d[, w := as.numeric(get(wcol))]
  d[, q := qbin(get(meas)), by=Quarter]
  ew <- d[, .(ew=mean(Ret_lead)), by=.(Quarter,q)]
  vw <- d[is.finite(w) & w>0, .(vw=weighted.mean(Ret_lead, w)), by=.(Quarter,q)]
  ewm <- ew[, .(ew=mean(ew)), by=q][order(q)]
  vwm <- vw[, .(vw=mean(vw)), by=q][order(q)]
  ls  <- dcast(ew, Quarter~q, value.var="ew"); ls[, LS:=get("5")-get("1")]
  s   <- nw_t(ls$LS, 4L)
  list(ew=setNames(ewm$ew, paste0("Q",ewm$q)), vw=setNames(vwm$vw, paste0("Q",vwm$q)),
       ls_ew_mean=unname(s["mean"]), ls_ew_t=unname(s["t"]), n=nrow(d))
}

# ---- 1. load LSEG panel + build CRSP quarterly panel ------------------------
P <- as.data.table(readRDS(vS(file.path(ANA,"panel_ric.rds"))))
say("[1] LSEG panel: %s firm-quarters | %s firms | %d quarters", format(nrow(P),big.mark=","),
    format(uniqueN(P$Ticker),big.mark=","), uniqueN(P$Quarter))

fqc <- as.data.table(readRDS(file.path(EXP,"exposure_firmquarter_crsp.rds"))); fqc[,permno:=as.integer(permno)]
fqc[, Quarter := as.Date(ISOdate(year,(quarter-1L)*3L+1L,1L))]
retc <- as.data.table(readRDS(file.path(ANA,"crsp_returns_quarterly.rds"))); retc[,permno:=as.integer(permno)]
C <- merge(fqc, retc[,.(permno,Quarter,RetQ,MCap_QEnd)], by=c("permno","Quarter"))
setorder(C, permno, Quarter); C[, Ret_lead := shift(RetQ,1L,type="lead"), by=permno]
C[, qn := shift(Quarter,1L,type="lead"), by=permno]; C[is.na(qn)|as.integer(qn-Quarter)>95L, Ret_lead:=NA_real_]; C[,qn:=NULL]
C <- C[is.finite(Ret_lead)]
say("    CRSP panel: %s firm-quarters | %s permno", format(nrow(C),big.mark=","), format(uniqueN(C$permno),big.mark=","))

# ---- 2. per-quintile means by measure (LSEG + CRSP) -------------------------
quint_lseg <- setNames(lapply(MEAS, function(m) quintile_means(P, m)), MEAS)
quint_crsp <- setNames(lapply(MEAS, function(m) quintile_means(C, m)), MEAS)
say("[2] per-quintile means computed (LSEG + CRSP), %d measures", length(MEAS))

# ---- 3. quintile means by REGION (LSEG) -------------------------------------
rmap <- as.data.table(readRDS(file.path(ROOT,"data/processed/REGION_MAP.rds")))
nb <- function(x) gsub("^[^A-Z0-9]+","", toupper(sub("\\..*$","",x)))
rmap[, base := nb(Ticker)]; rmap <- unique(rmap[!is.na(base) & base!="", .(base, Region)], by="base")
PR <- copy(P); PR[, base := nb(Ticker)]; PR <- rmap[PR, on="base"]
PR[is.na(Region), Region := "Other"]
cov_reg <- 100*mean(PR$Region != "Other")
regions <- c("NorthAmerica","Europe","AsiaPacific_DM","EM")
region_quint <- function(meas){
  d <- PR[is.finite(get(meas)) & is.finite(Ret_lead)]
  d[, q := qbin(get(meas)), by=Quarter]                          # global breakpoints (all firms)
  cell <- d[Region %in% regions, .(ew=mean(Ret_lead)), by=.(Quarter,Region,q)]
  means <- cell[, .(ew=mean(ew)), by=.(Region,q)][order(Region,q)]
  spr <- dcast(cell, Quarter+Region~q, value.var="ew")
  spr[, LS := get("5")-get("1")]
  sp <- spr[, as.list(nw_t(LS,4L)), by=Region]
  list(means=means, spread=sp) }
region_res <- setNames(lapply(MEAS, region_quint), MEAS)
say("[3] region cut: %.1f%% of firm-quarters mapped to a region (rest 'Other')", cov_reg)

# ---- 4. by CHARACTERISTICS: tercile x quintile double sorts (FF5 alpha Q5-Q1)
char_double <- function(p, meas, charcol){
  d <- p[is.finite(get(meas)) & is.finite(Ret_lead) & is.finite(get(charcol))]
  d[, tg := tbin(get(charcol)), by=Quarter]
  d[, q  := qbin(get(meas)),    by=Quarter]
  out <- lapply(1:3, function(g){ dd<-d[tg==g]
    ew <- dd[, .(ew=mean(Ret_lead)), by=.(Quarter,q)]
    w  <- dcast(ew, Quarter~q, value.var="ew"); if(!all(c("1","5") %in% names(w))) return(c(alpha=NA,t=NA))
    w[, LS:=get("5")-get("1")]; ff5_alpha(w[,.(Quarter,LS)]) })
  setNames(out, c("T1","T2","T3")) }
char_res <- list(
  size     = setNames(lapply(MEAS, function(m) char_double(P, m, "Size")),     MEAS),
  bm       = setNames(lapply(MEAS, function(m) char_double(P, m, "BM")),       MEAS),
  momentum = setNames(lapply(MEAS, function(m) char_double(P, m, "Momentum")), MEAS))
say("[4] characteristic double sorts (size/BM/momentum tercile x quintile) done")

# ---- 5. tidy headline long table -------------------------------------------
rows <- list()
add <- function(question, measure, sample, freq, stat, value, t=NA, n=NA)
  rows[[length(rows)+1]] <<- data.table(question, measure, sample, freq, stat, value, t, n)
# Q1 realization (contemp return) + IVOL from JSONs
if(!is.null(J$realization_q1)){ r<-J$realization_q1$results
  for(i in seq_len(nrow(r))) add("Q1_realization", r$measure[i], "LSEG","quarterly","contemp_ret",
      r$ret_contemp$coef[i], r$ret_contemp$t[i], r$ret_contemp$n[i]) }
if(!is.null(J$volatility_q1)){ r<-J$volatility_q1$results
  for(i in seq_len(nrow(r))) if(r$measure[i] %in% MEAS) add("Q1_ivol", r$measure[i],"LSEG","quarterly","ivol_coef", r$coef[i], r$t[i]) }
if(!is.null(J$volatility_q1$tvol)){ r<-J$volatility_q1$tvol      # total vol (systematic+idio) -- Randl Q&A
  for(i in seq_len(nrow(r))) if(r$measure[i] %in% MEAS) add("Q1_tvol", r$measure[i],"LSEG","quarterly","tvol_coef", r$coef[i], r$t[i]) }
# Q2 FM (LSEG + US) with controls
fm_add <- function(j, samp){ if(is.null(j)) return(); r<-j$results
  for(i in seq_len(nrow(r))) if(r$measure[i] %in% MEAS) add("Q2_pricing_FM", r$measure[i], samp,"quarterly","lambda",
      r$with_controls$lambda_q[i], r$with_controls$nw_t[i]) }
fm_add(J$fama_macbeth_ric,"LSEG"); fm_add(J$fama_macbeth_ric_us,"US")
if(!is.null(J$panel_fe_ric)){ r<-J$panel_fe_ric$results
  for(i in seq_len(nrow(r))) if(r$measure[i] %in% MEAS) add("Q2_pricing_FE", r$measure[i],"LSEG","quarterly","coef",
      r$fe_plus_controls$coef[i], r$fe_plus_controls$t[i]) }
# Q3 L/S FF5 alpha — LSEG quarterly (recomputed), CRSP monthly, gvkey monthly
for(m in MEAS){ add("Q3_LS_FF5", m, "LSEG","quarterly","ls_ew_mean", quint_lseg[[m]]$ls_ew_mean, quint_lseg[[m]]$ls_ew_t, quint_lseg[[m]]$n) }
if(!is.null(J$crsp_monthly)){ r<-J$crsp_monthly$q3; for(i in seq_len(nrow(r))) if(r$measure[i]%in%MEAS) add("Q3_LS_FF5", r$measure[i],"CRSP","monthly","ew_alpha", r$ew_alpha[i], r$ew_t[i]) }
if(!is.null(J$crsp_gvkey)){ r<-J$crsp_gvkey$monthly$q3; for(i in seq_len(nrow(r))) if(r$measure[i]%in%MEAS) add("Q3_LS_FF5", r$measure[i],"CRSP_gvkey","monthly","ew_alpha", r$ew_alpha[i], r$ew_t[i]) }
# augmented spanning (CRSP + LSEG)
for(src in c("crsp","lseg")) if(!is.null(J$augmented_spanning[[src]])){ mm<-J$augmented_spanning[[src]]$models
  for(nm in names(mm)) add("Q3_augmented_spanning","GeoRisk", toupper(src),"monthly", nm, mm[[nm]]$alpha, mm[[nm]]$t) }
# robustness
if(!is.null(J$robustness_q3)) for(m in intersect(names(J$robustness_q3),MEAS)){ b<-J$robustness_q3[[m]]
  for(nm in names(b)) add("Q3_robustness", m,"LSEG","monthly", nm, b[[nm]][1], b[[nm]][2], b[[nm]][3]) }
# region Q5-Q1 (GeoRisk + GeoSentiment)
for(m in MEAS){ sp<-region_res[[m]]$spread; for(i in seq_len(nrow(sp))) add("Q3_region", m, sp$Region[i],"quarterly","Q5_Q1", sp$mean[i], sp$t[i]) }
headline <- rbindlist(rows, fill=TRUE)
headline[, value := round(value, 6)]; headline[, t := round(t,3)]
say("[5] tidy headline table: %d rows", nrow(headline))

# ---- 6. assemble + write ----------------------------------------------------
MASTER <- list(
  meta = list(built = format(Sys.time(),"%Y-%m-%d %H:%M:%S"), measures = MEAS, labels = as.list(MLAB),
              n_lseg = nrow(P), n_crsp = nrow(C), region_coverage_pct = round(cov_reg,1)),
  json = J,
  quintiles = list(lseg = quint_lseg, crsp = quint_crsp),
  region = region_res,
  characteristics = char_res,
  headline = headline)
saveRDS(MASTER, vS(file.path(ANA,"RESULTS_MASTER.rds")))
fwrite(headline, vS(file.path(ANA,"RESULTS_MASTER.csv")))
write_json(list(meta=MASTER$meta, headline=headline,
                quintiles=list(lseg=quint_lseg, crsp=quint_crsp), region=region_res, characteristics=char_res),
           vS(file.path(ANA,"results_master.json")), pretty=TRUE, auto_unbox=TRUE, na="null", digits=6)
say("[6] wrote RESULTS_MASTER.{rds,csv} + results_master.json")

# ---- 7. verify reconciliation with portfolio_sorts_ric ----------------------
if(!is.null(J$portfolio_sorts_ric)){ ps<-J$portfolio_sorts_ric$results
  chk <- sapply(MEAS, function(m){ a<-quint_lseg[[m]]$ls_ew_mean; b<-ps$ew_ls_mean_q[ps$measure==m]; abs(a-b) })
  say("[7] reconciliation vs portfolio_sorts_ric (max |Q5-Q1 EW diff|): %.2e %s",
      max(chk,na.rm=TRUE), if(max(chk,na.rm=TRUE)<1e-3) "OK" else "*** MISMATCH ***") }
say("DONE.")
