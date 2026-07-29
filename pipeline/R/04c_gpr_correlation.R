# ==============================================================================
# pipeline/R/04c_gpr_correlation.R  (Stage 4 — external validation)
#
# Sautner JF validation analogue: their firm-level climate measure tracks an
# external climate-attention index; ours should track the Caldara-Iacoviello
# (2022, AER) Geopolitical Risk index. We aggregate the firm-quarter GeoExposure
# panel to an equal-weighted quarterly cross-sectional mean and correlate it with
# the GPR (and its threat/act sub-indices) in LEVELS and in 4-quarter CHANGES
# (changes guard against a spurious common trend). Sautner's bar is r > 0.4.
#
#   Rscript pipeline/R/04c_gpr_correlation.R
# Outputs: out/exposure/gpr_quarterly.csv, out/exposure/gpr_correlation.json
# ==============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(readxl) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root()
exp_dirname <- function() { .e <- Sys.getenv("GEO_EXPO"); if (nzchar(Sys.getenv("GEOV2"))||identical(.e,"v2")) "exposure_v2" else if (nzchar(.e)) paste0("exposure_",.e) else "exposure" }  # self-contained
EXPO <- file.path(ROOT, "out", exp_dirname())

# ---- 1. GPR monthly -> quarterly --------------------------------------------
# guess_max=Inf: GPR begins ~1985, but the file starts in 1900 — the default
# 1000-row type guess sees only leading NAs and wrongly types GPR as logical.
gpr <- as.data.table(read_excel(file.path(ROOT, "data/inputs/gpr_monthly.xls"),
                                guess_max = .Machine$integer.max))
gpr[, month := as.Date(month)]
gpr[, `:=`(year = as.integer(format(month, "%Y")),
           quarter = as.integer(substr(quarters(month), 2, 2)))]
gprq <- gpr[year >= 2002 & year <= 2025,
            .(GPR = mean(GPR, na.rm = TRUE), GPRT = mean(GPRT, na.rm = TRUE),
              GPRA = mean(GPRA, na.rm = TRUE)), by = .(year, quarter)]

# ---- 2. exposure panel -> equal-weighted quarterly mean ---------------------
# deduped RIC panel (Stage 3.5) — NOT the stale pre-dedup exposure_firmquarter.rds
ex <- as.data.table(readRDS(file.path(EXPO, "exposure_firmquarter_ric.rds")))
measures <- intersect(c("GeoExposure","GeoRisk","GeoSentiment","GeoExposureTFIDF","GeoExposure_pr"), names(ex))
exq <- ex[, c(lapply(.SD, mean, na.rm = TRUE), .(n_firmq = .N)),
          by = .(year, quarter), .SDcols = measures]

# ---- 3. merge, order, build 4-quarter changes -------------------------------
m <- merge(exq, gprq, by = c("year","quarter"))[order(year, quarter)]
m[, t := year * 4 + (quarter - 1)]
stopifnot(all(diff(m$t) == 1L))   # 4Q changes assume a contiguous quarterly series (audit guard)
gpr_vars <- c("GPR","GPRT","GPRA")
for (v in c(measures, gpr_vars)) m[, paste0("d_", v) := get(v) - shift(get(v), 4)]

fwrite(m, file.path(EXPO, "gpr_quarterly.csv"))

# ---- 4. correlations (levels + 4Q changes), Pearson + Spearman --------------
cor1 <- function(x, y, meth) {
  ok <- is.finite(x) & is.finite(y); if (sum(ok) < 8) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = meth))
}
res <- rbindlist(lapply(measures, function(mz) rbindlist(lapply(gpr_vars, function(gz) {
  data.table(measure = mz, gpr = gz,
    pearson_level   = round(cor1(m[[mz]],            m[[gz]],            "pearson"), 3),
    spearman_level  = round(cor1(m[[mz]],            m[[gz]],            "spearman"), 3),
    pearson_change  = round(cor1(m[[paste0("d_",mz)]], m[[paste0("d_",gz)]], "pearson"), 3),
    spearman_change = round(cor1(m[[paste0("d_",mz)]], m[[paste0("d_",gz)]], "spearman"), 3))
}))))

cat(sprintf("GeoExposure vs Caldara-Iacoviello GPR | %d quarters (%d-%d)\n",
            nrow(m), min(m$year), max(m$year)))
print(res)
head_r <- res[measure == "GeoExposure" & gpr == "GPR"]
cat(sprintf("\n  Headline GeoExposure vs GPR: level r=%.3f (Spearman %.3f) | 4Q-change r=%.3f\n",
            head_r$pearson_level, head_r$spearman_level, head_r$pearson_change))
cat(sprintf("  Sautner bar r>0.4 (level): %s\n", isTRUE(head_r$pearson_level > 0.4)))

write_json(list(n_quarters = nrow(m), span = c(min(m$year), max(m$year)),
                results = res,
                headline = list(measure = "GeoExposure", index = "GPR",
                                pearson_level = head_r$pearson_level,
                                spearman_level = head_r$spearman_level,
                                pearson_change = head_r$pearson_change,
                                pass_bar_0.4 = isTRUE(head_r$pearson_level > 0.4))),
           file.path(EXPO, "gpr_correlation.json"),
           pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("\n  wrote %s/gpr_quarterly.csv + gpr_correlation.json\n", EXPO))
