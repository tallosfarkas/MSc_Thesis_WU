# ==============================================================================
# pipeline/R/04f_llm_precision.R   (Stage 4 — snippet-audit scoring, multi-model)
#
# Scores every available LLM judge (out/validation/llm_results_<model>.csv) against
# BOTH the primary (unpruned) and pruned dictionary flags, split by Part A (call
# excerpts) / Part B (sentences). Folds in the legacy 02_analyze_llm_results.R
# extras: inter-model agreement + qualitative false-positive / false-negative dumps.
# The headline Sautner number is PRECISION (when the dict flags, does the LLM agree)
# from the strongest model (Qwen-72B > 7B > NLI); his bar is >85%.
#
#   Rscript pipeline/R/04f_llm_precision.R            (scores all models found)
# Outputs: out/validation/llm_precision.json + llm_validation_table.tex
# ==============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root()
VAL  <- file.path(ROOT, "out", "validation")

MODEL_ORDER <- c(large = "Qwen2.5-72B-Instruct (4-bit)",
                 med   = "Qwen2.5-32B-Instruct (4-bit)",
                 small = "Qwen2.5-7B-Instruct (4-bit)",
                 nli   = "BART-large-mnli (NLI, CPU)")
files <- file.path(VAL, sprintf("llm_results_%s.csv", names(MODEL_ORDER)))
present <- names(MODEL_ORDER)[file.exists(files)]
if (!length(present)) stop("no llm_results_*.csv in ", VAL)

# base sample (any results file carries the sample columns)
base <- fread(file.path(VAL, sprintf("llm_results_%s.csv", present[1])))
base[, dy_primary := toupper(as.character(dictionary_flag)) %in% c("TRUE","1")]
base[, dy_pruned  := toupper(as.character(dictionary_flag_pruned)) %in% c("TRUE","1")]
keep <- c("item_id","validation_type","stratum","text","bigrams_found","dy_primary","dy_pruned")
d <- base[, ..keep]
for (m in present) {
  r <- fread(file.path(VAL, sprintf("llm_results_%s.csv", m)))[, .(item_id, lab = llm_label)]
  setnames(r, "lab", paste0("y_", m)); d <- merge(d, r, by = "item_id", all.x = TRUE)
}
cat(sprintf("LLM snippet audit | models: %s | %d items (A=%d, B=%d)\n",
            paste(present, collapse=", "), nrow(d),
            sum(d$validation_type=="A"), sum(d$validation_type=="B")))

score <- function(dy, lab) {
  ok <- lab %in% c("YES","NO"); dy <- dy[ok]; ly <- lab[ok] == "YES"
  tp<-sum(dy&ly); fp<-sum(!dy&ly); fn<-sum(dy&!ly)
  # Dictionary is the classifier, LLM is the reference. DICTIONARY PRECISION =
  # of dict-flagged items, fraction the LLM confirms geoeconomic = tp/(tp+fn).
  # DICTIONARY RECALL = of LLM-geoeconomic items, fraction the dict catches = tp/(tp+fp).
  list(precision = round(tp/max(tp+fn,1),3), recall = round(tp/max(tp+fp,1),3),
       agreement = round(mean(dy==ly),3), n = sum(ok), n_flagged = sum(dy))
}

# ---- main precision table: model x subset x dict ----------------------------
rows <- rbindlist(lapply(present, function(m){
  lab <- d[[paste0("y_", m)]]
  rbindlist(lapply(c("All","A","B"), function(sub){
    idx <- if (sub=="All") rep(TRUE, nrow(d)) else d$validation_type == sub
    sp <- score(d$dy_primary[idx], lab[idx]); sr <- score(d$dy_pruned[idx], lab[idx])
    data.table(model = m, subset = sub,
               prec_primary = sp$precision, recall_primary = sp$recall, agree_primary = sp$agreement,
               prec_pruned = sr$precision, n = sp$n)
  }))
}))
print(rows)

# ---- by-stratum LLM-YES rate per model --------------------------------------
strat <- d[, c(.(n = .N),
               lapply(present, function(m) round(mean(get(paste0("y_",m))=="YES", na.rm=TRUE),3))),
            by = stratum]
setnames(strat, c("stratum","n", paste0("yes_", present)))
cat("\nLLM-YES rate by stratum:\n"); print(strat[order(stratum)])

# ---- inter-model agreement --------------------------------------------------
agr <- list()
if (length(present) >= 2) {
  cmb <- combn(present, 2, simplify = FALSE)
  for (p in cmb) {
    a <- d[[paste0("y_",p[1])]]; b <- d[[paste0("y_",p[2])]]
    ok <- a %in% c("YES","NO") & b %in% c("YES","NO")
    agr[[paste(p, collapse="_vs_")]] <- round(mean(a[ok]==b[ok]),3)
  }
  cat("\nInter-model agreement:\n"); for (k in names(agr)) cat(sprintf("  %s: %.1f%%\n", k, 100*agr[[k]]))
}

# ---- qualitative FP / FN from the strongest available model -----------------
ref <- present[1]; lab <- d[[paste0("y_", ref)]]
fp <- d[dy_primary == FALSE & lab == "YES"][1:min(8,.N)]
fn <- d[dy_primary == TRUE  & lab == "NO" ][1:min(8,.N)]
cat(sprintf("\n--- %s false positives (dict=NO, LLM=YES) ---\n", ref))
if (nrow(fp)) for (i in 1:nrow(fp)) cat(sprintf("  [%s] %s\n", fp$stratum[i], substr(fp$text[i],1,110)))
cat(sprintf("--- %s false negatives (dict=YES, LLM=NO) ---\n", ref))
if (nrow(fn)) for (i in 1:nrow(fn)) cat(sprintf("  [%s | %s] %s\n", fn$stratum[i], substr(fn$bigrams_found[i],1,35), substr(fn$text[i],1,90)))

# ---- headline + JSON --------------------------------------------------------
hl <- rows[model == ref & subset == "All"]
cat(sprintf("\n  HEADLINE (%s, all items): primary precision = %.1f%% | pruned = %.1f%%  (Sautner bar >85%%)\n",
            ref, 100*hl$prec_primary, 100*hl$prec_pruned))
write_json(list(models = present, n = nrow(d),
                table = rows, by_stratum = strat, inter_model_agreement = agr,
                headline_model = ref,
                headline_primary_precision = hl$prec_primary,
                headline_pruned_precision = hl$prec_pruned,
                pass_bar_0.85 = isTRUE(hl$prec_pruned > 0.85)),
           file.path(VAL, "llm_precision.json"), pretty = TRUE, auto_unbox = TRUE)

# ---- LaTeX table (All-items, primary) ---------------------------------------
tex <- rows[subset == "All"]
tex[, Model := MODEL_ORDER[model]]
lines <- c("\\begin{table}[!h]\\centering",
  "\\caption{LLM validation of the geoeconomic dictionary (new locked dicts, A+B sample). Precision/recall/agreement treat the dictionary as the reference; precision = fraction of dict-flagged items the LLM confirms as geoeconomic.}",
  "\\begin{tabular}{lrrrr}\\toprule",
  "Model & N & Prec. primary (\\%) & Prec. pruned (\\%) & Agree. (\\%)\\\\ \\midrule",
  tex[, sprintf("%s & %d & %.1f & %.1f & %.1f\\\\", Model, n, 100*prec_primary, 100*prec_pruned, 100*agree_primary)],
  "\\bottomrule\\end{tabular}\\end{table}")
writeLines(lines, file.path(VAL, "llm_validation_table.tex"))
cat(sprintf("\n  wrote llm_precision.json + llm_validation_table.tex\n"))
