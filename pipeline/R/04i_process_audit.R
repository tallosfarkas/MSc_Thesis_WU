# ==============================================================================
# pipeline/R/04i_process_audit.R   (Stage 4 — score the RETURNED human audit, Sautner Fig.1)
#
# Run AFTER the rater returns the coded workbook. Merges the coded 'Audit' sheet with the
# private key and produces the Sautner A.2 result:
#   - true-positive rate (share CCAudit=1) by GeoExposure decile  -> should rise ~monotonically;
#   - the Figure-1 relationship: median GeoExposure percentile vs P(positive) per decile;
#   - top-decile correct-positive rate (Sautner: 310/339 = 91%);
#   - rank correlation (decile vs positive rate);
#   - optional human-vs-LLM agreement if the same calls are in the LLM sample.
#
#   Rscript pipeline/R/04i_process_audit.R [path_to_returned_workbook.xlsx]
#   (default: out/validation/snippet_audit_workbook_CODED.xlsx, else the original workbook)
# Output: out/analysis/snippet_audit_results.json + out/figures/fig_P_human_audit_{pastel,print}.png
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(openxlsx); library(jsonlite); library(ggplot2) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); VAL <- file.path(ROOT,"out","validation"); ANA <- file.path(ROOT,"out","analysis"); FIG <- file.path(ROOT,"out","figures")
say <- function(...) cat(sprintf(...),"\n")
args <- commandArgs(trailingOnly=TRUE)
cand <- c(args[1], file.path(VAL,"snippet_audit_workbook_CODED.xlsx"), file.path(VAL,"snippet_audit_workbook.xlsx"))
wbf <- cand[which(file.exists(cand))[1]]; if (is.na(wbf)) stop("no workbook found")
say("[in] reading coded workbook: %s", wbf)

A <- as.data.table(read.xlsx(wbf, sheet="Audit")); setnames(A, 1, "rater_id")
ccol <- grep("CCAudit", names(A), value=TRUE)[1]; concol <- grep("Confidence", names(A), value=TRUE)[1]
A[, CCAudit := suppressWarnings(as.integer(get(ccol)))]
A[, conf := suppressWarnings(as.integer(get(concol)))]
n_coded <- sum(!is.na(A$CCAudit))
if (n_coded < 0.5*nrow(A)) stop(sprintf("workbook looks UNCODED (%d/%d CCAudit filled). Code it first, then re-run.", n_coded, nrow(A)))
say("[in] %d/%d snippets coded | CCAudit=1: %d (%.0f%%)", n_coded, nrow(A), sum(A$CCAudit==1,na.rm=TRUE), 100*mean(A$CCAudit==1,na.rm=TRUE))

key <- fread(file.path(VAL,"snippet_audit_KEY.csv"))
D <- merge(A[,.(rater_id,CCAudit,conf)], key, by="rater_id")
D <- D[!is.na(CCAudit)]

# ---- TP rate by decile + percentile mapping --------------------------------
bydec <- D[, .(n=.N, n_pos=sum(CCAudit==1), pos_rate=mean(CCAudit==1),
               med_score=median(GeoExposure), mean_conf=mean(conf,na.rm=TRUE)), by=decile][order(decile)]
# percentile midpoint per decile (Sautner x-axis); decile 0 = zero-exposure bin
bydec[, percentile := ifelse(decile==0, 0, decile*10 - 5)]
say("\n=== Human audit: P(CCAudit=1) by GeoExposure decile ===")
print(bydec[, .(decile, n, n_pos, pos_rate=round(pos_rate,3), med_score=round(med_score,4), mean_conf=round(mean_conf,2))])

topdec <- bydec[decile==10]
rank_cor <- suppressWarnings(cor(bydec$decile, bydec$pos_rate, method="spearman"))
say("\n  top-decile correct-positive rate: %d/%d = %.0f%% (Sautner: 310/339 = 91%%)",
    topdec$n_pos, topdec$n, 100*topdec$pos_rate)
say("  Spearman(decile, positive-rate) = %.2f (positive + monotone = the audit validates the score)", rank_cor)

# ---- optional: human vs LLM on the same calls ------------------------------
hl <- NULL
llm_f <- file.path(VAL,"llm_validation_sample.csv")
if (file.exists(llm_f)) { L<-fread(llm_f); if ("Id" %in% names(L) && "dictionary_flag" %in% names(L)) {
  Lc <- unique(L[,.(Id, llm_flag=as.integer(dictionary_flag))], by="Id")
  HL <- merge(D[,.(Id,CCAudit)], Lc, by="Id")
  if (nrow(HL)>10){ hl<-list(n=nrow(HL), agreement=mean(HL$CCAudit==HL$llm_flag)); say("\n  human-vs-LLM dictionary_flag agreement on %d shared calls: %.0f%%", hl$n, 100*hl$agreement) } } }

out <- list(n_coded=n_coded, n_total=nrow(A), overall_pos_rate=mean(D$CCAudit==1),
            by_decile=bydec, top_decile=list(n_pos=topdec$n_pos, n=topdec$n, rate=topdec$pos_rate),
            spearman_decile_posrate=rank_cor, human_vs_llm=hl, source_workbook=basename(wbf))
write_json(out, file.path(ANA,"snippet_audit_results.json"), pretty=TRUE, auto_unbox=TRUE, na="null", digits=4)
say("\n[done] wrote out/analysis/snippet_audit_results.json")

# ---- Figure 1: percentile vs P(positive) -----------------------------------
PAL <- list(pastel=c(pt="#234c78", ln="#5B9BD5"), print=c(pt="black", ln="grey50"))
for (pl in names(PAL)){ p<-PAL[[pl]]
  g <- ggplot(bydec, aes(percentile, 100*pos_rate)) +
    geom_smooth(method="lm", se=FALSE, colour=p["ln"], linewidth=0.7, linetype="dashed") +
    geom_point(colour=p["pt"], size=2.6) + geom_line(colour=p["pt"], linewidth=0.5) +
    scale_x_continuous(breaks=c(0,seq(5,95,10)), labels=c("0",paste0("D",1:10))) +
    labs(title="Human audit validates GeoExposure (Sautner Figure 1)",
         subtitle="Share of snippets a human rates as clear geoeconomic exposure, by GeoExposure decile",
         x="GeoExposure decile (0 = zero-exposure bin)", y="P(rater codes CCAudit = 1), %") +
    theme_minimal(base_size=13) + theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold",size=13), plot.subtitle=element_text(size=10,colour="grey30"))
  ggsave(file.path(FIG, sprintf("fig_P_human_audit_%s.png",pl)), g, width=8.5, height=5, dpi=300, bg="white") }
file.copy(list.files(FIG,pattern="^fig_P_human_audit_(pastel|print)\\.png$",full.names=TRUE), file.path(ROOT,"msc_thesis_obsidian","assets","figures"), overwrite=TRUE)
say("  wrote fig_P_human_audit_{pastel,print}.png. DONE.")
