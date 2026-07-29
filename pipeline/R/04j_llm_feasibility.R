# ==============================================================================
# pipeline/R/04j_llm_feasibility.R   (Stage 4 — LLM feasibility & case study aggregation)
#
# Aggregates the timed small/mid/large x flag/extract runs into ONE feasibility table +
# a full-corpus cost EXTRAPOLATION, to justify the KLR choice and document why a
# Clayton-2025-style full-DB LLM extraction is infeasible on the WU cluster.
# Reads:  out/validation/llm_timing_{small,med,large}_{flag,extract}.json (throughput+VRAM)
#         out/validation/llm_results_*.csv (+ snippet/sample dict flags) for accuracy
# Writes: out/analysis/llm_feasibility.json + out/figures/fig_Q_llm_feasibility_{pastel,print}.png
#
#   Rscript pipeline/R/04j_llm_feasibility.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(ggplot2) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); VAL <- file.path(ROOT,"out","validation"); ANA <- file.path(ROOT,"out","analysis"); FIG <- file.path(ROOT,"out","figures")
dir.create(FIG,showWarnings=FALSE,recursive=TRUE); say <- function(...) cat(sprintf(...),"\n")

# ---- corpus + cluster constants (the scale we'd have to process) ------------
N_CALLS <- 456996L; N_SENT <- 163e6; N_GPU <- 8L
KLR_CPU_HOURS <- 24                      # one ~24h CPU job for the full 163M-sentence dictionary
PARAMS_B <- c(small=7, med=32)           # billions; large filled from its timing json

# ---- read timing jsons ------------------------------------------------------
read_t <- function(model, task){ p<-file.path(VAL,sprintf("llm_timing_%s_%s.json",model,task))
  if(!file.exists(p)) return(NULL); as.list(fromJSON(p)) }
tiers <- c("small","med","large"); tasks <- c("flag","extract")
TIM <- list(); for(m in tiers) for(tk in tasks){ r<-read_t(m,tk); if(!is.null(r)) TIM[[paste(m,tk,sep="_")]]<-r }
say("[timing] found %d timing files: %s", length(TIM), paste(names(TIM),collapse=", "))

# ---- agreement vs dict flag per tier (flag task) ----------------------------
# The LLM is the stricter judge: it flags far fewer snippets than the dictionary.
# Treating the LLM as ground truth, the DICTIONARY is a high-recall / low-precision
# screen. We report both, with unambiguous names, plus the raw YES counts.
acc_of <- function(model){
  f <- file.path(VAL, sprintf("llm_results_%s.csv", model))
  na <- list(dict_prec_vs_llm=NA, dict_recall_vs_llm=NA, llm_yes_n=NA, dict_yes_n=NA, n=NA, yes_rate=NA)
  if(!file.exists(f)) return(na)
  d <- fread(f)
  if(!all(c("llm_label","dictionary_flag") %in% names(d))) return(na)
  d <- d[llm_label %in% c("YES","NO")]
  dy <- toupper(as.character(d$dictionary_flag)) %in% c("TRUE","1")
  ly <- d$llm_label=="YES"
  tp <- sum(dy & ly); fp <- sum(!dy & ly); fn <- sum(dy & !ly)
  list(dict_prec_vs_llm   = tp/max(tp+fn,1),   # of dict-YES, frac the LLM confirms (dictionary precision vs LLM)
       dict_recall_vs_llm = tp/max(tp+fp,1),   # of LLM-YES, frac the dict caught (dictionary recall vs LLM)
       llm_yes_n = sum(ly), dict_yes_n = sum(dy), n=nrow(d), yes_rate=mean(ly))
}
ACC <- setNames(lapply(tiers, acc_of), tiers)

# inter-model agreement (does the verdict move with model size?) — from 04f if present
AGREE <- tryCatch({ pj <- file.path(VAL,"llm_precision.json")
  if(file.exists(pj)) fromJSON(pj)$inter_model_agreement else NULL }, error=function(e) NULL)

# ---- honest read of model-size sensitivity ---------------------------------
# Overall label agreement is INFLATED by the shared-NO majority (the LLMs flag few items).
# Report the all-NO floor + agreement on the informative subset (any model YES) + how many of
# the 72B's positives the smaller models also catch. This is the number an examiner will press.
SENS <- tryCatch({
  rd <- function(m){ f<-file.path(VAL,sprintf("llm_results_%s.csv",m)); if(!file.exists(f)) return(NULL)
    x<-fread(f)[llm_label %in% c("YES","NO"), .(item_id, lab=llm_label)]; setnames(x,"lab",m); x }
  s<-rd("small"); md<-rd("med"); l<-rd("large")
  if(is.null(s)||is.null(l)) NULL else {
    d <- Reduce(function(a,b) merge(a,b,by="item_id"), Filter(Negate(is.null), list(s,md,l)))
    n <- nrow(d); ly <- d[large=="YES"]
    list(n=n,
         yes_n=list(small=sum(d$small=="YES"), med=if(!is.null(md)) sum(d$med=="YES") else NA, large=sum(d$large=="YES")),
         allNO_floor=list(small=mean(d$small=="NO"), large=mean(d$large=="NO")),  # agreement if all-NO
         union_yes_n=sum(d$small=="YES" | d$large=="YES" | (if(!is.null(md)) d$med=="YES" else FALSE)),
         agree_on_union_yes_large_vs_small=mean(d[(small=="YES"|large=="YES")]$small==d[(small=="YES"|large=="YES")]$large),
         of_large_yes_small_also=if(nrow(ly)) mean(ly$small=="YES") else NA,
         of_large_yes_med_also  =if(!is.null(md) && nrow(ly)) mean(ly$med=="YES") else NA)
  }
}, error=function(e) NULL)

# ---- build the tier table ---------------------------------------------------
row <- function(m){
  lf <- TIM[[paste0(m,"_flag")]]; le <- TIM[[paste0(m,"_extract")]]
  largemod <- if(m=="large" && !is.null(lf)) lf$model else NA
  pB <- if(m=="large"){ if(!is.null(lf) && grepl("72B|70b|70B",paste(largemod))) 72 else if(!is.null(lf) && grepl("Mixtral|8x7",paste(largemod))) 47 else NA } else PARAMS_B[[m]]
  fits <- if(m=="large"){ if(!is.null(lf)) (is.null(lf$status)||lf$status=="ok") else NA } else TRUE
  data.table(tier=m, model=if(m=="large") largemod else c(small="Qwen2.5-7B",med="Qwen2.5-32B")[[m]],
             params_B=pB, fits=fits,
             flag_s_item = if(!is.null(lf) && !is.null(lf$sec_per_item)) as.numeric(lf$sec_per_item) else NA,
             extract_s_item = if(!is.null(le) && !is.null(le$sec_per_item)) as.numeric(le$sec_per_item) else NA,
             peak_vram_gb = if(!is.null(lf) && !is.null(lf$peak_vram_gb)) as.numeric(lf$peak_vram_gb) else NA,
             n_gpu = if(!is.null(lf) && !is.null(lf$n_gpu)) as.integer(lf$n_gpu) else NA,
             llm_yes_n = ACC[[m]]$llm_yes_n, dict_yes_n = ACC[[m]]$dict_yes_n,
             dict_prec_vs_llm = ACC[[m]]$dict_prec_vs_llm,
             dict_recall_vs_llm = ACC[[m]]$dict_recall_vs_llm, yes_rate = ACC[[m]]$yes_rate)
}
T <- rbindlist(lapply(tiers,row), fill=TRUE)
# parallel instances on 8 A30 = floor(8 / gpus_per_model); small=1gpu->8, med/large=2gpu->4
T[, gpus_per := fifelse(is.na(n_gpu)|n_gpu<1, fifelse(tier=="small",1L,2L), n_gpu)]
T[, instances := pmax(1L, as.integer(N_GPU %/% gpus_per))]
# full-DB cost: structured EXTRACT over all calls (the Clayton task), and flag over all calls
T[, extract_gpu_h := extract_s_item * N_CALLS / 3600]
T[, extract_walldays := extract_gpu_h / instances / 24]
T[, flag_gpu_h := flag_s_item * N_CALLS / 3600]
T[, flag_walldays := flag_gpu_h / instances / 24]

# ---- defense scenarios under the REAL cluster limit (max 3 concurrent GPU jobs) ----
# Two faithful questions: (Q2) a Sautner-comparable firm measure needs a per-SENTENCE flag
# (the score is a frequency); (Q1) a full Clayton replication feeds the WHOLE call into the
# structured extract (~CALL_MULT x the input tokens of a short validation snippet).
INST_REAL  <- 3L                                   # user's limit: 3 concurrent gpu jobs
SENT_PER_CALL <- N_SENT / N_CALLS                  # ~357
TOK_SNIPPET <- 250; TOK_CALL <- SENT_PER_CALL * 22 # ~rough token scale
CALL_MULT  <- TOK_CALL / TOK_SNIPPET               # ~31x
days_real  <- function(gpu_h) gpu_h / INST_REAL / 24
T[, sautner_flag_sentence_days := days_real(flag_s_item * N_SENT / 3600)]      # Q2 faithful
T[, sautner_flag_call_days     := days_real(flag_s_item * N_CALLS / 3600)]     # Q2 NOT-faithful floor
T[, clayton_extract_fullcall_days := days_real(extract_s_item * N_CALLS / 3600 * CALL_MULT)] # Q1 faithful
T[, clayton_extract_floor_days    := days_real(extract_s_item * N_CALLS / 3600)]             # Q1 floor
scen <- list(inst_real=INST_REAL, sent_per_call=round(SENT_PER_CALL,1), call_mult=round(CALL_MULT,1),
             klr_cpu_days=round(KLR_CPU_HOURS/24,2),
             tiers=T[, .(tier, model, params_B, fits, peak_vram_gb, flag_s_item, extract_s_item,
                         sautner_flag_sentence_days=round(sautner_flag_sentence_days,1),
                         sautner_flag_call_days=round(sautner_flag_call_days,2),
                         clayton_extract_fullcall_days=round(clayton_extract_fullcall_days,1),
                         clayton_extract_floor_days=round(clayton_extract_floor_days,1))])

say("\n=== LLM feasibility — timed tiers + full-corpus extrapolation (%s calls, 8x A30) ===", format(N_CALLS,big.mark=","))
print(T[, .(tier, model, params_B, fits, peak_vram_gb, flag_s_item, extract_s_item,
            llm_yes_n, dict_yes_n, dict_prec=round(dict_prec_vs_llm,3),
            extract_gpu_h=round(extract_gpu_h), extract_walldays=round(extract_walldays,1))])
if(!is.null(AGREE)) say("  inter-model agreement: %s",
   paste(sprintf("%s=%.1f%%", names(AGREE), 100*unlist(AGREE)), collapse="  "))
say("  (vs KLR: ONE ~%dh CPU job for the full 163M-sentence dictionary)", KLR_CPU_HOURS)

out <- list(meta=list(n_calls=N_CALLS, n_sentences=N_SENT, n_gpu=N_GPU, klr_cpu_hours=KLR_CPU_HOURS,
                      gpu="NVIDIA A30 24GB x8 (4 nodes)", built=format(Sys.time(),"%Y-%m-%d %H:%M:%S")),
            tiers=T, timing=TIM, inter_model_agreement=AGREE, defense_scenarios=scen,
            size_sensitivity=SENS)
write_json(out, file.path(ANA,"llm_feasibility.json"), pretty=TRUE, auto_unbox=TRUE, na="null", digits=4)
say("\n[done] wrote out/analysis/llm_feasibility.json")

# ---- figure: full-DB extract cost (wall-days) by tier, log scale + KLR line --
pd <- T[is.finite(extract_walldays)]
if(nrow(pd)>0){ pd[, lab := sprintf("%s\n(%sB)", model, params_B)]
  PAL <- list(pastel=c(bar="#2E5A88", klr="#3d9c86"), print=c(bar="grey25", klr="grey55"))
  for(pl in names(PAL)){ p<-PAL[[pl]]
    g <- ggplot(pd, aes(reorder(lab, params_B), extract_walldays)) +
      geom_col(width=0.7, fill=p["bar"]) +
      geom_hline(yintercept=KLR_CPU_HOURS/24, colour=p["klr"], linetype="dashed", linewidth=0.9) +
      annotate("text", x=0.7, y=KLR_CPU_HOURS/24, label="KLR: 1 CPU day", vjust=-0.6, hjust=0, size=3.2, colour=p["klr"]) +
      geom_text(aes(label=sprintf("%.0f GPU-days", extract_walldays)), vjust=-0.4, size=3.4, colour="grey20") +
      scale_y_log10() +
      labs(title="Full-corpus LLM extraction is infeasible; KLR is one CPU day",
           subtitle=sprintf("Wall-clock GPU-days to run Clayton-style structured extraction over %s calls on 8x A30 (log scale)", format(N_CALLS,big.mark=",")),
           x=NULL, y="GPU-days (log)") +
      theme_minimal(base_size=13) + theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold",size=13), plot.subtitle=element_text(size=9.5,colour="grey30"))
    ggsave(file.path(FIG, sprintf("fig_Q_llm_feasibility_%s.png",pl)), g, width=8.5, height=5, dpi=300, bg="white") }
  file.copy(list.files(FIG,pattern="^fig_Q_llm_feasibility_(pastel|print)\\.png$",full.names=TRUE), file.path(ROOT,"msc_thesis_obsidian","assets","figures"), overwrite=TRUE)
  say("  wrote fig_Q_llm_feasibility_{pastel,print}.png")
} else say("  [skip figure] no extract timing yet")
