#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_llm_feasibility.sh  (Stage 4 — LLM feasibility & case study)
# Timed small/mid/large x flag/extract comparison on a representative subset, to
# justify KLR and document why a Clayton-2025-style full-DB LLM extraction is
# infeasible on the WU cluster (8x A30 24GB). 2x A30, gpu partition.
#   PRE-DOWNLOAD models on a LOGIN node first (HF throttle on GPU nodes):
#     bash pipeline/cluster/predownload_llm_models.sh
#   then: sbatch pipeline/cluster/slurm_llm_feasibility.sh
# ==============================================================================
#SBATCH --job-name=llm_feas
#SBATCH --partition=gpu
#SBATCH --gres=gpu:2
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=80G
#SBATCH --time=12:00:00
#SBATCH --output=out/validation/slurm_logs/llm_feas_%j.out
#SBATCH --error=out/validation/slurm_logs/llm_feas_%j.err

set -uo pipefail
mkdir -p out/validation/slurm_logs
ml purge
module load python/3.11.11-gcc-15.1.0-u2m6tuq
export HF_HOME="$HOME/hf_cache"; mkdir -p "$HF_HOME"
export HF_HUB_OFFLINE=1   # rely on the pre-downloaded cache; never download on the GPU node

# Use the 989-item stratified sample: it has both `text` and the dictionary flags that
# 04f scores precision/recall against (the snippet pool lacks the dict flag + uses snippet_text).
SUB="out/validation/llm_validation_sample.csv"
EXTRACT_LIMIT=50   # extract is ~50x slower (256 tokens) -> bound it; 04j extrapolates

# --- transformers env: small (1 GPU) + med (2 GPU), both tasks ----------------
GVENV="$HOME/llm_gpu_env"; [ -f "$GVENV/bin/activate" ] || bash pipeline/cluster/setup_llm_gpu_env.sh
source "$GVENV/bin/activate"
python -c "import torch;[print(f'  GPU{i} {torch.cuda.get_device_name(i)} {torch.cuda.get_device_properties(i).total_memory//1024**3}GB') for i in range(torch.cuda.device_count())]" || true

for M in small med; do
  echo "[$(date +'%T')] $M flag ..."   ; python pipeline/python/04e_llm_classify.py --model "$M" --task flag    --input "$SUB" || true
  echo "[$(date +'%T')] $M extract ..."; python pipeline/python/04e_llm_classify.py --model "$M" --task extract --input "$SUB" --limit "$EXTRACT_LIMIT" || true
done
echo "[$(date +'%T')] nli flag (CPU baseline) ..."; python pipeline/python/04e_llm_classify.py --model nli --task flag --input "$SUB" || true

# --- LARGE ceiling test (transformers + AWQ; vLLM does not build on this stack) --
# autoawq lets transformers load the pre-quantized AWQ weights; same GPU venv.
python -c "import awq" 2>/dev/null || pip install autoawq --quiet || echo "  WARN: autoawq install failed -> 04e2 will report does_not_fit"
echo "[$(date +'%T')] large flag (AWQ) ..."   ; python pipeline/python/04e2_llm_vllm.py --task flag    --input "$SUB" || true
echo "[$(date +'%T')] large extract (AWQ) ..."; python pipeline/python/04e2_llm_vllm.py --task extract --input "$SUB" --limit "$EXTRACT_LIMIT" || true
deactivate || true

# --- score the flag results (precision/recall vs dict) ------------------------
module load r
Rscript pipeline/R/04f_llm_precision.R || true
echo "[$(date +'%T')] feasibility job done. Pull out/validation/{llm_results_*,llm_timing_*}.{csv,json}"
