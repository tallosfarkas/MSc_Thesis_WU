#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_llm_sample.sh  (Stage 4 — build the A+B audit sample)
# Runs 04d with the sklearn-only KLR venv (needs pyarrow/sklearn/yaml). Full-year
# reads to group sentences into call-level Part-A excerpts, so it's a bit heavier
# than a pure subsample — a short CPU job. Chains into slurm_llm_qwen.sh (afterok).
#   sbatch pipeline/cluster/slurm_llm_sample.sh
# ==============================================================================
#SBATCH --job-name=llm_sample
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=6G
#SBATCH --time=02:00:00
#SBATCH --output=out/validation/slurm_logs/llm_sample_%j.out
#SBATCH --error=out/validation/slurm_logs/llm_sample_%j.err

set -euo pipefail
mkdir -p out/validation/slurm_logs
echo "[$(date +'%F %T')] sampling A+B snippets (KLR venv) | $(hostname)"
pipeline/python/.venv/bin/python pipeline/python/04d_sample_for_llm.py
echo "[$(date +'%F %T')] done"
