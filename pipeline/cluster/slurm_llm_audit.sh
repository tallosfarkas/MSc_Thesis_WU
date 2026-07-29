#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_llm_audit.sh  (Stage 4 — snippet audit, NLI on CPU)
# Samples snippets with the sklearn-only KLR venv, then classifies with a
# DEDICATED transformers venv (torch CPU + transformers) so the heavy ML deps
# never touch the locked KLR venv. NLI (bart-large-mnli) runs fine on CPU.
#   sbatch pipeline/cluster/slurm_llm_audit.sh
# For the Qwen GPU variant instead: submit 04e with --model small to a gpu node.
# ==============================================================================
#SBATCH --job-name=llm_audit
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=4G
#SBATCH --time=06:00:00
#SBATCH --output=out/validation/slurm_logs/llm_audit_%j.out
#SBATCH --error=out/validation/slurm_logs/llm_audit_%j.err

set -euo pipefail
mkdir -p out/validation/slurm_logs
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK:-16}"

echo "[$(date +'%F %T')] 1/3 sampling snippets (KLR venv) | $(hostname)"
pipeline/python/.venv/bin/python pipeline/python/04d_sample_for_llm.py

# dedicated NLI venv (torch CPU + transformers), built once and reused.
# `uv venv --seed` ships pip into the venv (a plain `uv venv` does not), so we can
# use the canonical `pip install torch --index-url .../whl/cpu` (CPU wheel + deps).
NLIENV="$HOME/llm_audit_venv"
if ! "$NLIENV/bin/python" -c "import transformers, torch" 2>/dev/null; then
  echo "[$(date +'%F %T')] building NLI venv at $NLIENV"
  rm -rf "$NLIENV"
  uv venv "$NLIENV" --python 3.12 --seed
  "$NLIENV/bin/pip" install --quiet --upgrade pip
  "$NLIENV/bin/pip" install --quiet torch --index-url https://download.pytorch.org/whl/cpu
  "$NLIENV/bin/pip" install --quiet "transformers>=4.44" pandas
fi

export HF_HOME="$HOME/hf_cache"; mkdir -p "$HF_HOME"
echo "[$(date +'%F %T')] 2/3 classifying (NLI, CPU)"
"$NLIENV/bin/python" pipeline/python/04e_llm_classify.py --model nli

echo "[$(date +'%F %T')] 3/3 scoring precision"
module load r
Rscript pipeline/R/04f_llm_precision.R nli
echo "[$(date +'%F %T')] done"
