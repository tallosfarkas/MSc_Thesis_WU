#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_corpus.sh
#
# Stage 1 — build per-year corpora. One SLURM array task per year, 2002-2025.
# Array indices 1..24 map to years 2002..2025 (1=2002, 24=2025).
#
# Resource sizing (validated against local test on 2002):
#   - Largest year (2024, 825 MB on disk) peaks at ~12 GB RAM after load+strip.
#   - Single-threaded R code; no point requesting >2 CPUs.
#   - Walltime: smallest years ~5 min, largest ~25 min. 1h is safe.
#
# Submit from project root on the cluster:
#   cd ~/thesis_clean
#   sbatch pipeline/cluster/slurm_corpus.sh
#
# Or run a single year for testing:
#   sbatch --array=1 pipeline/cluster/slurm_corpus.sh        # just 2002
#   sbatch --array=11 pipeline/cluster/slurm_corpus.sh       # just 2012
#
# Monitor:
#   squeue -u $USER
#   tail -f out/corpus/slurm_logs/corpus_<jobid>_<task>.out
# ==============================================================================
#SBATCH --job-name=corpus
#SBATCH --partition=short
#SBATCH --array=1-24
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=01:00:00
#SBATCH --output=out/corpus/slurm_logs/corpus_%A_%a.out
#SBATCH --error=out/corpus/slurm_logs/corpus_%A_%a.err

set -euo pipefail

# --- environment ---
module load r

# --- ensure log dir exists (SLURM won't create it) ---
mkdir -p out/corpus/slurm_logs

# --- map array index 1..24 -> year 2002..2025 ---
YEAR=$((2001 + SLURM_ARRAY_TASK_ID))

echo "============================================================"
echo "[$(date +'%F %T')] STAGE 1 — Build corpus for year ${YEAR}"
echo "  Host       : $(hostname)"
echo "  Job ID     : ${SLURM_JOB_ID}"
echo "  Array task : ${SLURM_ARRAY_TASK_ID} -> year ${YEAR}"
echo "  R version  : $(Rscript --version 2>&1)"
echo "============================================================"

Rscript pipeline/R/01_build_corpus.R "${YEAR}"

echo "============================================================"
echo "[$(date +'%F %T')] DONE — year ${YEAR}"
echo "============================================================"
