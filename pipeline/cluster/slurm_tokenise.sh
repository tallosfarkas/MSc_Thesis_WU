#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_tokenise.sh
#
# Stage 2a — tokenise per year, array job. 24 tasks parallel.
# Each task tokenises one year's sentences -> dfm_YYYY.rds + meta_YYYY.rds.
#
# Sizing:
#   - Largest year (2024, 11M sentences) peaks at ~16 GB RAM.
#   - Request 8 CPUs + 32 GB each task. Plenty of headroom.
#   - 24 tasks * 8 CPUs = 192 CPUs total = fits comfortably in 6-node short budget.
#   - Walltime: 1h is generous (expect 1-10 min per year).
#
# Submit from project root on cluster:
#   sbatch pipeline/cluster/slurm_tokenise.sh
# ==============================================================================
#SBATCH --job-name=tokenise
#SBATCH --partition=short
#SBATCH --array=1-24
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=03:00:00
#SBATCH --output=out/dict/slurm_logs/tokenise_%A_%a.out
#SBATCH --error=out/dict/slurm_logs/tokenise_%A_%a.err

set -euo pipefail
module load r

export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export RCPP_PARALLEL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"

mkdir -p out/dict/slurm_logs

YEAR=$((2001 + SLURM_ARRAY_TASK_ID))

echo "============================================================"
echo "[$(date +'%F %T')] STAGE 2a — Tokenise year ${YEAR}"
echo "  Host  : $(hostname)"
echo "  Job   : ${SLURM_JOB_ID}  Task: ${SLURM_ARRAY_TASK_ID} -> year ${YEAR}"
echo "  CPUs  : ${SLURM_CPUS_PER_TASK}"
echo "============================================================"

Rscript pipeline/R/02a_tokenise.R "${YEAR}"

echo "[$(date +'%F %T')] DONE"
