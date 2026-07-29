#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_vintage_discovery.sh   (Phase A of the expanding-window
# OOS-on-discovery analysis)
#
# Re-runs the KLR keyword discovery at each ANNUAL vintage cutoff 2012..2024,
# using ONLY the per-year DFMs with year <= cutoff (a real-time, point-in-time
# dictionary). Answers Randl's look-ahead question: would a trader who only had
# pre-t text have discovered a materially different dictionary?
#
# Array index = cutoff year. %4 caps concurrency (each task is 64 CPU x 4 GB =
# 256 GB; a node has 500 GB, so ~1-2 per node). Each task ~4-8h on `short`.
# Output: out/dict/vintage/dictionary_vintage_<YYYY>.csv (+ keyness + audit).
# ==============================================================================
#SBATCH --job-name=klr_vintage
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=4G
#SBATCH --time=23:59:00
#SBATCH --array=2012-2024%13
# %13 = no self-imposed throttle; `short` has no per-job CPU cap, so the only
# limiter is node availability (each task = 64 CPU x 256 GB). SLURM queues the
# rest as (Resources) and starts them as nodes free. Lower %N only to be polite
# on a busy shared cluster, or drop --cpus-per-task to fit more tasks per node.
#SBATCH --output=out/dict/slurm_logs/klr_vintage_%a_%j.out
#SBATCH --error=out/dict/slurm_logs/klr_vintage_%a_%j.err

set -euo pipefail
module load r

export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export MKL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export RCPP_PARALLEL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"

mkdir -p out/dict/slurm_logs out/dict/vintage

YEAR="${SLURM_ARRAY_TASK_ID}"
echo "============================================================"
echo "[$(date +'%F %T')] PHASE A — vintage KLR discovery, cutoff=${YEAR}"
echo "  Host    : $(hostname)   Job: ${SLURM_JOB_ID}  Task: ${SLURM_ARRAY_TASK_ID}"
echo "  CPUs    : ${SLURM_CPUS_PER_TASK}"
echo "============================================================"

GEO_CUTOFF="${YEAR}" Rscript pipeline/R/02b_klr_discovery.R

echo "[$(date +'%F %T')] DONE vintage ${YEAR}"
