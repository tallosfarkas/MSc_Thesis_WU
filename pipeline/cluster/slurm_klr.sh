#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_klr.sh
#
# Stage 2 — Sautner-faithful KLR keyword discovery. Single big job.
# Reads all out/corpus/sentences_*.rds from Stage 1.
#
# Resource sizing (refactored 2026-05-31 after old version got stuck at
# 1 thread for 19h):
#   - 64 cores, 4 GB/CPU = 256 GB total memory.
#   - Tokenisation is now chunked across 64 cores via parallel::mclapply.
#   - On medium partition (14-day walltime) for safety — the new code should
#     finish in 4-8h but we don't trust estimates after the last incident.
#   - Thread env vars exported BEFORE Rscript runs because some libraries
#     read them at load time, not after RcppParallel::setThreadOptions.
# ==============================================================================
#SBATCH --job-name=klr
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=4G
#SBATCH --time=23:59:00
#SBATCH --output=out/dict/slurm_logs/klr_%j.out
#SBATCH --error=out/dict/slurm_logs/klr_%j.err

set -euo pipefail
module load r

# Critical: export thread counts BEFORE Rscript starts so libraries that read
# env at load time (BLAS, OpenMP, RcppParallel internals) see the right count.
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export MKL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export RCPP_PARALLEL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"

mkdir -p out/dict/slurm_logs

echo "============================================================"
echo "[$(date +'%F %T')] STAGE 2 — KLR keyword discovery"
echo "  Host       : $(hostname)"
echo "  Job ID     : ${SLURM_JOB_ID}"
echo "  CPUs       : ${SLURM_CPUS_PER_TASK}"
echo "  Mem/CPU    : ${SLURM_MEM_PER_CPU:-?} MB  (total: $((${SLURM_MEM_PER_CPU:-0} * ${SLURM_CPUS_PER_TASK:-1})) MB)"
echo "  OMP_NUM_THREADS  = ${OMP_NUM_THREADS}"
echo "  RCPP_PARALLEL    = ${RCPP_PARALLEL_NUM_THREADS}"
echo "  R version  : $(Rscript --version 2>&1)"
echo "============================================================"

Rscript pipeline/R/02b_klr_discovery.R

echo "============================================================"
echo "[$(date +'%F %T')] DONE"
echo "============================================================"
