#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_exposure.sh
# Stage 3 — per-year exposure measurement (Sautner Eqs 1-4). Array 1-24.
# Reads out/dict/dfm_YYYY.rds + out/corpus/{sentences,calls}_YYYY.rds, writes
# out/exposure/exposure_YYYY.rds + tfidf_parts_YYYY.rds. Chain the combine after:
#   A=$(sbatch --parsable slurm_exposure.sh)
#   sbatch --dependency=afterok:$A slurm_exposure_combine.sh
# ==============================================================================
#SBATCH --job-name=exposure
#SBATCH --partition=short
#SBATCH --array=1-24
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=03:00:00
#SBATCH --output=out/exposure/slurm_logs/exposure_%A_%a.out
#SBATCH --error=out/exposure/slurm_logs/exposure_%A_%a.err

set -euo pipefail
module load r
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export RCPP_PARALLEL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
mkdir -p out/exposure/slurm_logs

echo "[$(date +'%F %T')] Stage 3 exposure | array task ${SLURM_ARRAY_TASK_ID} | $(hostname)"
Rscript pipeline/R/03_measure_exposure.R
echo "[$(date +'%F %T')] done task ${SLURM_ARRAY_TASK_ID}"
