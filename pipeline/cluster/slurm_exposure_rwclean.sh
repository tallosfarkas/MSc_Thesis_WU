#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_exposure_rwclean.sh
# ROBUSTNESS (faithfulness) — Stage 3 re-score with the risk-word-CLEAN list
# (risk_words_clean.yml drops the circular 'exposure'/'exposed'). Writes to
# out/exposure_rwclean/ via GEO_EXPO=rwclean; reuses the LOCKED dictionary DFMs
# in out/dict (only the Eq.3 risk conditioning changes). Array 1-24, then combine:
#   A=$(sbatch --parsable slurm_exposure_rwclean.sh)
#   sbatch --dependency=afterok:$A slurm_exposure_rwclean_combine.sh
# ==============================================================================
#SBATCH --job-name=expo_rwclean
#SBATCH --partition=short
#SBATCH --array=1-24
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=03:00:00
#SBATCH --output=out/exposure_rwclean/slurm_logs/expo_%A_%a.out
#SBATCH --error=out/exposure_rwclean/slurm_logs/expo_%A_%a.err

set -euo pipefail
module load r
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export RCPP_PARALLEL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
# Robustness track: clean risk-word list + dedicated output dir (dict DFMs unchanged).
export GEO_EXPO=rwclean
export GEO_RISKWORDS=risk_words_clean.yml
mkdir -p out/exposure_rwclean/slurm_logs

echo "[$(date +'%F %T')] Stage 3 rwclean | array task ${SLURM_ARRAY_TASK_ID} | $(hostname)"
Rscript pipeline/R/03_measure_exposure.R
echo "[$(date +'%F %T')] done task ${SLURM_ARRAY_TASK_ID}"
