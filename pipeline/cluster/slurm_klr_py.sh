#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_klr_py.sh
#
# Stage 2b PYTHON — genuine sklearn KLR discovery (Sautner-faithful), full corpus.
# Mirrors pipeline/R/02b_klr_discovery.R but with the real estimators Sautner
# names (MultinomialNB + LinearSVC + RandomForest, GridSearchCV cv=5).
#
# Memory is the binding constraint: the untrimmed bigram matrix for ~166M
# sentences is ~60 GB (CountVectorizer holds the full vocab, no min_df). We ask
# for 64 CPUs x 7 GB = 448 GB (< the 500 GB node ceiling). sklearn parallelism
# is via joblib n_jobs, so BLAS/OMP threads are pinned to 1 to avoid
# oversubscription.
#
#   sbatch pipeline/cluster/slurm_klr_py.sh
#   RF_VOCAB_SIZE=50000 sbatch ... (sensitivity)
# ==============================================================================
#SBATCH --job-name=klr_py
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=7G
#SBATCH --time=23:59:00
#SBATCH --output=out/dict/slurm_logs/klr_py_%j.out
#SBATCH --error=out/dict/slurm_logs/klr_py_%j.err

set -euo pipefail
mkdir -p out/dict/slurm_logs

# Mode passed as a plain arg (avoids the fragile --export=ALL env retrieval):
#   sbatch pipeline/cluster/slurm_klr_py.sh           -> sautner (default)
#   sbatch pipeline/cluster/slurm_klr_py.sh rparity   -> R-mirror
export KLR_MODE="${1:-sautner}"
# 2nd arg "calibrate" -> NB+RF Platt-calibrated (robustness; writes *_cal).
[ "${2:-}" = "calibrate" ] && export KLR_CALIBRATE=1

# joblib does the parallelism; pin math-library threads to 1 (sklearn-recommended)
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

PYBIN="pipeline/python/.venv/bin/python"

echo "============================================================"
echo "[$(date +'%F %T')] STAGE 2b PYTHON — sklearn KLR discovery"
echo "  Host    : $(hostname)"
echo "  Job ID  : ${SLURM_JOB_ID}"
echo "  CPUs    : ${SLURM_CPUS_PER_TASK}  | Mem/CPU: ${SLURM_MEM_PER_CPU:-?} MB"
echo "  Python  : $($PYBIN --version 2>&1)"
echo "  KLR_MODE: ${KLR_MODE}"
echo "============================================================"

$PYBIN pipeline/python/02b_klr_discovery.py

echo "[$(date +'%F %T')] DONE"
