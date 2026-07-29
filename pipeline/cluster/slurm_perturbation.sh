#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_perturbation.sh
# Stage 4 — leave-one-seed perturbation (Sautner robustness). Array 1-50: ONE
# dropped seed per task. Each task vectorises once (~5 min) then runs a single
# full re-derivation (the RF predict over ~166M sentences is the ~3.5h long pole,
# so one derive/task keeps every task well under walltime; the array fans out
# across nodes instead of serialising 50 derivations).
#   sbatch pipeline/cluster/slurm_perturbation.sh
#   then: sbatch --dependency=afterok:<JID> --wrap 'module load r; Rscript pipeline/R/04b_perturbation_corr.R'
# ==============================================================================
#SBATCH --job-name=perturb
#SBATCH --partition=short
#SBATCH --array=1-50%21
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --mem-per-cpu=6G
#SBATCH --time=08:00:00
#SBATCH --output=out/dict/perturb/slurm_logs/perturb_%A_%a.out
#SBATCH --error=out/dict/perturb/slurm_logs/perturb_%A_%a.err

set -euo pipefail
mkdir -p out/dict/perturb/slurm_logs
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
I=$(( SLURM_ARRAY_TASK_ID - 1 ))   # 0-based dropped-seed index
export KLR_PERTURB_RANGE="${I}-${I}"
echo "[$(date +'%F %T')] perturbation seeds ${KLR_PERTURB_RANGE} | $(hostname)"
pipeline/python/.venv/bin/python pipeline/python/04_perturbation.py
echo "[$(date +'%F %T')] done range ${KLR_PERTURB_RANGE}"
