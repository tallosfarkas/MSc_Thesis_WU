#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_perturbation_measure.sh  (Stage 4 — faithful perturbation)
# One pass: vectorise corpus with the combined dropped+locked vocab, aggregate to
# call, recompute Eq.1 exposure per dropped-seed dict, correlate vs locked.
#   sbatch pipeline/cluster/slurm_perturbation_measure.sh
# ==============================================================================
#SBATCH --job-name=perturb_meas
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=10G
#SBATCH --time=04:00:00
#SBATCH --output=out/dict/perturb/slurm_logs/perturb_meas_%j.out
#SBATCH --error=out/dict/perturb/slurm_logs/perturb_meas_%j.err

set -euo pipefail
mkdir -p out/dict/perturb/slurm_logs
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK:-16}"
module load r   # 04g shells out to Rscript to pull B from exposure_calls.rds
echo "[$(date +'%F %T')] faithful perturbation (measure-level) | $(hostname)"
pipeline/python/.venv/bin/python pipeline/python/04g_perturbation_measure.py
echo "[$(date +'%F %T')] done"
