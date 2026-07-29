#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_exposure_combine.sh
# Stage 3 combine — global TF-IDF (Eq.4) + firm-quarter panel. Single job,
# submit with --dependency=afterok on the exposure array.
# ==============================================================================
#SBATCH --job-name=expo_comb
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=7G
#SBATCH --time=04:00:00
#SBATCH --output=out/exposure/slurm_logs/combine_%j.out
#SBATCH --error=out/exposure/slurm_logs/combine_%j.err

set -euo pipefail
module load r
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
mkdir -p out/exposure/slurm_logs

echo "[$(date +'%F %T')] Stage 3 combine | $(hostname)"
Rscript pipeline/R/03b_combine_exposure.R
echo "[$(date +'%F %T')] combine done"
