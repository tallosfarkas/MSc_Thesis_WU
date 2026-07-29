#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_exposure_combine_v2.sh
# V2 CORRECTNESS TRACK — Stage 3 combine with GEOV2=1 (global TF-IDF + firm-
# quarter panel from out/exposure_v2/). Submit with --dependency=afterok on the
# v2 exposure array.
# ==============================================================================
#SBATCH --job-name=expo_comb_v2
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=7G
#SBATCH --time=04:00:00
#SBATCH --output=out/exposure_v2/slurm_logs/combine_%j.out
#SBATCH --error=out/exposure_v2/slurm_logs/combine_%j.err

set -euo pipefail
module load r
export GEOV2=1
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
mkdir -p out/exposure_v2/slurm_logs

echo "[$(date +'%F %T')] Stage 3 V2 combine | $(hostname)"
Rscript pipeline/R/03b_combine_exposure.R
echo "[$(date +'%F %T')] combine done"
