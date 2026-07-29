#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_exposure_v2.sh
# V2 CORRECTNESS TRACK — Stage 3 exposure per year with GEOV2=1: reads
# out/dict_v2/ dfms (exact tokenizer), uses risk_words_v2.yml if present
# (Hassan/Sautner-reconciled), writes out/exposure_v2/. Chain combine after:
#   A=$(sbatch --parsable slurm_exposure_v2.sh)
#   sbatch --dependency=afterok:$A slurm_exposure_combine_v2.sh
# ==============================================================================
#SBATCH --job-name=exposure_v2
#SBATCH --partition=short
#SBATCH --array=1-24
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=06:00:00
#SBATCH --output=out/exposure_v2/slurm_logs/exposure_%A_%a.out
#SBATCH --error=out/exposure_v2/slurm_logs/exposure_%A_%a.err

set -euo pipefail
module load r
export GEOV2=1
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export RCPP_PARALLEL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
mkdir -p out/exposure_v2/slurm_logs

echo "[$(date +'%F %T')] Stage 3 V2 exposure | task ${SLURM_ARRAY_TASK_ID} | $(hostname)"
Rscript pipeline/R/03_measure_exposure.R
echo "[$(date +'%F %T')] done task ${SLURM_ARRAY_TASK_ID}"
