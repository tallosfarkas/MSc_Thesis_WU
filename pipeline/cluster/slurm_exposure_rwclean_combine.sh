#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_exposure_rwclean_combine.sh
# Combine the per-year risk-word-CLEAN exposure into out/exposure_rwclean/
# exposure_calls.rds (via GEO_EXPO=rwclean), then run the identifier mapping so
# the firm-quarter CRSP panel is ready to pull for the local 05j headline check.
# ==============================================================================
#SBATCH --job-name=expo_rwclean_comb
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=7G
#SBATCH --time=04:00:00
#SBATCH --output=out/exposure_rwclean/slurm_logs/combine_%j.out
#SBATCH --error=out/exposure_rwclean/slurm_logs/combine_%j.err

set -euo pipefail
module load r
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export GEO_EXPO=rwclean
export GEO_RISKWORDS=risk_words_clean.yml
mkdir -p out/exposure_rwclean/slurm_logs

echo "[$(date +'%F %T')] combine rwclean exposure"
Rscript pipeline/R/03b_combine_exposure.R
# 03e needs the v8.2 map + local pricing; it runs fine on the cluster copy too.
Rscript pipeline/R/03e_map_identifiers.R
echo "[$(date +'%F %T')] done combine + map (rwclean)"
