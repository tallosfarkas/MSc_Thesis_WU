#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_realtime_oos.sh   (Phase C of the expanding-window OOS)
#
# Dictionary divergence (vintage vs full) + real-time LSEG GeoRisk/GeoSentiment
# L/S (fixed vs real-time dictionary, FF5 alpha in-sample & walk-forward OOS).
# Light job. Depends on Phase B (slurm_vintage_exposure.sh) via afterok.
# CRSP real-time L/S is a LOCAL follow-up (ec_ccm permno crosswalk is local-only).
# ==============================================================================
#SBATCH --job-name=rt_oos
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=02:00:00
#SBATCH --output=out/analysis/slurm_logs/rt_oos_%j.out
#SBATCH --error=out/analysis/slurm_logs/rt_oos_%j.err

set -euo pipefail
module load r
mkdir -p out/analysis/slurm_logs

echo "============================================================"
echo "[$(date +'%F %T')] PHASE C — real-time dictionary OOS + divergence"
echo "============================================================"

GEO_FIX=1 GEO_TAG=_v11 GEO_TIES=first Rscript pipeline/R/14_realtime_dict_oos.R

echo "[$(date +'%F %T')] DONE — out/analysis/realtime_dict_oos.json"
