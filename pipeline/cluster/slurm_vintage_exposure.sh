#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_vintage_exposure.sh   (Phase B of the expanding-window
# OOS analysis) — REAL-TIME exposure scoring.
#
# For each year Y, score that year's calls with the dictionary discovered on
# text <= Y-1 (the vintage that a trader would actually have had at the start of
# year Y). Reuses the tested Stage-3 scorer (03_measure_exposure.R) via two env
# overrides: GEO_DICT (the vintage csv) and GEO_EXP_TAG=rt (namespaces output to
# exposure_rt_<year>.rds). GeoRisk/GeoSentiment/GeoExposure (Eqs 1-3) are
# per-call, so no global-docfreq complication; Eq.4 TF-IDF is not real-time-
# adjusted here (not the tradeable measure).
#
# Depends on Phase A (slurm_vintage_discovery.sh) via --dependency=afterok.
# Array index = year Y (2013..2025) -> uses vintage Y-1 (2012..2024).
# ==============================================================================
#SBATCH --job-name=rt_exposure
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=03:00:00
#SBATCH --array=2013-2025
#SBATCH --output=out/exposure/slurm_logs/rt_exposure_%A_%a.out
#SBATCH --error=out/exposure/slurm_logs/rt_exposure_%A_%a.err

set -euo pipefail
module load r
mkdir -p out/exposure/slurm_logs

YEAR="${SLURM_ARRAY_TASK_ID}"
VINT=$(( YEAR - 1 ))
DICT="$(pwd)/out/dict/vintage/dictionary_vintage_${VINT}.csv"

echo "============================================================"
echo "[$(date +'%F %T')] PHASE B — real-time exposure: year ${YEAR} scored with vintage ${VINT}"
echo "  dict: ${DICT}"
echo "============================================================"
if [ ! -f "${DICT}" ]; then
  echo "ERROR: vintage dictionary not found: ${DICT} (did Phase A finish for ${VINT}?)" >&2
  exit 1
fi

GEO_DICT="${DICT}" GEO_EXP_TAG=rt Rscript pipeline/R/03_measure_exposure.R "${YEAR}"

echo "[$(date +'%F %T')] DONE real-time exposure year ${YEAR}"
