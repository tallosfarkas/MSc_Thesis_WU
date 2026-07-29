#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_tokenise_v2.sh
#
# V2 CORRECTNESS TRACK (methodology audit 2026-06-12). Stage 2a re-tokenise,
# array over 24 years, with GEOV2=1:
#   - EXACT sklearn tokenizer (regex run extraction; fixes the hyphen/possessive
#     deletion of the ICU path: "supply-chain" -> supply, chain)
#   - stopword YAML boolean fix (on/off/no were never removed in v1)
# Writes out/dict_v2/dfm_YYYY.rds + meta_YYYY.rds. v1 outputs untouched.
#
#   sbatch pipeline/cluster/slurm_tokenise_v2.sh
# ==============================================================================
#SBATCH --job-name=tokenise_v2
#SBATCH --partition=short
#SBATCH --array=1-24
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=06:00:00
#SBATCH --output=out/dict_v2/slurm_logs/tokenise_%A_%a.out
#SBATCH --error=out/dict_v2/slurm_logs/tokenise_%A_%a.err

set -euo pipefail
module load r
export GEOV2=1
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
export RCPP_PARALLEL_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
mkdir -p out/dict_v2/slurm_logs

YEAR=$((2001 + SLURM_ARRAY_TASK_ID))
echo "[$(date +'%F %T')] STAGE 2a V2 — tokenise ${YEAR} | $(hostname)"
Rscript pipeline/R/02a_tokenise.R "${YEAR}"
echo "[$(date +'%F %T')] DONE"
