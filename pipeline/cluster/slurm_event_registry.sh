#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_event_registry.sh  (Stage 3.5 — event registry, CLUSTER)
# Builds out/exposure/event_registry.rds from out/corpus/calls_*.rds (one row per
# Id, earliest-file-year ticker). Slim output to pull local for 03e mapping.
#   sbatch pipeline/cluster/slurm_event_registry.sh
# ==============================================================================
#SBATCH --job-name=evt_registry
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=16G
#SBATCH --time=01:00:00
#SBATCH --output=out/exposure/slurm_logs/evt_registry_%j.out
#SBATCH --error=out/exposure/slurm_logs/evt_registry_%j.err

set -euo pipefail
mkdir -p out/exposure/slurm_logs
module load r
echo "[$(date +'%F %T')] building event registry | $(hostname)"
Rscript pipeline/R/03d_event_registry.R
echo "[$(date +'%F %T')] done"
