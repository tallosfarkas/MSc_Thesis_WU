#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_export_parquet.sh
#
# Stage 2p — export Stage-1 sentences (out/corpus/sentences_YYYY.rds) to parquet
# so the Python sklearn pipeline can read raw sentence TEXT. Per-year array.
#   sbatch pipeline/cluster/slurm_export_parquet.sh        # array 1-24
# ==============================================================================
#SBATCH --job-name=export_pq
#SBATCH --partition=short
#SBATCH --array=1-24
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=8G
#SBATCH --time=01:00:00
#SBATCH --output=out/corpus_parquet/slurm_logs/export_%A_%a.out
#SBATCH --error=out/corpus_parquet/slurm_logs/export_%A_%a.err

set -euo pipefail
module load r
mkdir -p out/corpus_parquet/slurm_logs

echo "[$(date +'%F %T')] export year array task ${SLURM_ARRAY_TASK_ID} on $(hostname)"
Rscript pipeline/R/02p_export_sentences.R
echo "[$(date +'%F %T')] done task ${SLURM_ARRAY_TASK_ID}"
