#!/usr/bin/env bash
CLUSTER="${CLUSTER:-wucluster}"   # override with your own SSH host alias
# ==============================================================================
# pipeline/cluster/rsync_from_cluster.sh
#
# Pull outputs from ${CLUSTER}:~/thesis_clean/out/ back to local out/.
# Use after a cluster job finishes to inspect results locally.
#
# Usage (from project root):
#   bash pipeline/cluster/rsync_from_cluster.sh         # DRY RUN  (small artefacts)
#   bash pipeline/cluster/rsync_from_cluster.sh go      # pull small artefacts
#   bash pipeline/cluster/rsync_from_cluster.sh go --full   # also pull heavy intermediates
#
# Default scope: only small, non-regenerable artefacts (dictionary CSVs, audit
# JSON, summary CSVs). The heavy intermediate RDS files (dfm_YYYY.rds,
# meta_YYYY.rds, classifier_predictions.rds — ~6 GB) stay cluster-only: they
# are regenerable and only consumed by Stage 3 which also runs on the cluster.
# Keeps the local OneDrive folder lean.
#
# Pass --full to pull everything (e.g. for offline debugging of tokenisation).
#
# Pipeline code never flows back (cluster is mirror, local is source of truth).
# ==============================================================================
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REMOTE="${CLUSTER}:~/thesis_clean/out/"
LOCAL="$PROJECT_ROOT/out/"
MODE="${1:-dry}"
SCOPE="${2:-}"

FLAGS=(-avz --partial --progress --human-readable
       --exclude='slurm_logs/'
       --exclude='*.log')

# Default: exclude heavy regenerable intermediates. --full overrides.
if [ "$SCOPE" != "--full" ]; then
  FLAGS+=(--exclude='dict/dfm_*.rds'
          --exclude='dict/meta_*.rds'
          --exclude='dict/classifier_predictions.rds'
          --exclude='corpus/sentences_*.rds'
          --exclude='corpus/calls_*.rds')
  echo "=== Scope: SMALL artefacts only (dictionary CSVs, audit JSON, summaries) ==="
  echo "    Pass 'go --full' to also pull heavy intermediates (~6 GB)."
else
  echo "=== Scope: FULL (including ~6 GB of regenerable intermediates) ==="
fi
if [ "$MODE" != "go" ]; then
  FLAGS+=(-n)
  echo "=== DRY RUN (no files transferred). Pass 'go' to actually pull. ==="
else
  echo "=== PULL from $REMOTE -> $LOCAL ==="
fi

mkdir -p "$LOCAL"
rsync "${FLAGS[@]}" "$REMOTE" "$LOCAL"

if [ "$MODE" = "go" ]; then
  echo
  echo "=== Pull complete ==="
  echo "Local outputs:"
  du -sh "$LOCAL"*/  2>/dev/null || true
fi
