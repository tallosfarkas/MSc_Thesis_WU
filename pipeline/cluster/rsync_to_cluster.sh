#!/usr/bin/env bash
CLUSTER="${CLUSTER:-wucluster}"   # override with your own SSH host alias
# ==============================================================================
# pipeline/cluster/rsync_to_cluster.sh
#
# Selective rsync of the MINIMAL working set to ${CLUSTER}:~/thesis_clean/.
# Scope (Step 1 of /Users/farkastallos/.claude/plans/recursive-drifting-hare.md):
#   * data/parsed/    (~12 GB; 24 TParsed_YYYY.RData files)
#   * pipeline/       (configs + R + cluster scripts)
# Excludes everything else — legacy scripts, raw, processed, archive, etc.
#
# Usage (from project root):
#   bash pipeline/cluster/rsync_to_cluster.sh        # DRY RUN (default; safe)
#   bash pipeline/cluster/rsync_to_cluster.sh go     # actually push
# ==============================================================================
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REMOTE="${CLUSTER}:~/thesis_clean"
MODE="${1:-dry}"

FLAGS=(-avz --partial --progress --human-readable
       --exclude-from="$PROJECT_ROOT/pipeline/cluster/rsync.exclude")
if [ "$MODE" != "go" ]; then
  FLAGS+=(-n)
  echo "=== DRY RUN (no files will be transferred). Pass 'go' to push. ==="
else
  echo "=== LIVE PUSH to $REMOTE ==="
fi

# Ensure intermediate dirs exist on the remote — rsync 3.x errors if not.
echo
echo "--- Ensuring remote directory structure exists ---"
ssh ${CLUSTER} 'mkdir -p ~/thesis_clean/pipeline ~/thesis_clean/data/parsed'

echo
echo "--- Syncing pipeline/ ---"
rsync "${FLAGS[@]}" "$PROJECT_ROOT/pipeline/" "$REMOTE/pipeline/"

echo
echo "--- Syncing data/parsed/ (~12 GB) ---"
rsync "${FLAGS[@]}" "$PROJECT_ROOT/data/parsed/" "$REMOTE/data/parsed/"

echo
if [ "$MODE" = "go" ]; then
  echo "=== Push complete ==="
  echo "Verify with:"
  echo "  ssh ${CLUSTER} 'du -sh ~/thesis_clean/* && ls ~/thesis_clean/data/parsed | wc -l && ls ~/thesis_clean/pipeline'"
else
  echo "=== Dry run complete. Re-run with 'go' to actually push. ==="
fi
