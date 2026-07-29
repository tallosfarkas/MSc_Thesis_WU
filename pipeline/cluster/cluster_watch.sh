#!/usr/bin/env bash
CLUSTER="${CLUSTER:-wucluster}"   # override with your own SSH host alias
# ==============================================================================
# pipeline/cluster/cluster_watch.sh
# Resilient SLURM job monitor. Instead of holding one long SSH session (which
# dies on VPN drops), it makes short independent connections on an interval,
# tolerates failed connections (retries next tick), and exits when the watched
# job leaves the queue.
#
#   bash pipeline/cluster/cluster_watch.sh <JOBID> [interval_sec] [max_ticks]
# Defaults: interval 120s, max_ticks 200 (~6.5h).
# ==============================================================================
set -uo pipefail
JOB="${1:?usage: cluster_watch.sh <JOBID> [interval] [max_ticks]}"
INT="${2:-120}"
MAX="${3:-200}"
REMOTE="~/thesis_clean"

for t in $(seq 1 "$MAX"); do
  OUT=$(ssh -o ConnectTimeout=10 ${CLUSTER} "
    cd $REMOTE 2>/dev/null
    ST=\$(squeue -h -j $JOB -o '%T %M %L' 2>/dev/null)
    if [ -z \"\$ST\" ]; then echo 'GONE'; else echo \"STATE \$ST\"; fi
    LOG=\$(ls -t out/dict/slurm_logs/*${JOB}*.out 2>/dev/null | head -1)
    [ -n \"\$LOG\" ] && tail -n 3 \"\$LOG\"
  " 2>/dev/null) || { echo "[$(date +%H:%M)] tick $t: connection failed (VPN?), retrying"; sleep "$INT"; continue; }

  echo "[$(date +%H:%M)] tick $t:"; echo "$OUT" | sed 's/^/    /'
  if printf '%s' "$OUT" | grep -q '^GONE'; then
    echo "[$(date +%H:%M)] job $JOB left the queue — done."; exit 0
  fi
  sleep "$INT"
done
echo "watch hit max_ticks without completion (job may still be running)."
