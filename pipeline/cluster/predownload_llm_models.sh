#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/predownload_llm_models.sh
# Pre-download the LLM weights on a LOGIN node (HF is ~46 MB/s on login vs
# ~0.85 MB/s on the throttled GPU nodes). The feasibility job runs HF_HUB_OFFLINE.
# Downloads: Qwen-7B, Qwen-32B (transformers 4-bit) + the large AWQ candidates
# (Qwen-72B-AWQ = the 70B ceiling test; Mixtral-8x7B-AWQ = the "older model that fits").
#   ssh wucluster && cd ~/thesis_clean && bash pipeline/cluster/predownload_llm_models.sh
# ==============================================================================
set -uo pipefail
module purge
module load python/3.11.11-gcc-15.1.0-u2m6tuq
export HF_HOME="$HOME/hf_cache"; mkdir -p "$HF_HOME"
GVENV="$HOME/llm_gpu_env"; [ -f "$GVENV/bin/activate" ] || bash pipeline/cluster/setup_llm_gpu_env.sh
source "$GVENV/bin/activate"
pip install -q "huggingface_hub[cli]" || true

MODELS=(
  "Qwen/Qwen2.5-7B-Instruct"
  "Qwen/Qwen2.5-32B-Instruct"
  "Qwen/Qwen2.5-72B-Instruct-AWQ"
  "TheBloke/Mixtral-8x7B-Instruct-v0.1-AWQ"
)
for m in "${MODELS[@]}"; do
  echo "=== downloading $m -> $HF_HOME ($(date +'%T')) ==="
  python - "$m" <<'PY' || echo "  WARN: $1 download failed (continuing)"
import sys
from huggingface_hub import snapshot_download
m = sys.argv[1]
snapshot_download(m, ignore_patterns=["*.pth","*.bin.index.json.lock","original/*"], max_workers=8)
print("  cached:", m)
PY
done
echo "=== predownload done. du -sh \$HF_HOME ==="; du -sh "$HF_HOME" 2>/dev/null || true
