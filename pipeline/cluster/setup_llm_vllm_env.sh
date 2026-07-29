#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/setup_llm_vllm_env.sh
# Separate venv for the vLLM large-model ceiling test (04e2_llm_vllm.py). vLLM
# bundles its own torch, so it must NOT share the transformers/bitsandbytes env.
# Run on a LOGIN node. vLLM may fail to build against the A30 / CUDA stack -- that
# is fine; 04e2 records status="vllm_unavailable" and the case study reports the
# transformers ceiling (Qwen-32B) instead.
#   ssh wucluster && cd ~/thesis_clean && bash pipeline/cluster/setup_llm_vllm_env.sh
# ==============================================================================
set -uo pipefail   # NOT -e: we want to report a vLLM build failure, not abort silently
VENV="$HOME/llm_vllm_env"
echo "=== vLLM env setup -> $VENV ==="
module purge
module load python/3.11.11-gcc-15.1.0-u2m6tuq
python --version

[ -f "$VENV/bin/activate" ] || python -m venv "$VENV"
source "$VENV/bin/activate"
pip install --upgrade pip --quiet

echo "  installing vllm (pulls a compatible torch) ..."
if pip install "vllm>=0.6,<0.9" autoawq --quiet; then
  python - <<'PY'
try:
    import vllm, torch
    print(f"  vllm {vllm.__version__} | torch {torch.__version__} | CUDA avail {torch.cuda.is_available()}")
except Exception as e:
    print(f"  vllm import FAILED post-install: {e}")
PY
  echo "=== vLLM env ready (or import-warned above) ==="
else
  echo "=== vLLM install FAILED -- 04e2 will record vllm_unavailable; that is a valid feasibility result ==="
fi
