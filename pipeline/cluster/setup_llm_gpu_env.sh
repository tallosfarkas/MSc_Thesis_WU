#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/setup_llm_gpu_env.sh
# One-time GPU Python env for the Qwen LLM judge (Stage 4 snippet audit).
# Adapted from the proven legacy scripts/cluster/setup_llm_env.sh. Run on a
# LOGIN node (pip install needs no GPU); the CUDA check just warns if run there.
#
#   ssh wucluster
#   cd ~/thesis_clean && bash pipeline/cluster/setup_llm_gpu_env.sh
# ==============================================================================
set -euo pipefail
VENV="$HOME/llm_gpu_env"
echo "=== GPU LLM env setup -> $VENV ==="
module purge
module load python/3.11.11-gcc-15.1.0-u2m6tuq
echo "  python: $(python --version)"

[ -f "$VENV/bin/activate" ] || python -m venv "$VENV"
source "$VENV/bin/activate"
pip install --upgrade pip --quiet

echo "  installing torch (CUDA 11.8, matches A30) ..."
pip install torch --index-url https://download.pytorch.org/whl/cu118 --quiet
echo "  installing transformers / bitsandbytes / accelerate ..."
pip install transformers bitsandbytes accelerate sentencepiece protobuf pandas scikit-learn --quiet

python - <<'PY'
import torch, transformers
print(f"  torch {torch.__version__} | CUDA avail {torch.cuda.is_available()} | transformers {transformers.__version__}")
try:
    import bitsandbytes as bnb; print(f"  bitsandbytes {bnb.__version__}")
except Exception as e:
    print(f"  bitsandbytes import warn: {e}")
PY
echo "=== done. Model weights download on first GPU run into \$HF_HOME ==="
