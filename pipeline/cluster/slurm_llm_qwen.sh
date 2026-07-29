#!/usr/bin/env bash
# ==============================================================================
# pipeline/cluster/slurm_llm_qwen.sh  (Stage 4 — Qwen-72B judge, the strong arm)
# The legacy 83%-precision model. 2x A30 (4-bit). Also runs the CPU NLI judge on
# the same sample (cheap) so 04f can report a multi-model table + inter-model
# agreement. Assumes the GPU venv exists (pipeline/cluster/setup_llm_gpu_env.sh).
#   sbatch --dependency=afterok:<sampleJID> pipeline/cluster/slurm_llm_qwen.sh
# ==============================================================================
#SBATCH --job-name=llm_qwen
#SBATCH --partition=gpu
#SBATCH --gres=gpu:2
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=80G
#SBATCH --time=06:00:00
#SBATCH --output=out/validation/slurm_logs/llm_qwen_%j.out
#SBATCH --error=out/validation/slurm_logs/llm_qwen_%j.err

set -euo pipefail
mkdir -p out/validation/slurm_logs
ml purge
module load python/3.11.11-gcc-15.1.0-u2m6tuq
export HF_HOME="$HOME/hf_cache"; mkdir -p "$HF_HOME"

VENV="$HOME/llm_gpu_env"
if [ ! -f "$VENV/bin/activate" ]; then
  echo "[$(date +'%F %T')] GPU venv missing -> building"
  bash pipeline/cluster/setup_llm_gpu_env.sh
fi
source "$VENV/bin/activate"
python -c "import torch; [print(f'  GPU {i}: {torch.cuda.get_device_name(i)} {torch.cuda.get_device_properties(i).total_memory//1024**3}GB') for i in range(torch.cuda.device_count())]" || true

MODEL="${1:-large}"   # pass 'small' (Qwen-7B, 1 GPU) or 'large' (Qwen-72B, 2 GPU)
echo "[$(date +'%F %T')] 1/3 Qwen ($MODEL) ..."
python pipeline/python/04e_llm_classify.py --model "$MODEL"

echo "[$(date +'%F %T')] 2/3 NLI cross-check (CPU on this node) ..."
python pipeline/python/04e_llm_classify.py --model nli

echo "[$(date +'%F %T')] 3/3 scoring (all available models) ..."
module load r
Rscript pipeline/R/04f_llm_precision.R
echo "[$(date +'%F %T')] done"
