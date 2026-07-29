#!/usr/bin/env python3
# ==============================================================================
# pipeline/python/04e2_llm_vllm.py   (Stage 4 — the LARGE-model ceiling test)
#
# Tests whether the WU cluster (2x A30, 48 GB) can run a LARGE LLM at all. vLLM does
# NOT build on this stack (transformers/ProcessorMixin clash), so we load pre-quantized
# AWQ 4-bit weights via transformers + autoawq (device_map="auto"). Tries a DESCENDING
# priority list and uses the FIRST that loads -- so the "large" tier is the biggest model
# that actually fits, per Hornik's advice ("smaller/older models that fit"):
#   1. Qwen2.5-72B-Instruct-AWQ   (Clayton size; ~40 GB AWQ, tight on 2x A30)
#   2. Mixtral-8x7B-Instruct-AWQ  (MoE 47B total / 13B active, ~25 GB -- the "older model that fits")
# Same flag + extract tasks/prompts/parsers as 04e. Writes results + timing JSON; if none
# load, records status="does_not_fit" with the error (a valid feasibility outcome).
#
#   python pipeline/python/04e2_llm_vllm.py --task flag    [--limit 220]
#   python pipeline/python/04e2_llm_vllm.py --task extract --limit 50
# ==============================================================================
import argparse, os, time, json
from pathlib import Path
import pandas as pd
import importlib.util

def find_root():
    d = os.path.abspath(os.getcwd())
    while d not in ("/", ""):
        if os.path.exists(os.path.join(d, "pipeline", "config", "params.yml")): return d
        d = os.path.dirname(d)
    raise SystemExit("no root")
ROOT = find_root()
_spec = importlib.util.spec_from_file_location("c04e", os.path.join(ROOT, "pipeline/python/04e_llm_classify.py"))
c04e = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(c04e)

LARGE_PRIORITY = [
    "Qwen/Qwen2.5-72B-Instruct-AWQ",
    "TheBloke/Mixtral-8x7B-Instruct-v0.1-AWQ",
]

def write_status(task, status, model=None, err=None, **extra):
    rec = {"model": model or "large", "task": task, "runtime": "transformers-awq", "status": status}
    if err: rec["error"] = str(err)[:400]
    rec.update(extra)
    p = os.path.join(ROOT, f"out/validation/llm_timing_large_{task}.json")
    with open(p, "w") as f: json.dump(rec, f, indent=2)
    print(f"  [large/{task}] status={status} model={model} -> {p}", flush=True)
    return rec

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--task", choices=["flag", "extract"], default="flag")
    ap.add_argument("--input", default=os.path.join(ROOT, "out/validation/llm_validation_sample.csv"))
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--models", default=None, help="comma-separated override of the priority list")
    a = ap.parse_args()
    if "HF_HOME" not in os.environ:
        os.environ["HF_HOME"] = str(Path.home() / "hf_cache"); Path(os.environ["HF_HOME"]).mkdir(parents=True, exist_ok=True)
    df = pd.read_csv(a.input)
    if "text" not in df.columns and "snippet_text" in df.columns: df["text"] = df["snippet_text"]
    if "item_id" not in df.columns and "snippet_id" in df.columns: df["item_id"] = df["snippet_id"]
    df = df[df["text"].notna() & (df["text"].astype(str).str.strip() != "")].reset_index(drop=True)
    if a.limit and a.limit < len(df): df = df.iloc[:a.limit].reset_index(drop=True)
    print(f"=== LARGE ceiling test (transformers-AWQ): task={a.task} | {len(df)} items", flush=True)

    try:
        import torch
        from transformers import AutoTokenizer, AutoModelForCausalLM
    except Exception as e:
        write_status(a.task, "transformers_unavailable", err=e); return

    sys_msg = c04e.EXTRACT_SYSTEM if a.task == "extract" else c04e.SYSTEM
    usr_tpl = c04e.EXTRACT_PROMPT if a.task == "extract" else c04e.PROMPT
    new_tok = 256 if a.task == "extract" else 5
    trunc   = 1200 if a.task == "extract" else 800
    parse   = c04e.parse_extract if a.task == "extract" else c04e.parse_yn

    cand = a.models.split(",") if a.models else LARGE_PRIORITY
    tok = mdl = used = None; last_err = None
    for m in cand:
        try:
            print(f"  trying {m} (AWQ, device_map=auto) ...", flush=True)
            torch.cuda.reset_peak_memory_stats() if torch.cuda.is_available() else None
            tok = AutoTokenizer.from_pretrained(m, trust_remote_code=True)
            tok.padding_side = "left"
            if tok.pad_token is None: tok.pad_token = tok.eos_token
            mdl = AutoModelForCausalLM.from_pretrained(m, device_map="auto",
                    trust_remote_code=True, torch_dtype=torch.float16).eval()
            used = m; print(f"  LOADED {m}", flush=True); break
        except Exception as e:
            last_err = e; print(f"  FAILED {m}: {str(e)[:200]}", flush=True)
            try: del mdl
            except Exception: pass
            if torch.cuda.is_available(): torch.cuda.empty_cache()
    if mdl is None:
        write_status(a.task, "does_not_fit", err=last_err, tried=cand); return

    bs = 4 if a.task == "flag" else 1
    out, t0 = [], time.time()
    for i in range(0, len(df), bs):
        b = df.iloc[i:i+bs]; prompts = []
        for _, row in b.iterrows():
            msgs = [{"role":"system","content":sys_msg},{"role":"user","content":usr_tpl % str(row["text"])[:trunc]}]
            prompts.append(tok.apply_chat_template(msgs, tokenize=False, add_generation_prompt=True))
        inp = tok(prompts, return_tensors="pt", padding=True, truncation=True, max_length=1536).to(mdl.device)
        with torch.no_grad():
            oid = mdl.generate(**inp, max_new_tokens=new_tok, do_sample=False, pad_token_id=tok.pad_token_id)
        ilen = inp["input_ids"].shape[1]
        for j, (_, row) in enumerate(b.iterrows()):
            raw = tok.decode(oid[j, ilen:], skip_special_tokens=True).strip()
            out.append({"item_id": row["item_id"], "llm_label": parse(raw), "llm_score": None, "llm_raw": raw[:240]})
        print(f"  [{i+len(b)}/{len(df)}] {time.time()-t0:.0f}s", flush=True)
    total_s = time.time() - t0

    tag = "large" if a.task == "flag" else f"large_{a.task}"
    outp = os.path.join(ROOT, f"out/validation/llm_results_{tag}.csv")
    df.merge(pd.DataFrame(out), on="item_id", how="left").to_csv(outp, index=False)
    peak = max((torch.cuda.max_memory_allocated(i) for i in range(torch.cuda.device_count())), default=0)/1024**3
    write_status(a.task, "ok", model=used, n_items=len(df), n_gpu=torch.cuda.device_count(),
                 total_s=round(total_s,1), sec_per_item=round(total_s/max(len(df),1),3),
                 items_per_s=round(len(df)/max(total_s,1e-9),3), peak_vram_gb=round(peak,2), max_new_tokens=new_tok)
    print(f"  done: {used} {len(df)} items in {total_s:.0f}s ({total_s/max(len(df),1):.2f}s/item, peak {peak:.1f}GB) -> {outp}", flush=True)

if __name__ == "__main__":
    main()
