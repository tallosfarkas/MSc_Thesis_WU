#!/usr/bin/env python3
# ==============================================================================
# pipeline/python/04e_llm_classify.py   (Stage 4 — LLM judge for the snippet audit)
#
# Adapts the legacy scripts/04_validation/01_llm_validation.py to the new
# pipeline paths + geoeconomic prompt. Reads the stratified sample from 04d and
# labels each snippet YES/NO for "discusses geoeconomic risk". 04f_llm_precision.R
# then scores precision/recall vs the dictionary flag (Sautner bar: >85% precision
# in the high-confidence / top-decile stratum).
#
# Models (--model):
#   nli   facebook/bart-large-mnli   (CPU, zero-shot NLI, ~1.6GB, no token)  [default]
#   small Qwen/Qwen2.5-7B-Instruct   (1 GPU, 4-bit)
#   med   Qwen/Qwen2.5-32B-Instruct  (2 GPU, 4-bit ~18-20GB)  <- biggest that fits 2xA30
#   large Qwen/Qwen2.5-72B-Instruct  (does NOT fit 2xA30 / 46GB; needs CPU offload)
#
#   python pipeline/python/04e_llm_classify.py --model med
# ==============================================================================
import argparse, os, time
from pathlib import Path
import pandas as pd

DECODERS = {"small": "Qwen/Qwen2.5-7B-Instruct",
            "med":   "Qwen/Qwen2.5-32B-Instruct",
            "large": "Qwen/Qwen2.5-72B-Instruct"}

GEO_LABELS = ["geoeconomic risk", "other content"]
PROMPT = (
    "Does this earnings-call passage discuss geoeconomic risk? Geoeconomic risk "
    "covers: trade wars, tariffs, export controls, sanctions, supply-chain "
    "disruption from geopolitical tensions, energy security, rare-earth access, "
    "technology bans, state-actor cyber threats, or geopolitical instability "
    "affecting business operations.\n\nText: \"\"\"%s\"\"\"\n\nAnswer YES or NO:"
)
SYSTEM = "You are a financial text classifier. Reply with exactly one word: YES or NO."

# Clayton-2025-style structured extraction (the expensive task): relevance + type + direction.
EXTRACT_SYSTEM = ("You are a financial text analyst extracting structured geoeconomic-pressure "
    "signals from earnings calls. Reply with ONLY a compact JSON object, no prose.")
EXTRACT_PROMPT = (
    "From the passage below, extract geoeconomic pressure as JSON with keys: "
    '"relevant" (true/false: does it discuss geoeconomic pressure?), '
    '"type" (one of tariff, sanction, export_control, supply_chain, other, none), '
    '"direction" (one of faced, imposed, threatened, none), '
    '"confidence" (0-1). If not relevant, set type and direction to "none".\n\n'
    "Passage: \"\"\"%s\"\"\"\n\nJSON:")

def find_root():
    d = os.path.abspath(os.getcwd())
    while d not in ("/", ""):
        if os.path.exists(os.path.join(d, "pipeline", "config", "params.yml")): return d
        d = os.path.dirname(d)
    raise SystemExit("no root")
ROOT = find_root()

def run_nli(df):
    from transformers import pipeline
    clf = pipeline("zero-shot-classification", model="facebook/bart-large-mnli",
                   device=-1, multi_label=False)
    out, t0, bs = [], time.time(), 16
    for i in range(0, len(df), bs):
        b = df.iloc[i:i+bs]; texts = [str(t)[:1500] for t in b["text"].tolist()]
        res = clf(texts, candidate_labels=GEO_LABELS, hypothesis_template="{}")
        res = res if isinstance(res, list) else [res]
        for j, (_, row) in enumerate(b.iterrows()):
            top, sc = res[j]["labels"][0], res[j]["scores"][0]
            out.append({"item_id": row["item_id"], "llm_label": "YES" if top == GEO_LABELS[0] else "NO",
                        "llm_score": round(float(sc), 4), "llm_raw": top})
        if (i // bs) % 10 == 0: print(f"  [{i+len(b)}/{len(df)}] {time.time()-t0:.0f}s", flush=True)
    return out

def parse_yn(raw):
    up = raw.upper().strip()
    if up.startswith("YES"): return "YES"
    if up.startswith("NO"): return "NO"
    for tk in up.split():
        if tk.rstrip(".,") == "YES": return "YES"
        if tk.rstrip(".,") == "NO": return "NO"
    return "UNCLEAR"

def parse_extract(raw):
    # Clayton-style: collapse the JSON to a YES/NO flag (relevant) for scoring; raw JSON kept in llm_raw.
    import json, re
    m = re.search(r"\{.*\}", raw, re.S)
    if m:
        try:
            o = json.loads(m.group(0))
            rel = o.get("relevant")
            if isinstance(rel, bool): return "YES" if rel else "NO"
            if isinstance(rel, str):  return "YES" if rel.lower() in ("true","yes") else "NO"
        except Exception:
            pass
    up = raw.upper()
    if '"RELEVANT": TRUE' in up or '"RELEVANT":TRUE' in up: return "YES"
    if '"RELEVANT": FALSE' in up or '"RELEVANT":FALSE' in up: return "NO"
    return "UNCLEAR"

def write_timing(model, task, n_items, total_s, new_tok):
    import json
    try:
        import torch
        ng = torch.cuda.device_count()
        peak = max((torch.cuda.max_memory_allocated(i) for i in range(ng)), default=0) / 1024**3 if ng else 0.0
    except Exception:
        ng, peak = 0, 0.0
    rec = {"model": model, "task": task, "n_items": int(n_items), "n_gpu": int(ng),
           "total_s": round(total_s, 1), "items_per_s": round(n_items/max(total_s,1e-9), 3),
           "sec_per_item": round(total_s/max(n_items,1), 3), "max_new_tokens": new_tok,
           "peak_vram_gb": round(peak, 2), "status": "ok"}
    p = os.path.join(ROOT, f"out/validation/llm_timing_{model}_{task}.json")
    with open(p, "w") as f: json.dump(rec, f, indent=2)
    print(f"  timing -> {p}: {rec['sec_per_item']}s/item, {rec['items_per_s']}/s, peak {rec['peak_vram_gb']}GB", flush=True)
    return rec

def run_decoder(df, model_name, bs, task="flag"):
    import torch
    from transformers import AutoTokenizer, AutoModelForCausalLM, BitsAndBytesConfig
    bnb = BitsAndBytesConfig(load_in_4bit=True, bnb_4bit_quant_type="nf4",
                             bnb_4bit_compute_dtype=torch.bfloat16, bnb_4bit_use_double_quant=True)
    tok = AutoTokenizer.from_pretrained(model_name, trust_remote_code=True)
    tok.padding_side = "left"
    if tok.pad_token is None: tok.pad_token = tok.eos_token
    mdl = AutoModelForCausalLM.from_pretrained(model_name, quantization_config=bnb,
            device_map="auto", trust_remote_code=True, torch_dtype=torch.bfloat16).eval()
    sys_msg = EXTRACT_SYSTEM if task == "extract" else SYSTEM
    usr_tpl = EXTRACT_PROMPT if task == "extract" else PROMPT
    new_tok = 256 if task == "extract" else 5
    trunc   = 1200 if task == "extract" else 800
    out, t0 = [], time.time()
    for i in range(0, len(df), bs):
        b = df.iloc[i:i+bs]; prompts = []
        for _, row in b.iterrows():
            msgs = [{"role":"system","content":sys_msg},
                    {"role":"user","content":usr_tpl % str(row["text"])[:trunc]}]
            prompts.append(tok.apply_chat_template(msgs, tokenize=False, add_generation_prompt=True))
        inp = tok(prompts, return_tensors="pt", padding=True, truncation=True, max_length=1536).to(mdl.device)
        with torch.no_grad():
            oid = mdl.generate(**inp, max_new_tokens=new_tok, do_sample=False, pad_token_id=tok.pad_token_id)
        ilen = inp["input_ids"].shape[1]
        for j, (_, row) in enumerate(b.iterrows()):
            raw = tok.decode(oid[j, ilen:], skip_special_tokens=True).strip()
            if task == "extract":
                out.append({"item_id": row["item_id"], "llm_label": parse_extract(raw),
                            "llm_score": None, "llm_raw": raw[:240]})
            else:
                out.append({"item_id": row["item_id"], "llm_label": parse_yn(raw),
                            "llm_score": None, "llm_raw": raw[:50]})
        print(f"  [{i+len(b)}/{len(df)}] {time.time()-t0:.0f}s", flush=True)
    return out

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", choices=["nli", "small", "med", "large"], default="nli")
    ap.add_argument("--task", choices=["flag", "extract"], default="flag")
    ap.add_argument("--input", default=os.path.join(ROOT, "out/validation/llm_validation_sample.csv"))
    ap.add_argument("--limit", type=int, default=0, help="cap items (0 = all); use to bound the extract task")
    ap.add_argument("--output", default=None)
    a = ap.parse_args()
    tag = a.model if a.task == "flag" else f"{a.model}_{a.task}"
    if a.output is None:
        a.output = os.path.join(ROOT, f"out/validation/llm_results_{tag}.csv")
    if "HF_HOME" not in os.environ:
        os.environ["HF_HOME"] = str(Path.home() / "hf_cache"); Path(os.environ["HF_HOME"]).mkdir(parents=True, exist_ok=True)
    print(f"=== LLM audit: {a.model} task={a.task} | in={a.input} out={a.output} | HF_HOME={os.environ['HF_HOME']}", flush=True)
    df = pd.read_csv(a.input)
    if "text" not in df.columns and "snippet_text" in df.columns: df["text"] = df["snippet_text"]
    if "item_id" not in df.columns and "snippet_id" in df.columns: df["item_id"] = df["snippet_id"]
    df = df[df["text"].notna() & (df["text"].astype(str).str.strip() != "")].reset_index(drop=True)
    if a.limit and a.limit < len(df): df = df.iloc[:a.limit].reset_index(drop=True)
    print(f"  {len(df)} items", flush=True)
    _t0 = time.time()
    if a.model == "nli":
        if a.task == "extract": raise SystemExit("nli does not support the extract task")
        res = run_nli(df)
    else:
        res = run_decoder(df, DECODERS[a.model], bs={"small": 8, "med": 2}.get(a.model, 1), task=a.task)
    write_timing(a.model, a.task, len(df), time.time()-_t0, 256 if a.task=="extract" else 5)
    outdf = df.merge(pd.DataFrame(res), on="item_id", how="left")
    Path(a.output).parent.mkdir(parents=True, exist_ok=True)
    outdf.to_csv(a.output, index=False)
    # quick precision among dict-flagged
    v = outdf[outdf["llm_label"].isin(["YES","NO"])].copy()
    v["dy"] = v["dictionary_flag"].astype(str).str.upper().isin(["TRUE","1"])
    v["ly"] = v["llm_label"] == "YES"
    tp = int(((v.dy)&(v.ly)).sum()); fp = int(((~v.dy)&(v.ly)).sum())
    fn = int(((v.dy)&(~v.ly)).sum())
    prec = tp/max(tp+fp,1); rec = tp/max(tp+fn,1)
    print(f"\n  precision={prec:.1%} recall={rec:.1%} agree={(v.dy==v.ly).mean():.1%} -> {a.output}", flush=True)

if __name__ == "__main__":
    main()
