#!/usr/bin/env python3
# ==============================================================================
# pipeline/python/04g_perturbation_measure.py   (Stage 4 — FAITHFUL perturbation)
#
# Sautner's actual robustness test: drop each seed, re-derive the dictionary,
# rebuild the EXPOSURE MEASURE, and require high correlation with the full measure
# (his bar > 0.85). 04b only compared bigram-SET overlap (stricter, tail-sensitive);
# this compares the thing that matters — Eq.1 exposure per firm.
#
# For each dropped seed i the dictionary = (50 seeds minus seed_i) U (dict_drop_i
# discovered, top-9600). We recompute per-call Eq.1 = count_dict / B, reusing the
# EXACT B (total bigrams/call) already stored by Stage 3 in exposure_calls.rds, so
# only the numerator (which bigrams are in the dict) varies. Then correlate each
# dropped measure with the locked measure at call AND firm-quarter level.
#
#   pipeline/python/.venv/bin/python pipeline/python/04g_perturbation_measure.py
# Output: out/dict/perturb/perturbation_measure.json
# ==============================================================================
import os, sys, glob, csv, json, unicodedata
import numpy as np, scipy.sparse as sp, pyarrow.parquet as pq, yaml
from collections import defaultdict
from sklearn.feature_extraction.text import CountVectorizer
from scipy.stats import spearmanr

def find_root():
    d = os.path.abspath(os.getcwd())
    while d not in ("/", ""):
        if os.path.exists(os.path.join(d, "pipeline/config/params.yml")): return d
        d = os.path.dirname(d)
    sys.exit("no root")
ROOT = find_root()
PERT = os.path.join(ROOT, "out", "dict", "perturb")
def strip(s): return "".join(c for c in unicodedata.normalize("NFKD", s.lower()) if not unicodedata.combining(c))
def log(m): print(m, flush=True)

# ---- dictionaries -----------------------------------------------------------
seeds = [strip(s) for s in yaml.safe_load(open(os.path.join(ROOT, "pipeline/config/seeds.yml")))["seeds"]]
locked_disc = []
with open(os.path.join(ROOT, "pipeline/config/dictionary_geoeconomic.csv")) as fh:
    for r in csv.DictReader(fh):
        if r["origin"] == "discovered": locked_disc.append(strip(r["bigram"]))
locked = set(seeds) | set(locked_disc)

drops = {}   # i -> set of bigrams = (seeds minus seed_i) U discovered_i
for fp in sorted(glob.glob(os.path.join(PERT, "dict_drop_*.csv"))):
    i = int("".join(filter(str.isdigit, os.path.basename(fp))))
    disc = []
    with open(fp) as fh:
        for r in csv.DictReader(fh): disc.append(strip(r["bigram"]))
    drops[i] = (set(seeds) - {seeds[i]}) | set(disc)
log(f"[measure] locked={len(locked):,} | dropped dicts={len(drops)} (each ~{len(next(iter(drops.values()))):,})")

vocab = sorted(locked.union(*drops.values()))
vidx = {b: k for k, b in enumerate(vocab)}
log(f"[measure] combined vocab={len(vocab):,}")

# ---- vectorise corpus, aggregate to call by Id (cached) ---------------------
cache_C = os.path.join(PERT, "_callcount.npz"); cache_ids = os.path.join(PERT, "_callcount_ids.npy")
cache_vocab = os.path.join(PERT, "_callcount_vocab.txt")
cached_vocab = open(cache_vocab).read().splitlines() if os.path.exists(cache_vocab) else None
if os.path.exists(cache_C) and cached_vocab == vocab:
    C = sp.load_npz(cache_C).tocsc(); call_ids = np.load(cache_ids, allow_pickle=True)
    log(f"[measure] loaded cached call-count matrix {C.shape}")
else:
    vec = CountVectorizer(analyzer="word", strip_accents="unicode", ngram_range=(2, 2),
                          lowercase=True, stop_words="english", vocabulary=vocab)
    call_ids, blocks = [], []
    for fp in sorted(glob.glob(os.path.join(ROOT, "out/corpus_parquet/sentences_*.parquet"))):
        t = pq.read_table(fp, columns=["Id", "text"])
        ids = t.column("Id").to_pylist(); txt = ["" if x is None else x for x in t.column("text").to_pylist()]
        X = vec.transform(txt)
        uniq, inv = np.unique(np.asarray(ids), return_inverse=True)
        G = sp.csr_matrix((np.ones(len(ids), np.int32), (inv, np.arange(len(ids)))),
                          shape=(len(uniq), len(ids)))
        blocks.append(G @ X); call_ids.extend(uniq.tolist())
        log(f"  {os.path.basename(fp)}: calls={len(uniq):,}")
    C = sp.vstack(blocks).tocsc(); call_ids = np.array(call_ids)
    sp.save_npz(cache_C, C.tocsr()); np.save(cache_ids, call_ids)
    open(cache_vocab, "w").write("\n".join(vocab))
    log(f"[measure] call-count matrix {C.shape} (cached)")

def dict_count(bigset):
    cols = [vidx[b] for b in bigset if b in vidx]
    return np.asarray(C[:, cols].sum(axis=1)).ravel()
cnt_locked = dict_count(locked)

# ---- join Stage-3 B per call (reuse exact denominator) ----------------------
import subprocess, tempfile
rds = os.path.join(ROOT, "out/exposure/exposure_calls.rds")
csvtmp = os.path.join(PERT, "_calls_B.csv")
if not os.path.exists(csvtmp):
    rcode = f'x<-readRDS("{rds}"); data.table::fwrite(x[,.(Id,B,ticker,year,quarter,GeoExposure)], "{csvtmp}")'
    subprocess.run(["Rscript", "-e", f'suppressMessages(library(data.table)); {rcode}'], check=True)
import pandas as pd
b = pd.read_csv(csvtmp, dtype={"Id": str}).drop_duplicates(subset="Id")   # Stage-3 panel has dup Ids
bmap = dict(zip(b.Id, b.B)); fqmap = b.set_index("Id")[["ticker","year","quarter"]].to_dict("index")
ids_str = call_ids.astype(str)
Bvec = np.array([bmap.get(i, np.nan) for i in ids_str], float)
ok = np.isfinite(Bvec) & (Bvec > 0)          # all matched calls (locked exposure may be 0)
log(f"[measure] calls matched with B>0: {ok.sum():,}/{len(ids_str):,}")

expo_locked = np.where(ok, cnt_locked / Bvec, np.nan)

# ---- per-seed correlation, call-level + firm-quarter ------------------------
def fq_aggregate(expo):
    agg = defaultdict(lambda: [0.0, 0])
    for k in np.where(ok)[0]:
        m = fqmap.get(ids_str[k]);
        if not m: continue
        key = (m["ticker"], m["year"], m["quarter"]); agg[key][0] += expo[k]; agg[key][1] += 1
    keys = list(agg); vals = np.array([agg[kk][0]/agg[kk][1] for kk in keys])
    return keys, vals
fq_keys_locked, fq_locked = fq_aggregate(expo_locked)
fq_index = {kk: j for j, kk in enumerate(fq_keys_locked)}

rows = []
for i in sorted(drops):
    cnt_i = dict_count(drops[i])
    expo_i = np.where(ok, cnt_i / Bvec, np.nan)
    m = ok
    pe = np.corrcoef(expo_locked[m], expo_i[m])[0, 1]
    sp_ = spearmanr(expo_locked[m], expo_i[m]).correlation
    # firm-quarter level (align on locked fq keys)
    _, fq_i = fq_aggregate(expo_i)
    pe_fq = np.corrcoef(fq_locked, fq_i)[0, 1] if len(fq_i) == len(fq_locked) else np.nan
    rows.append({"seed_idx": i, "pearson_call": round(float(pe), 4),
                 "spearman_call": round(float(sp_), 4), "pearson_fq": round(float(pe_fq), 4)})
    log(f"  drop {i:2d}: call r={pe:.3f} rho={sp_:.3f} | fq r={pe_fq:.3f}")

pc = np.array([r["pearson_call"] for r in rows]); pf = np.array([r["pearson_fq"] for r in rows])
summary = {"n_dropped": len(rows), "n_calls": int(ok.sum()),
           "pearson_call_mean": round(float(pc.mean()), 4), "pearson_call_min": round(float(pc.min()), 4),
           "pearson_fq_mean": round(float(np.nanmean(pf)), 4), "pearson_fq_min": round(float(np.nanmin(pf)), 4),
           "pass_call_0.85": bool((pc > 0.85).all()), "pass_fq_0.85": bool((np.nan_to_num(pf) > 0.85).all()),
           "per_seed": rows}
with open(os.path.join(PERT, "perturbation_measure.json"), "w") as fh: json.dump(summary, fh, indent=2)
log(f"\n[measure] CALL: mean r={summary['pearson_call_mean']} min={summary['pearson_call_min']} | "
    f"FQ: mean r={summary['pearson_fq_mean']} min={summary['pearson_fq_min']}")
log(f"[measure] PASS >0.85  call={summary['pass_call_0.85']}  fq={summary['pass_fq_0.85']}")
log("[measure] wrote out/dict/perturb/perturbation_measure.json")
