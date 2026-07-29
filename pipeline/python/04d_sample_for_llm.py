#!/usr/bin/env python3
# ==============================================================================
# pipeline/python/04d_sample_for_llm.py   (Stage 4 — snippet-audit sampler)
#
# Build a stratified sample to audit dictionary PRECISION (Sautner's validation
# analogue: human-coded precision, here adapted to an LLM judge in 04e). Uses the
# EXACT measure tokeniser (sklearn CountVectorizer, bigrams, strip-accents,
# lowercase, english stopwords) restricted to the locked dict vocabulary, so a
# sentence's "dict hit count" here == what Eq.1 counts. Flags every item with BOTH
# the primary (unpruned 9,650) and pruned (4,586) dicts for a side-by-side contrast.
#
# TWO parts (mirrors the legacy two-part design — chosen 2026-06-03):
#   Part A  CALL-level: 500 calls (250 geo / 250 non-geo). Excerpt = the call's
#           highest-dict-hit sentences concatenated to ~800 chars (geo) / random
#           sentences (non-geo). Gives the LLM real context -> fair precision.
#   Part B  SENTENCE-level: 700 single sentences, stratified by dict-hit count:
#           B1 >=2 bigrams / B2 =1 / B3 seed-word near-miss / B4 clean. Strict test.
#
# Output: out/validation/llm_validation_sample.csv (schema consumed by 04e):
#   item_id, validation_type, text, dictionary_flag, dictionary_flag_pruned,
#   n_bigrams, n_bigrams_pruned, exposure_score, Id, call_date, year_bin,
#   stratum, bigrams_found
#
#   pipeline/python/.venv/bin/python pipeline/python/04d_sample_for_llm.py
# ==============================================================================
import os, sys, csv, unicodedata
import numpy as np, pyarrow.parquet as pq, yaml
from collections import defaultdict
from sklearn.feature_extraction.text import CountVectorizer, ENGLISH_STOP_WORDS

def find_root():
    d = os.path.abspath(os.getcwd())
    while d not in ("/", ""):
        if os.path.exists(os.path.join(d, "pipeline", "config", "params.yml")): return d
        d = os.path.dirname(d)
    sys.exit("no root")
ROOT = find_root()
SEED = 20250401
rng = np.random.default_rng(SEED)

SAMPLE_YEARS = [2008, 2015, 2019, 2020, 2022, 2023]   # span GFC..Ukraine
PER_YEAR_POOL = 250_000                               # sentences/year into the Part-B pool
B_TARGETS = {"B1_high_confidence": 200, "B2_borderline": 150,
             "B3_near_miss": 150, "B4_clean_negative": 200}   # 700 sentence items
A_GEO_PER_YEAR = 42        # ~250 geo + ~250 non-geo calls across 6 years
A_NONGEO_PER_YEAR = 42
A_GEO_MIN_PRUNED = 2       # a "geo call" = >=2 curated(pruned) bigrams in the call
A_MAXCHARS, B_MAXCHARS = 800, 600
A_MAX_SENT = 6             # sentences concatenated into a call excerpt

def strip(s): return "".join(c for c in unicodedata.normalize("NFKD", s.lower()) if not unicodedata.combining(c))

# ---- locked dictionaries + seed unigrams ------------------------------------
def load_bigrams(path):
    out = []
    with open(path) as fh:
        for row in csv.DictReader(fh): out.append(strip(row["bigram"]))
    return sorted(set(out))
dict_bigrams = load_bigrams(os.path.join(ROOT, "pipeline/config/dictionary_geoeconomic.csv"))
pruned_bigrams = load_bigrams(os.path.join(ROOT, "pipeline/config/dictionary_geoeconomic_pruned.csv"))
seeds = yaml.safe_load(open(os.path.join(ROOT, "pipeline/config/seeds.yml")))["seeds"]
seed_units = sorted({w for s in seeds for w in strip(s).split()
                     if len(w) > 2 and w not in ENGLISH_STOP_WORDS})
print(f"[sample] primary bigrams={len(dict_bigrams):,} | pruned={len(pruned_bigrams):,} | "
      f"seed unigrams={len(seed_units)}", flush=True)

vd = CountVectorizer(analyzer="word", strip_accents="unicode", ngram_range=(2, 2),
                     lowercase=True, stop_words="english", vocabulary=dict_bigrams)
vp = CountVectorizer(analyzer="word", strip_accents="unicode", ngram_range=(2, 2),
                     lowercase=True, stop_words="english", vocabulary=pruned_bigrams)
vs = CountVectorizer(analyzer="word", strip_accents="unicode", ngram_range=(1, 1),
                     lowercase=True, vocabulary=seed_units)
dict_vocab = np.array(dict_bigrams)

def year_bin(y): return "pre_2018" if y < 2018 else ("2018_2021" if y <= 2021 else "post_2021")
def bigrams_for(Xd_csr, i, k=5):
    cols = Xd_csr.indices[Xd_csr.indptr[i]:Xd_csr.indptr[i+1]]
    return "|".join(dict_vocab[cols][:k]) if len(cols) else ""

# ---- per-year pass: one full vectorise serves both Part A and Part B --------
b_pool = []   # (text, year, nbg, npr, seed, bf)  for sentence strata
a_rows = []   # dict per call excerpt
for y in SAMPLE_YEARS:
    fp = os.path.join(ROOT, "out", "corpus_parquet", f"sentences_{y}.parquet")
    if not os.path.exists(fp): print(f"  WARN missing {fp}"); continue
    tbl = pq.read_table(fp, columns=["Id", "text"])
    ids_all = tbl.column("Id").to_pylist(); txt_all = tbl.column("text").to_pylist()
    keep = [i for i, t in enumerate(txt_all) if t and len(t) >= 40]
    ids = [ids_all[i] for i in keep]; texts = [txt_all[i] for i in keep]
    del ids_all, txt_all
    Xd = vd.transform(texts); Xp = vp.transform(texts); Xs = vs.transform(texts)
    nbg = np.asarray(Xd.sum(axis=1)).ravel().astype(int)
    npr = np.asarray(Xp.sum(axis=1)).ravel().astype(int)
    seedhit = np.asarray(Xs.sum(axis=1)).ravel() > 0
    Xd = Xd.tocsr()

    # --- Part B pool: random subsample of this year's sentences ---
    pick_b = rng.choice(len(texts), min(PER_YEAR_POOL, len(texts)), replace=False)
    for i in pick_b:
        b_pool.append((texts[i][:B_MAXCHARS], y, int(nbg[i]), int(npr[i]),
                       bool(seedhit[i]), bigrams_for(Xd, i)))

    # --- Part A: group sentence indices by call Id ---
    call_idx = defaultdict(list)
    for i, cid in enumerate(ids): call_idx[cid].append(i)
    geo_ids, nongeo_ids = [], []
    for cid, idxs in call_idx.items():
        cp = int(npr[idxs].sum()); cprim = int(nbg[idxs].sum())
        if cp >= A_GEO_MIN_PRUNED: geo_ids.append(cid)
        elif cprim == 0:           nongeo_ids.append(cid)
    rng.shuffle(geo_ids); rng.shuffle(nongeo_ids)

    def build_excerpt(cid, geo):
        idxs = np.array(call_idx[cid])
        if geo:
            order = idxs[np.argsort(-nbg[idxs])][:A_MAX_SENT]   # top dict-hit sentences
        else:
            order = idxs[rng.choice(len(idxs), min(A_MAX_SENT, len(idxs)), replace=False)]
        exc, used = "", []
        for j in order:
            exc = (exc + " | " + texts[j]).strip(" |")
            used.append(j)
            if len(exc) >= A_MAXCHARS: break
        bf = "|".join(sorted({b for j in used for b in bigrams_for(Xd, j).split("|") if b}))[:120]
        cp = int(npr[idxs].sum()); cprim = int(nbg[idxs].sum())
        return {"text": exc[:A_MAXCHARS], "year": y, "nbg": cprim, "npr": cp,
                "geo": geo, "bf": bf, "Id": cid,
                "dens": round(cprim / max(len(idxs), 1), 4)}
    for cid in geo_ids[:A_GEO_PER_YEAR]:    a_rows.append(build_excerpt(cid, True))
    for cid in nongeo_ids[:A_NONGEO_PER_YEAR]: a_rows.append(build_excerpt(cid, False))
    print(f"  {y}: sents={len(texts):,} primary-flagged={(nbg>0).sum():,} "
          f"pruned-flagged={(npr>0).sum():,} | calls geo={len(geo_ids):,} nongeo={len(nongeo_ids):,}",
          flush=True)
    del texts, ids, call_idx, Xd, Xp, Xs

def stratum_of(n, seed):
    if n >= 2: return "B1_high_confidence"
    if n == 1: return "B2_borderline"
    return "B3_near_miss" if seed else "B4_clean_negative"

strata = {k: [] for k in B_TARGETS}
for r in b_pool: strata[stratum_of(r[2], r[4])].append(r)

# ---- write combined sample --------------------------------------------------
out = os.path.join(ROOT, "out", "validation"); os.makedirs(out, exist_ok=True)
fp = os.path.join(out, "llm_validation_sample.csv")
COLS = ["item_id","validation_type","text","dictionary_flag","dictionary_flag_pruned",
        "n_bigrams","n_bigrams_pruned","exposure_score","Id","call_date","year_bin",
        "stratum","bigrams_found"]
na, nb = 0, 0
with open(fp, "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(COLS)
    # Part A
    rng.shuffle(a_rows)
    for r in a_rows:
        na += 1
        w.writerow([f"A_{na}", "A", r["text"], str(r["nbg"] >= 1), str(r["npr"] >= 1),
                    r["nbg"], r["npr"], r["dens"], r["Id"], "", year_bin(r["year"]),
                    "A_geo_call" if r["geo"] else "A_nongeo_call", r["bf"]])
    print(f"  Part A: {na} calls (geo={sum(r['geo'] for r in a_rows)})", flush=True)
    # Part B
    for s, tgt in B_TARGETS.items():
        pool = strata[s]; k = min(len(pool), tgt)
        pick = [pool[i] for i in rng.choice(len(pool), k, replace=False)] if pool else []
        print(f"  {s}: have {len(pool):,} -> sampled {k}", flush=True)
        for t, y, nbg_, npr_, seed, bf in pick:
            nb += 1
            w.writerow([f"B_{nb}", "B", t, str(nbg_ >= 1), str(npr_ >= 1), nbg_, npr_,
                        "", "", "", year_bin(y), s, bf])
print(f"[sample] wrote {na} Part-A + {nb} Part-B = {na+nb} items -> {fp}", flush=True)
