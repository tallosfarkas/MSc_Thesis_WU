#!/usr/bin/env python3
# ==============================================================================
# pipeline/python/04_perturbation.py
#
# STAGE 4 — leave-one-seed perturbation (Sautner JF p.1468 robustness: drop each
# initial seed, re-derive the dictionary, require high correlation with the full
# version -> the measure does not hinge on any single seed).
#
# Mirrors the LOCKED sautner discovery (pipeline/python/02b_klr_discovery.py):
# same CountVectorizer, same R=seed-sentences / S split, same NB + calibrated
# LinearSVC + RandomForest ensemble at P(R)>0.8, same signed-G2 mining. Hyper-
# parameters are PINNED to the locked run's best (alpha=1, C=1, RF n=100/sqrt) so
# the only thing that varies is the dropped seed (and to avoid 50x grid-search).
#
# Efficiency: vectorise ONCE per job, then loop over a RANGE of dropped seeds
# (KLR_PERTURB_RANGE="lo-hi", 0-based) so a small SLURM array covers all 50
# without recomputing the 45-min vectoriser 50x.
#
# Output per dropped seed i: out/dict/perturb/dict_drop_<i>.csv (top-K discovered
# bigrams by G2). 04b_perturbation_corr.R then correlates each with the full lock.
#
#   KLR_PERTURB_RANGE=0-9  python pipeline/python/04_perturbation.py
# ==============================================================================
import os, sys, gc, time, glob, unicodedata, csv
import numpy as np, scipy.sparse as sp, pyarrow.parquet as pq, yaml
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.svm import LinearSVC
from sklearn.calibration import CalibratedClassifierCV
from sklearn.ensemble import RandomForestClassifier
from scipy.stats import chi2 as _chi2

T0 = time.time()
def log(m, *a): print(f"[{time.time()-T0:8.1f}s] " + (m % a if a else m), flush=True)

def find_root():
    d = os.path.abspath(os.getcwd())
    while d not in ("/", ""):
        if os.path.exists(os.path.join(d, "pipeline", "config", "params.yml")): return d
        d = os.path.dirname(d)
    sys.exit("no root")
ROOT = find_root()
N_CORES = int(os.environ.get("SLURM_CPUS_PER_TASK", os.cpu_count() or 8))
P = yaml.safe_load(open(os.path.join(ROOT, "pipeline/config/params.yml")))
SEED = int(P["klr"]["seed"]); P_THR = float(P["klr"]["p_threshold"])
REF = int(P["klr"]["reference_sample_size"]); CAP = int(P["klr"]["model_vocab_size"])
TOPK = 9600  # match the locked primary's discovered count for a like-for-like compare
SEEDS = list(yaml.safe_load(open(os.path.join(ROOT, "pipeline/config/seeds.yml")))["seeds"])
OUT = os.path.join(ROOT, "out", "dict", "perturb"); os.makedirs(OUT, exist_ok=True)

lo, hi = (int(x) for x in os.environ.get("KLR_PERTURB_RANGE", f"0-{len(SEEDS)-1}").split("-"))
log("perturbation seeds [%d..%d] of %d | cores=%d", lo, hi, len(SEEDS), N_CORES)

# ---- vectorise ONCE (Sautner exact config, no trim) -------------------------
PQ = os.path.join(ROOT, "out", "corpus_parquet")
texts, ids = [], []
for fp in sorted(glob.glob(os.path.join(PQ, "sentences_*.parquet"))):
    t = pq.read_table(fp, columns=["text"]); texts.extend(t.column("text").to_pylist())
texts = ["" if x is None else x for x in texts]
log("loaded %s sentences; vectorising ...", f"{len(texts):,}")
vec = CountVectorizer(analyzer="word", strip_accents="unicode", ngram_range=(2, 2),
                      lowercase=True, stop_words="english", min_df=1)
X = vec.fit_transform(texts); del texts; gc.collect()
vocab = vec.get_feature_names_out(); vidx = {b: i for i, b in enumerate(vocab)}
col_freq = np.asarray(X.sum(axis=0)).ravel().astype(np.int64)
top_cols = np.argsort(col_freq)[::-1][:CAP]
log("X = %s x %s (nnz=%s)", f"{X.shape[0]:,}", f"{X.shape[1]:,}", f"{X.nnz:,}")

def strip(s): return "".join(c for c in unicodedata.normalize("NFKD", s.lower()) if not unicodedata.combining(c))
seeds_norm = [strip(s) for s in SEEDS]
rng = np.random.default_rng(SEED)
A = np.asarray(col_freq, float)  # totals reused in G2

def derive(drop_i):
    """Full faithful re-derivation with seed `drop_i` removed; returns top-K bigrams."""
    keep = [s for j, s in enumerate(seeds_norm) if j != drop_i]
    scols = np.array([vidx[s] for s in keep if s in vidx], dtype=np.int64)
    isR = np.asarray(X[:, scols].sum(axis=1)).ravel() > 0
    r_idx = np.where(isR)[0]; s_idx = np.where(~isR)[0]
    s_tr = rng.choice(s_idx, size=min(REF, s_idx.size), replace=False)
    tr = np.concatenate([r_idx, s_tr]); y = np.concatenate([np.ones(r_idx.size, np.int8), np.zeros(s_tr.size, np.int8)])
    Xtr = X[tr]
    rf_cols = np.union1d(top_cols, scols); Xtr_rf = Xtr[:, rf_cols]
    XS = X[s_idx]; XS_rf = XS[:, rf_cols]
    Rc = lambda c: int(np.where(c.classes_ == 1)[0][0])
    nb = MultinomialNB(alpha=1.0).fit(Xtr, y)
    sv = CalibratedClassifierCV(LinearSVC(C=1.0, dual=True, max_iter=5000), method="sigmoid", cv=5).fit(Xtr, y)
    rf = RandomForestClassifier(n_estimators=100, max_features="sqrt", random_state=SEED, n_jobs=N_CORES).fit(Xtr_rf, y)
    p_nb = nb.predict_proba(XS)[:, Rc(nb)]; p_sv = sv.predict_proba(XS)[:, Rc(sv)]
    p_rf = np.empty(XS_rf.shape[0], np.float32)
    for st in range(0, XS_rf.shape[0], 2_000_000):
        en = min(st + 2_000_000, XS_rf.shape[0]); p_rf[st:en] = rf.predict_proba(XS_rf[st:en])[:, Rc(rf)]
    inT = (p_nb > P_THR) | (p_sv > P_THR) | (p_rf > P_THR)
    tgt = isR.copy(); tgt[s_idx[inT]] = True
    st = np.asarray(X[tgt].sum(axis=0)).ravel().astype(float); sn = A - st
    At, Ct = st.sum(), sn.sum(); Nt = At + Ct
    a, c = st, sn; b, d = At - a, Ct - c
    with np.errstate(divide="ignore", invalid="ignore"):
        def term(o, e):
            o2 = np.zeros_like(o); m = (o > 0) & (e > 0); o2[m] = o[m]*np.log(o[m]/e[m]); return o2
        G2 = 2*(term(a, (a+c)*At/Nt)+term(b, (a+b)*(b+d)/Nt)+term(c, (c+d)*(a+c)/Nt)+term(d, (c+d)*(b+d)/Nt))
    G2 *= np.where((a/At) >= (c/Ct), 1.0, -1.0)
    keep_mask = (st > 0) & ~np.isin(np.arange(len(vocab)), scols)  # exclude the seeds themselves
    order = np.argsort(-G2[keep_mask]); idxs = np.where(keep_mask)[0][order][:TOPK]
    return [(vocab[j], float(G2[j])) for j in idxs]

for i in range(lo, hi + 1):
    t = time.time(); rows = derive(i)
    with open(os.path.join(OUT, f"dict_drop_{i}.csv"), "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["bigram", "G2"]); w.writerows(rows)
    log("  dropped seed %d (%s): %d bigrams (%.1f min)", i, SEEDS[i], len(rows), (time.time()-t)/60)
log("=== perturbation range [%d..%d] DONE ===", lo, hi)
