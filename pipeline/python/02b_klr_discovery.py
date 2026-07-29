#!/usr/bin/env python3
# ==============================================================================
# pipeline/python/02b_klr_discovery.py
#
# Sautner-faithful KLR keyword discovery, GENUINE sklearn implementation.
# The parity counterpart to pipeline/R/02b_klr_discovery.R.
#
# Sautner et al. (2023) released only the final climate dictionaries (pickles)
# and the scoring code -- NOT the discovery training code. So this mirrors the
# Online Appendix (p. 2-3) prose step by step, using the real sklearn estimators
# Sautner names. Where the R pipeline had to deviate for memory reasons (logit
# instead of LinearSVC; min_docfreq=5 trim; all-three capped to 10k features),
# this Python version stays closer to Sautner:
#
#   STEP                              Sautner / this .py                R 02b deviation
#   --------------------------------- --------------------------------- -------------------
#   tokenise                          CountVectorizer EXACT line        sklearn-mirror in R
#   vocab trim                        none (Sautner: no min_df)         min_docfreq=5
#   reference set R                   sentences with >=1 seed bigram    same
#   search set S                      the rest                          same
#   training set                      R + random 100k of S              same
#   classifier 1                      MultinomialNB                     same (quanteda nb)
#   classifier 2                      LinearSVC (Platt-calibrated)      LOGIT (LiblineaR t0)
#   classifier 3                      RandomForestClassifier            ranger
#   tuning                            GridSearchCV(cv=5)                pinned hyperparams
#   feature space NB/SVC              FULL vocab (sparse-native)        capped 10k
#   feature space RF                  top-K capped (RF can't do full)   capped 10k (same)
#   ensemble                          any classifier P(R) > 0.80        same
#   mine bigrams                      target (R u T) vs non-target      same
#   rank                              signed G2 likelihood ratio        same (quanteda "lr")
#
# Outputs carry a _py suffix and live in out/dict/ beside the R artefacts.
#
# Run (cluster):  pipeline/python/.venv/bin/python pipeline/python/02b_klr_discovery.py
# Env overrides:  RF_VOCAB_SIZE=50000  (RF feature cap)
# ==============================================================================
import os, sys, gc, json, glob, time, unicodedata
import numpy as np
import scipy.sparse as sp
import pyarrow.parquet as pq
import yaml

from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.svm import LinearSVC
from sklearn.calibration import CalibratedClassifierCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV

T_START = time.time()
def log(msg, *a):
    el = time.time() - T_START
    print(f"[{el:8.1f}s] " + (msg % a if a else msg), flush=True)

# ---- Resolve project root ----------------------------------------------------
def find_root():
    d = os.path.abspath(os.getcwd())
    while d not in ("/", ""):
        if os.path.exists(os.path.join(d, "pipeline", "config", "params.yml")):
            return d
        d = os.path.dirname(d)
    sys.exit("Could not locate project root.")
ROOT = find_root()

N_CORES = int(os.environ.get("SLURM_CPUS_PER_TASK", os.cpu_count() or 8))

# ---- Hyperparameters (same params.yml the R pipeline reads) ------------------
with open(os.path.join(ROOT, "pipeline", "config", "params.yml")) as f:
    P = yaml.safe_load(f)
SEED            = int(P["klr"]["seed"])                  # 42
P_THRESHOLD     = float(P["klr"]["p_threshold"])         # 0.80
REF_SAMPLE_SIZE = int(P["klr"]["reference_sample_size"]) # 100,000
CURATE_TOP_K    = int(P["klr"]["curate_top_k"])          # 500
CV_FOLDS        = int(P["klr"]["cv_folds"])              # 5
MODEL_VOCAB_SIZE = int(os.environ.get("RF_VOCAB_SIZE",
                                      P["klr"]["model_vocab_size"]))  # RF cap (10k)

# ---- Run mode ----------------------------------------------------------------
#   sautner  (default): faithful to the Online Appendix -- LinearSVC (calibrated),
#            GridSearchCV cv=5, NO vocab trim, NB+SVC on the FULL bigram vocab.
#   rparity            : mirror pipeline/R/02b_klr_discovery.R EXACTLY -- logistic
#            regression instead of SVC, min_df=5 trim, all three classifiers on
#            the capped 10k vocab, pinned hyperparameters (no grid search).
# The two together decompose language/library effect (rparity-py vs R) from
# method effect (sautner vs rparity).
MODE = os.environ.get("KLR_MODE", "sautner").strip().lower()
if MODE not in ("sautner", "rparity"):
    sys.exit(f"KLR_MODE must be 'sautner' or 'rparity', got {MODE!r}")
SUFFIX = "_py" if MODE == "sautner" else "_pyr"
# Robustness: calibrate NB+RF (Platt/sigmoid) like SVC so the P(R)>0.8 vote is a
# genuine 3-way consensus, not NB-overconfidence. Writes *_cal so it never
# clobbers the locked run. Env KLR_CALIBRATE=1 (sautner mode only).
CALIBRATE = os.environ.get("KLR_CALIBRATE", "0") == "1" and MODE == "sautner"
if CALIBRATE:
    SUFFIX += "_cal"
MIN_DF = 1 if MODE == "sautner" else int(P["klr"].get("trim_min_docfreq", 5))
CLF2 = "linearsvc_calibrated" if MODE == "sautner" else "logistic_regression"
CAP_ALL = (MODE == "rparity")   # rparity: cap ALL three classifiers to rf_cols

with open(os.path.join(ROOT, "pipeline", "config", "seeds.yml")) as f:
    SEEDS = list(yaml.safe_load(f)["seeds"])

DICT_OUT = os.path.join(ROOT, "out", "dict")
os.makedirs(DICT_OUT, exist_ok=True)
PQ_DIR = os.path.join(ROOT, "out", "corpus_parquet")

log("STAGE 2b (PYTHON / sklearn) -- Sautner KLR keyword discovery  [MODE=%s]", MODE)
log("  cores=%d  P_threshold=%.2f  ref_sample=%d  CV_folds=%d  top_K=%d  RF_cap=%d",
    N_CORES, P_THRESHOLD, REF_SAMPLE_SIZE, CV_FOLDS, CURATE_TOP_K, MODEL_VOCAB_SIZE)
log("  min_df=%d  clf2=%s  cap_all_three=%s  suffix=%s", MIN_DF, CLF2, CAP_ALL, SUFFIX)
log("  sklearn %s  seeds=%d", __import__("sklearn").__version__, len(SEEDS))

audit = dict(
    implementation="python-sklearn",
    mode=MODE, clf2_type=CLF2, min_df=MIN_DF, cap_all_three=CAP_ALL,
    hyperparams=dict(seed=SEED, p_threshold=P_THRESHOLD,
                     ref_sample_size=REF_SAMPLE_SIZE, cv_folds=CV_FOLDS,
                     curate_top_k=CURATE_TOP_K, rf_vocab_cap=MODEL_VOCAB_SIZE),
    cores=N_CORES, n_seeds=len(SEEDS),
    started_at=time.strftime("%Y-%m-%d %H:%M:%S"))

# ============================================================================
# 1. LOAD all sentence text from parquet (year, Id, sentence_id, text)
# ============================================================================
files = sorted(glob.glob(os.path.join(PQ_DIR, "sentences_*.parquet")))
if not files:
    sys.exit(f"No parquet in {PQ_DIR}. Run pipeline/R/02p_export_sentences.R first.")
log("[1/9] Loading %d parquet year-files ...", len(files))

texts, years, ids, sids = [], [], [], []
for fp in files:
    t = pq.read_table(fp, columns=["year", "Id", "sentence_id", "text"])
    texts.extend(t.column("text").to_pylist())
    years.append(t.column("year").to_numpy())
    ids.append(t.column("Id").to_numpy())
    sids.append(t.column("sentence_id").to_numpy())
    log("    %s: %s sentences", os.path.basename(fp), f"{t.num_rows:,}")
years = np.concatenate(years); ids = np.concatenate(ids); sids = np.concatenate(sids)
# guard against None text
texts = ["" if x is None else x for x in texts]
N_TOTAL = len(texts)
audit["n_sentences_total"] = N_TOTAL
log("    TOTAL sentences: %s", f"{N_TOTAL:,}")

# ============================================================================
# 2. VECTORISE -- Sautner's EXACT CountVectorizer line. No min_df (faithful).
# ============================================================================
log("[2/9] CountVectorizer fit_transform (Sautner's exact config, min_df=%d) ...", MIN_DF)
t0 = time.time()
vec = CountVectorizer(analyzer="word", strip_accents="unicode",
                      ngram_range=(2, 2), lowercase=True, stop_words="english",
                      min_df=MIN_DF)
X = vec.fit_transform(texts)          # csr (N x V), dtype int64
del texts; gc.collect()
vocab = vec.get_feature_names_out()   # array[str], length V
V = X.shape[1]
audit["dfm_n_docs"] = int(X.shape[0])
audit["dfm_n_features"] = int(V)
log("    X = %s docs x %s features  (%.1f min, nnz=%s)",
    f"{X.shape[0]:,}", f"{V:,}", (time.time()-t0)/60, f"{X.nnz:,}")

# ============================================================================
# 3. LABEL R (target) vs S (search) via seed-bigram columns
# ============================================================================
log("[3/9] Labelling target sentences (R) ...")
vocab_index = {b: i for i, b in enumerate(vocab)}
# CountVectorizer lowercased AND accent-stripped (strip_accents='unicode') the
# corpus, so normalise seeds identically for the lookup — else an accented seed
# would silently fail to match. Mirror sklearn 'unicode': NFKD + drop combining.
def _strip_accents(s):
    return "".join(c for c in unicodedata.normalize("NFKD", s.lower())
                   if not unicodedata.combining(c))
seeds_norm = [_strip_accents(s) for s in SEEDS]
n_tok = [len(s.split()) for s in seeds_norm]
non_bigram = [s for s, k in zip(seeds_norm, n_tok) if k != 2]
if non_bigram:
    log("    WARNING: %d seed(s) are not bigrams: %s", len(non_bigram), " | ".join(non_bigram))
seed_cols = np.array([vocab_index[s] for s in seeds_norm if s in vocab_index], dtype=np.int64)
present_seeds = [s for s in seeds_norm if s in vocab_index]
missing_seeds = [s for s in seeds_norm if s not in vocab_index]
audit["n_seeds_in_dfm"] = len(present_seeds)
audit["missing_seeds"] = missing_seeds
audit["n_seeds_non_bigram"] = len(non_bigram)
log("    %d seeds total, %d present, %d missing", len(SEEDS), len(present_seeds), len(missing_seeds))
if missing_seeds:
    log("    missing: %s", " | ".join(missing_seeds))

seed_hits = np.asarray(X[:, seed_cols].sum(axis=1)).ravel()
is_R = seed_hits > 0
r_idx = np.where(is_R)[0]
s_idx_all = np.where(~is_R)[0]
n_R, n_S = int(r_idx.size), int(s_idx_all.size)
audit.update(n_R=n_R, n_S=n_S, R_share_pct=round(100*n_R/N_TOTAL, 4))
log("    R (target): %s (%.4f%%)   S (search): %s", f"{n_R:,}", audit["R_share_pct"], f"{n_S:,}")

# ============================================================================
# 4. TRAINING SET = R + random 100k sample of S (Sautner OA p.2)
# ============================================================================
log("[4/9] Sampling %d from S for training ...", REF_SAMPLE_SIZE)
rng = np.random.default_rng(SEED)
s_train = rng.choice(s_idx_all, size=min(REF_SAMPLE_SIZE, n_S), replace=False)
train_idx = np.concatenate([r_idx, s_train])
y_train = np.concatenate([np.ones(r_idx.size, dtype=np.int8),
                          np.zeros(s_train.size, dtype=np.int8)])   # 1 = R
audit["n_train_R"], audit["n_train_S"] = int(r_idx.size), int(s_train.size)
log("    training set: %d (R=%d, S_sample=%d)", train_idx.size, r_idx.size, s_train.size)
X_train = X[train_idx]                      # full-vocab sparse, for NB + SVC

# RF feature cap: top-K bigrams by total frequency + seeds forced in
log("[4b/9] RF model vocab = top %s bigrams + seeds ...", f"{MODEL_VOCAB_SIZE:,}")
col_freq = np.asarray(X.sum(axis=0)).ravel().astype(np.int64)
top_cols = np.argsort(col_freq)[::-1][:MODEL_VOCAB_SIZE]
rf_cols = np.union1d(top_cols, seed_cols)
audit["rf_model_vocab_size"] = int(rf_cols.size)
audit["full_vocab_size"] = int(V)
log("    RF vocab: %s features (%d seeds forced in) | full vocab %s",
    f"{rf_cols.size:,}", len(present_seeds), f"{V:,}")
X_train_rf = X_train[:, rf_cols]

# In rparity mode ALL THREE classifiers train on the capped vocab (mirror R);
# in sautner mode NB + clf2 use the full vocab, only RF is capped.
X_train_c2 = X_train_rf if CAP_ALL else X_train     # NB + clf2 training matrix

# ============================================================================
# 5. FIT three classifiers  (Sautner OA p.2-3)
# ============================================================================
log("[5/9] Fitting three classifiers [mode=%s] ...", MODE)

log("    [5a] MultinomialNB ...")
t0 = time.time()
if MODE == "sautner":
    nb_gs = GridSearchCV(MultinomialNB(), {"alpha": [0.1, 0.5, 1.0]},
                         cv=CV_FOLDS, scoring="roc_auc", n_jobs=N_CORES)
    nb_gs.fit(X_train_c2, y_train)
    m_nb = nb_gs.best_estimator_
    audit["nb_best"] = nb_gs.best_params_
    if CALIBRATE:
        m_nb = CalibratedClassifierCV(MultinomialNB(**nb_gs.best_params_),
                                      method="sigmoid", cv=CV_FOLDS).fit(X_train_c2, y_train)
        log("        NB CALIBRATED (sigmoid)")
    log("        NB best=%s  cv_auc=%.4f  (%.1f sec)", nb_gs.best_params_,
        nb_gs.best_score_, time.time()-t0)
else:   # rparity: pinned alpha=1.0 (quanteda smooth=1), no grid
    m_nb = MultinomialNB(alpha=1.0).fit(X_train_c2, y_train)
    audit["nb_best"] = {"alpha": 1.0}
    log("        NB pinned alpha=1.0  (%.1f sec)", time.time()-t0)

log("    [5b] Classifier 2 = %s ...", CLF2)
t0 = time.time()
if MODE == "sautner":
    svc_gs = GridSearchCV(LinearSVC(dual=True, max_iter=5000), {"C": [0.1, 1.0, 10.0]},
                          cv=CV_FOLDS, scoring="roc_auc", n_jobs=N_CORES)
    svc_gs.fit(X_train_c2, y_train)
    bestC = svc_gs.best_params_["C"]
    # LinearSVC has no predict_proba; Platt-scale (sigmoid) via CV for P(R)>0.8.
    m_c2 = CalibratedClassifierCV(LinearSVC(C=bestC, dual=True, max_iter=5000),
                                  method="sigmoid", cv=CV_FOLDS)
    m_c2.fit(X_train_c2, y_train)
    audit["clf2_best"] = {"C": bestC}
    log("        SVC best C=%s  cv_auc=%.4f  + calibrated  (%.1f sec)",
        bestC, svc_gs.best_score_, time.time()-t0)
else:   # rparity: L2 logistic regression, C=1 (mirror LiblineaR type=0), pinned
    from sklearn.linear_model import LogisticRegression
    m_c2 = LogisticRegression(C=1.0, penalty="l2", solver="liblinear", max_iter=1000)
    m_c2.fit(X_train_c2, y_train)
    audit["clf2_best"] = {"C": 1.0, "penalty": "l2"}
    log("        Logistic regression pinned C=1.0  (%.1f sec)", time.time()-t0)

log("    [5c] RandomForest (n_estimators=100), sparse fit ...")
t0 = time.time()
if MODE == "sautner":
    rf_gs = GridSearchCV(
        RandomForestClassifier(n_estimators=100, random_state=SEED, n_jobs=N_CORES),
        {"max_features": ["sqrt"]},   # minimal grid: RF gridsearch is the costliest step
        cv=CV_FOLDS, scoring="roc_auc", n_jobs=1)
    rf_gs.fit(X_train_rf, y_train)
    m_rf = rf_gs.best_estimator_
    audit["rf_best"] = rf_gs.best_params_
    if CALIBRATE:
        m_rf = CalibratedClassifierCV(
            RandomForestClassifier(n_estimators=100, random_state=SEED, n_jobs=N_CORES,
                                   **rf_gs.best_params_),
            method="sigmoid", cv=CV_FOLDS).fit(X_train_rf, y_train)
        log("        RF CALIBRATED (sigmoid)")
    # Documented deviation: RF grid is a single point (max_features=sqrt) because
    # RF grid-search is the costliest step; NB and SVC do search the full grid.
    audit["rf_grid_note"] = "single-point max_features=sqrt (cost; deliberate)"
    log("        RF best=%s  cv_auc=%.4f  (%.1f min)", rf_gs.best_params_,
        rf_gs.best_score_, (time.time()-t0)/60)
else:   # rparity: pinned n_estimators=100 (ranger num.trees=100), no grid
    m_rf = RandomForestClassifier(n_estimators=100, random_state=SEED,
                                  n_jobs=N_CORES).fit(X_train_rf, y_train)
    audit["rf_best"] = {"n_estimators": 100}
    log("        RF pinned n_estimators=100  (%.1f min)", (time.time()-t0)/60)

def Rcol(clf):
    return int(np.where(clf.classes_ == 1)[0][0])

# ============================================================================
# 6. PREDICT P(R) for every sentence in S
# ============================================================================
log("[6/9] Predicting P(R) on all %s sentences in S ...", f"{n_S:,}")
X_S = X[s_idx_all]
X_S_c2 = X_S[:, rf_cols] if CAP_ALL else X_S   # NB + clf2 predict matrix

log("    [6a] NB predict ...")
t0 = time.time()
p_nb = m_nb.predict_proba(X_S_c2)[:, Rcol(m_nb)]
log("        NB predict: %.1f min", (time.time()-t0)/60)

log("    [6b] %s predict ...", CLF2)
t0 = time.time()
p_c2 = m_c2.predict_proba(X_S_c2)[:, Rcol(m_c2)]
log("        clf2 predict: %.1f min", (time.time()-t0)/60)

log("    [6c] RF predict (chunked, n_jobs=%d) ...", N_CORES)
t0 = time.time()
X_S_rf = X_S[:, rf_cols]
rf_R = Rcol(m_rf)
CHUNK = 2_000_000
p_rf = np.empty(n_S, dtype=np.float32)
for start in range(0, n_S, CHUNK):
    end = min(start + CHUNK, n_S)
    p_rf[start:end] = m_rf.predict_proba(X_S_rf[start:end])[:, rf_R]
    if (start // CHUNK) % 5 == 0:
        log("        ... %s / %s  (%.1f min)", f"{end:,}", f"{n_S:,}", (time.time()-t0)/60)
log("        RF predict: %.1f min", (time.time()-t0)/60)

# ============================================================================
# 7. ENSEMBLE vote -> T
# ============================================================================
log("[7/9] Ensemble vote -- sentence enters T if max P(R) > %.2f ...", P_THRESHOLD)
v_nb, v_c2, v_rf = p_nb > P_THRESHOLD, p_c2 > P_THRESHOLD, p_rf > P_THRESHOLD
in_T = v_nb | v_c2 | v_rf
n_T = int(in_T.sum())
audit.update(n_T=n_T, T_share_of_S_pct=round(100*n_T/n_S, 4),
             n_T_nb=int(v_nb.sum()), n_T_c2=int(v_c2.sum()), n_T_rf=int(v_rf.sum()))
log("    T: %s (%.4f%% of S)   NB=%s %s=%s RF=%s", f"{n_T:,}",
    audit["T_share_of_S_pct"], f"{audit['n_T_nb']:,}",
    CLF2, f"{audit['n_T_c2']:,}", f"{audit['n_T_rf']:,}")

target_mask = is_R.copy()
target_mask[s_idx_all[in_T]] = True
audit["n_target_total"] = int(target_mask.sum())
log("    R u T = %s target sentences", f"{audit['n_target_total']:,}")

# ============================================================================
# 8. MINE bigrams: signed G2 likelihood ratio, target vs non-target
# ============================================================================
log("[8/9] Mining bigrams: target (R u T) vs non-target (S\\T) ...")
sums_target = np.asarray(X[target_mask].sum(axis=0)).ravel().astype(np.float64)
sums_total  = col_freq.astype(np.float64)
sums_nontarget = sums_total - sums_target

A = sums_target.sum(); C = sums_nontarget.sum(); Ntot = A + C
a = sums_target; c = sums_nontarget
b = A - a; d = C - c
# expected counts under independence
with np.errstate(divide="ignore", invalid="ignore"):
    Ea = (a + c) * A / Ntot; Eb = (a + b) * (b + d) / Ntot
    Ec = (c + d) * (a + c) / Ntot; Ed = (c + d) * (b + d) / Ntot
    def term(o, e):
        out = np.zeros_like(o)
        m = (o > 0) & (e > 0)
        out[m] = o[m] * np.log(o[m] / e[m])
        return out
    G2 = 2.0 * (term(a, Ea) + term(b, Eb) + term(c, Ec) + term(d, Ed))
# sign: positive when feature is over-represented in target
sign = np.where((a / A) >= (c / C), 1.0, -1.0)
G2 = G2 * sign
# chi-sq(1 df) p-value
from scipy.stats import chi2 as _chi2
pval = _chi2.sf(np.abs(G2), df=1)

# document frequencies (binary occurrence)
Xt_bin = (X[target_mask] > 0)
df_target = np.asarray(Xt_bin.sum(axis=0)).ravel().astype(np.int64)
target_share = np.divide(sums_target, sums_total, out=np.zeros_like(sums_target),
                         where=sums_total > 0)

keep = sums_target > 0
order = np.argsort(-G2[keep])
ki = np.where(keep)[0][order]
audit["n_candidate_bigrams"] = int(ki.size)
audit["n_sig_bonferroni"] = int((pval[ki] < 0.05 / ki.size).sum())
audit["n_target_only"] = int((sums_nontarget[ki] == 0).sum())
log("    %s candidate bigrams | Bonferroni-sig %s | target-only %s",
    f"{ki.size:,}", f"{audit['n_sig_bonferroni']:,}", f"{audit['n_target_only']:,}")

# ============================================================================
# 9. SAVE outputs (_py suffix, beside the R artefacts)
# ============================================================================
log("[9/9] Writing outputs ...")
import csv
def write_keyness(path, idxs):
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["bigram", "G2", "p", "target_freq", "nontarget_freq",
                    "target_docfreq", "target_share"])
        for j in idxs:
            w.writerow([vocab[j], f"{G2[j]:.6f}", f"{pval[j]:.3e}",
                        int(sums_target[j]), int(sums_nontarget[j]),
                        int(df_target[j]), f"{target_share[j]:.6f}"])

write_keyness(os.path.join(DICT_OUT, f"keyness_all{SUFFIX}.csv"), ki)
write_keyness(os.path.join(DICT_OUT, f"top_candidates_browse{SUFFIX}.csv"), ki[:2000])

# dictionary_v1{SUFFIX}.csv = seeds + top-K discovered
with open(os.path.join(DICT_OUT, f"dictionary_v1{SUFFIX}.csv"), "w", newline="") as fh:
    w = csv.writer(fh)
    w.writerow(["bigram", "origin", "target_freq", "nontarget_freq", "G2", "p", "target_share"])
    for s in SEEDS:
        w.writerow([s, "seed", "", "", "", "", ""])
    for j in ki[:CURATE_TOP_K]:
        w.writerow([vocab[j], "discovered", int(sums_target[j]), int(sums_nontarget[j]),
                    f"{G2[j]:.6f}", f"{pval[j]:.3e}", f"{target_share[j]:.6f}"])

audit["ended_at"] = time.strftime("%Y-%m-%d %H:%M:%S")
with open(os.path.join(DICT_OUT, f"klr_audit{SUFFIX}.json"), "w") as fh:
    json.dump(audit, fh, indent=2)

# classifier predictions for traceability + R-vs-Python comparison
np.savez_compressed(
    os.path.join(DICT_OUT, f"classifier_predictions{SUFFIX}.npz"),
    year=years[s_idx_all], Id=ids[s_idx_all], sentence_id=sids[s_idx_all],
    p_nb=p_nb.astype(np.float32), p_c2=p_c2.astype(np.float32),
    p_rf=p_rf, in_T=in_T)

log("    dictionary_v1%s.csv | keyness_all%s.csv | top_candidates_browse%s.csv | klr_audit%s.json",
    SUFFIX, SUFFIX, SUFFIX, SUFFIX)
log("=== STAGE 2b PYTHON DONE [mode=%s] ===  candidates=%s  target-only=%s",
    MODE, f"{audit['n_candidate_bigrams']:,}", f"{audit['n_target_only']:,}")
