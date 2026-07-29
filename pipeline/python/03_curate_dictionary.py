#!/usr/bin/env python3
# ==============================================================================
# pipeline/python/03_curate_dictionary.py
#
# Face-validity curation of a discovered KLR dictionary (Sautner OA p.3 "manual
# top-N review"). Stdlib-only — runs with plain `python3` on the Mac, no venv.
#
# Two phases so YOU stay in control of every keep/drop:
#
#   1) PROPOSE  (auto first pass):
#        python3 pipeline/python/03_curate_dictionary.py propose [--topk 500]
#      Classifies the top-K discovered bigrams (by G2) into clean-domain /
#      domain+generic / generic / noise, auto-marks KEEP (domain*) or DROP
#      (generic/noise), and writes:
#        out/dict/curation_review.csv   <- editable: flip the `decision` column
#        out/dict/dictionary_curated.csv <- draft (seeds + auto-KEEP discovered)
#
#   2) (you) open curation_review.csv, change any `decision` cell to KEEP/DROP.
#
#   3) FINALIZE (apply your edits):
#        python3 pipeline/python/03_curate_dictionary.py finalize
#      Rebuilds out/dict/dictionary_curated.csv from the KEEP rows + seeds.
#      Then lock it with:  make lock-dictionary
#
# Inputs (defaults): candidates = out/dict/top_candidates_browse_v4.csv
# (top-2000, G2-ranked), seeds = pipeline/config/seeds.yml. Override with --input.
# ==============================================================================
import csv, os, re, sys, argparse, io, contextlib

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DICT = os.path.join(ROOT, "out", "dict")
REVIEW = os.path.join(DICT, "curation_review.csv")
CURATED = os.path.join(DICT, "dictionary_curated.csv")      # pruned (KEEP only)
UNPRUNED = os.path.join(DICT, "dictionary_unpruned.csv")    # Sautner-faithful (all)

# Reuse the categoriser from analyze_dicts (suppress its on-import audit output).
sys.path.insert(0, os.path.join(ROOT, "pipeline", "python"))
with contextlib.redirect_stdout(io.StringIO()):
    from analyze_dicts import classify, themes_of  # stdlib-only module

def load_seeds(path):
    seeds = []
    for line in open(path):
        if line.lstrip().startswith("#"):
            continue
        m = re.match(r"^\s+-\s+(\S.*?)\s*$", line)
        if m:
            seeds.append(m.group(1))
    return seeds

def load_candidates(path):
    rows = []
    with open(path) as f:
        for r in csv.DictReader(f):
            rows.append(r)
    return rows

def propose(args):
    seeds = load_seeds(os.path.join(ROOT, "pipeline", "config", "seeds.yml"))
    cand = load_candidates(args.input)[: args.topk]
    with open(REVIEW, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["rank", "bigram", "category", "auto_decision", "decision",
                    "theme", "G2", "target_freq", "target_share"])
        kept = dropped = 0
        for i, r in enumerate(cand, 1):
            bg = r["bigram"].strip()
            cat, th = classify(bg)
            decision = "KEEP" if cat in ("domain", "domain+generic") else "DROP"
            kept += decision == "KEEP"; dropped += decision == "DROP"
            w.writerow([i, bg, cat, decision, decision, "|".join(sorted(th)),
                        r.get("G2", ""), r.get("target_freq", ""), r.get("target_share", "")])
    _write_both(seeds)
    print(f"[propose] top {len(cand)} candidates -> {kept} auto-KEEP, {dropped} auto-DROP")
    print(f"  review/edit : {REVIEW}  (change the `decision` column to KEEP/DROP)")
    print(f"  UNPRUNED    : {UNPRUNED}  ({len(seeds)} seeds + {len(cand)} discovered) [Sautner-faithful primary]")
    print(f"  pruned draft: {CURATED}  ({len(seeds)} seeds + {kept} discovered) [robustness]")
    print(f"  then        : finalize, then `make lock-dictionary`")

def _rows_from_review(keep_only):
    rows = []
    with open(REVIEW) as f:
        for r in csv.DictReader(f):
            if keep_only and r["decision"].strip().upper() != "KEEP":
                continue
            rows.append((r["bigram"], r.get("G2", ""), r.get("target_freq", ""),
                         r.get("target_share", "")))
    return rows

def _write_dict(path, seeds, disc_rows):
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["bigram", "origin", "G2", "target_freq", "target_share"])
        for s in seeds:
            w.writerow([s, "seed", "", "", ""])
        for bg, g2, tf, ts in disc_rows:
            w.writerow([bg, "discovered", g2, tf, ts])

def _write_both(seeds):
    # PRIMARY (Sautner-faithful): seeds + ALL reviewed discovered, no pruning.
    _write_dict(UNPRUNED, seeds, _rows_from_review(keep_only=False))
    # ROBUSTNESS (pruned): seeds + KEEP only.
    _write_dict(CURATED, seeds, _rows_from_review(keep_only=True))

def finalize(args):
    if not os.path.exists(REVIEW):
        sys.exit("No curation_review.csv — run `propose` first.")
    seeds = load_seeds(os.path.join(ROOT, "pipeline", "config", "seeds.yml"))
    _write_both(seeds)
    nall = len(_rows_from_review(keep_only=False)); nkeep = len(_rows_from_review(keep_only=True))
    print(f"[finalize] UNPRUNED {UNPRUNED}: {len(seeds)}+{nall} = {len(seeds)+nall} terms (Sautner-faithful)")
    print(f"[finalize] pruned   {CURATED}: {len(seeds)}+{nkeep} = {len(seeds)+nkeep} terms (robustness)")

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("propose"); p.add_argument("--input",
        default=os.path.join(DICT, "top_candidates_browse_v4.csv")); p.add_argument("--topk", type=int, default=500)
    sub.add_parser("finalize")
    a = ap.parse_args()
    (propose if a.cmd == "propose" else finalize)(a)
