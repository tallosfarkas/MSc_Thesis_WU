#!/usr/bin/env python3
# =====================================================================
# 02c_build_locked_dictionary.py
#
# Build the LOCKED 9,650-term production dictionary from the raw discovery
# output. This is the step between KLR discovery (02b) and exposure
# measurement (03), and it is the one that `curate_top_k: 500` does NOT do.
#
# The rule, verified on 2026-07-21 to reproduce the published dictionary
# 9,650/9,650 (100.000%) from a from-scratch cluster run:
#
#   1. read the full keyness table produced by 02b (~17M candidate bigrams)
#   2. DROP the seed bigrams from the candidate pool
#   3. keep the top 9,600 remaining bigrams by G2
#   4. add the 50 seeds back  ->  9,650 terms
#
# Seeds are excluded from the ranking on purpose: they are known-positive by
# construction, so letting them compete on G2 would displace 37 genuinely
# discovered terms (that is exactly the discrepancy this script resolves).
#
#   python3 pipeline/python/02c_build_locked_dictionary.py \
#       --keyness out/dict/keyness_all_py.csv \
#       --seeds   config/seeds.yml \
#       --out     config/dictionary_geoeconomic.csv
# =====================================================================
import argparse, csv, sys

TOP_K = 9600


def load_seeds(path):
    """Seeds from a YAML list or a one-per-line text/CSV file (stdlib only)."""
    seeds = []
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            s = line.strip()
            if not s or s.startswith("#"):
                continue
            if s.startswith("- "):
                s = s[2:].strip()
            elif s.endswith(":"):          # a YAML key, not a value
                continue
            seeds.append(s.strip().strip('"').strip("'").lower())
    return [s for s in seeds if s]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--keyness", required=True, help="keyness_all_py.csv from 02b")
    ap.add_argument("--seeds",   required=True, help="config/seeds.yml")
    ap.add_argument("--out",     required=True, help="dictionary_geoeconomic.csv to write")
    ap.add_argument("--top-k",   type=int, default=TOP_K)
    a = ap.parse_args()

    seeds = load_seeds(a.seeds)
    seedset = set(seeds)
    print(f"seeds loaded: {len(seeds)}")

    rows = []
    with open(a.keyness, newline="", encoding="utf-8") as fh:
        rd = csv.DictReader(fh)
        for r in rd:
            bg = (r.get("bigram") or "").strip()
            if not bg or bg.lower() in seedset:     # step 2: seeds never compete on G2
                continue
            try:
                g2 = float(r.get("G2") or "nan")
            except ValueError:
                continue
            if g2 != g2:                            # NaN
                continue
            rows.append((g2, bg, r.get("target_freq", ""), r.get("target_share", "")))
    print(f"candidates after dropping seeds: {len(rows):,}")

    rows.sort(key=lambda t: -t[0])                  # step 3: top-K by G2
    kept = rows[: a.top_k]
    print(f"kept top {len(kept):,} by G2 (cut G2 = {kept[-1][0]:.6f})")

    with open(a.out, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["bigram", "origin", "G2", "target_freq", "target_share"])
        for s in seeds:                             # step 4: seeds back in
            w.writerow([s, "seed", "", "", ""])
        for g2, bg, tf, ts in kept:
            w.writerow([bg, "discovered", g2, tf, ts])

    print(f"wrote {a.out}: {len(seeds) + len(kept):,} terms "
          f"({len(seeds)} seeds + {len(kept):,} discovered)")


if __name__ == "__main__":
    sys.exit(main())
