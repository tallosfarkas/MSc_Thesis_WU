# Reproducing the thesis, end to end

Every stage, in order, with what it reads, what it writes, roughly how long it takes, and whether
it needs a cluster. Commands assume you are at the repository root.

Two routes:

* **Route A (recommended, ~1 day on a laptop).** Use the shipped dictionary
  (`config/dictionary_geoeconomic.csv`) and start at **Stage 3**. This reproduces every number in
  the thesis without the expensive text stages.
* **Route B (full, needs a SLURM cluster).** Rebuild the corpus and re-discover the dictionary
  from raw transcripts, then continue. Only needed if you want to verify the discovery step
  itself, or run it on your own corpus.

Set the configuration first. All published numbers come from the **v2 minimal-cleaning** build —
one sklearn tokeniser and nothing aggressive, faithful to Sautner (2023) and Hassan (2025):

```bash
export SW_BP=off SW_MARKER=on SW_SENTSH=none      # minimal cleaning (no safe-harbour removal)
export GEO_FIX=1 GEO_TAG=_min GEO_EXPO=min GEO_TIES=first
```

* `SW_BP=off` is the lever that defines v2: the corpus stage does **not** strip safe-harbour
  boilerplate. (These are the defaults in `01_build_corpus.R`; the exports just make the run
  explicit and self-documenting.) To reproduce the frozen v1.1 record instead, set
  `SW_BP=iter SW_SENTSH=none GEO_TAG=_v11` and drop `GEO_EXPO`.
* `GEO_TAG=_min` tags every output artifact so it is never confused with a v1.1 (`_v11`) file; the
  thesis (`thesis_v2.Rnw`) reads the `_min` twins.
* `GEO_EXPO=min` writes and reads exposure under `out/exposure_min/`.
* `GEO_TIES=first` matters: with ~50% zeros in the early sample, average ties leave the bottom
  quintile empty and silently drop whole months. First-ties keeps all 276 months. Results are robust
  either way, but the published figures use first-ties.

---

## Route B, Stage 1–2: corpus and dictionary (cluster)

| Stage | Script | Reads | Writes | Time |
|---|---|---|---|---|
| 01 | `pipeline/R/01_build_corpus.R` | raw transcripts | `out/corpus/sentences_<year>.rds` | per-year array, ~1–2 h/yr |
| 02a | `pipeline/R/02a_tokenise.R` | corpus | tokenised bigrams | per-year array |
| 02p | `pipeline/R/02p_export_sentences.R` | corpus | parquet for Python | ~30 min |
| 02b | `pipeline/python/…` (sklearn) or `pipeline/R/02b_klr_discovery.R` | parquet / dfm | `config/dictionary_geoeconomic.csv` | 4–8 h, 64 cores, 256 GB |

Chained submission (mirrors the project Makefile):

```bash
# from your local checkout: push code to the cluster first
rsync -av --exclude-from=pipeline/cluster/rsync.exclude pipeline/ CLUSTER:~/thesis_clean/pipeline/

ssh CLUSTER 'cd ~/thesis_clean && \
  C=$(sbatch --parsable pipeline/cluster/slurm_corpus.sh)                                  && echo "corpus     $C" && \
  E=$(sbatch --parsable --dependency=afterok:$C pipeline/cluster/slurm_export_parquet.sh)  && echo "export     $E" && \
  P=$(sbatch --parsable --dependency=afterok:$E pipeline/cluster/slurm_klr_py.sh)          && echo "discovery  $P"'

# monitor
ssh CLUSTER 'squeue -u $USER -o "%.11i %.9P %.12j %.2t %.10M %.10L %R"'
bash pipeline/cluster/cluster_watch.sh <JOBID>
```

**Cluster notes (WU, adapt to yours).** Nodes are 128 CPU / 500 GB. Use `--mem-per-cpu`; asking
for `--cpus-per-task=128` at 4 GB/CPU exceeds the 500 GB ceiling, so cap at ~64 CPU × 7 GB or
120 × 4 GB. Submit on the `short` partition (1-day walltime, no per-job CPU cap); the default
`medium` partition caps CPUs at 10 and will reject the discovery job. Pass environment via plain
script arguments, **not** `--export=ALL,...`, which triggers "user env retrieval failed".

Stage 2b caches the trimmed document-feature matrix
(`out/dict/_cache_dfm_all_trimmed.rds`), so re-running with a new seed or threshold skips the
~3.5 h rebuild.

### Stage 2c: build the locked dictionary (do not skip)

Discovery writes a raw keyness table (~17M candidate bigrams) plus a small `dictionary_v1_py.csv`
of about 550 terms. **That 550-term file is not the production dictionary.** The locked
9,650-term dictionary is built by a separate selection step:

```bash
python3 pipeline/python/02c_build_locked_dictionary.py \
    --keyness out/dict/keyness_all_py.csv \
    --seeds   config/seeds.yml \
    --out     config/dictionary_geoeconomic.csv
```

The rule is: drop the 50 seeds from the candidate pool, keep the top 9,600 remaining bigrams by
G2, then add the seeds back. Seeds are excluded from the ranking deliberately, because they are
known-positive by construction and would otherwise displace 37 genuinely discovered terms.

Note that `curate_top_k: 500` in `config/params.yml` controls the *face-validity review depth*
only. Running discovery and stopping there gives roughly 550 terms, not the 9,650 the thesis uses.

**Expected output:** 9,650 bigrams (50 seeds + 9,600 discovered). The thesis does **not**
hand-prune; the Eq. 4 TF-IDF weighting absorbs noise, following Sautner et al. A from-scratch
cluster run on 2026-07-21 reproduced this dictionary **exactly, 9,650/9,650 (100.000%)**.

## Stage 3: exposure measurement (cluster array, or local per-year)

```bash
ssh CLUSTER 'cd ~/thesis_clean && \
  A=$(sbatch --parsable pipeline/cluster/slurm_exposure.sh) && echo "exposure $A" && \
  sbatch --dependency=afterok:$A pipeline/cluster/slurm_exposure_combine.sh'
```

| Script | Purpose |
|---|---|
| `03_measure_exposure.R` | the four measures per firm-quarter (Eqs. 1–4) |
| `03b_combine_exposure.R` | combine per-year shards |
| `03d_event_registry.R` | call-date registry for the daily event study |
| `03e_map_identifiers.R` | LSEG RIC → PERMNO (v8.2 map) |

Writes `out/exposure_min/exposure_firmquarter*.rds` (the directory follows `GEO_EXPO`; unset it and
the frozen v1.1 build writes plain `out/exposure/`). From here everything runs locally.

## Stage 4: validation

| Script | Checks |
|---|---|
| `04b_perturbation_corr.R` | leave-one-seed-out stability of the dictionary |
| `04c_gpr_correlation.R` | the measure should track aggregate GPR loosely (quarterly ρ ≈ 0.25) |
| `04f/04g/04h/04i` | LLM precision and the human snippet audit |
| `04j_llm_feasibility.R` | why a full LLM measure is infeasible at this scale |

## Stage 5: the asset-pricing analysis (local, ~2–4 h total)

Run in this order:

```bash
Rscript pipeline/R/05_build_analysis_panel.R      # firm-quarter panel + controls
Rscript pipeline/R/05a_quarterly_from_daily.R
Rscript pipeline/R/05m_realization_q1.R           # RQ1 contemporaneous return
Rscript pipeline/R/05e_volatility_q1.R            # RQ1 IVOL / TVOL
Rscript pipeline/R/05n_event_study.R              # RQ1 daily event study (heaviest: 43.1M daily rows)
Rscript pipeline/R/05d_fama_macbeth.R             # RQ2 pricing
Rscript pipeline/R/05f_panel_fe.R                 # RQ2 panel FE
Rscript pipeline/R/05r_sentiment_decomposition.R  # RQ2 tone legs
Rscript pipeline/R/05c_download_crsp.R            # CRSP pull (once)
Rscript pipeline/R/05j_crsp_monthly.R             # RQ3 headline long-short alpha
Rscript pipeline/R/05b_portfolio_sorts.R          # sort grid
Rscript pipeline/R/05k_robustness.R               # splits: size-neutral, pre/post-2018, SIC2, FIC300
Rscript pipeline/R/05q_augmented_spanning.R       # 8-factor spanning
Rscript pipeline/R/05p_hp_controls.R              # Hoberg-Phillips product-market controls
Rscript pipeline/R/05l_tnic_spillover.R           # TNIC peer spillover
Rscript pipeline/R/05t_defense_cuts.R             # industry (SIC2 / GICS-neutral), size splits
Rscript pipeline/R/14_realtime_dict_oos.R         # out-of-sample: real-time dictionary re-discovery
```

`05n_event_study.R` needs a working `arrow` install to read the daily parquet. If `arrow` fails
locally, run this one stage on the cluster.

## Stage 6: results master, figures, thesis artifacts

```bash
Rscript pipeline/R/06_results_master.R        # -> out/analysis/RESULTS_MASTER_min.{csv,json}
Rscript pipeline/R/06b_figures.R              # -> out/figures/
Rscript thesis/R/build_thesis_artifacts_min.R # -> thesis tables/*.tex and figures/*.pdf (reads _min twins)
Rscript thesis/R/build_strategy_chart_min.R   # cumulative-return, legs, and net-of-cost figures
```

`RESULTS_MASTER_min.csv` is the single source of truth for every inline number in the thesis; the
manuscript (`thesis_v2.Rnw`) pulls them live via `\Sexpr`, so no result is ever typed by hand. The
`_min` builders read the `_min` JSON twins; the untagged `build_thesis_artifacts.R` is the v1.1
variant, kept for reference.

## Verify

```bash
Rscript verify/verify_headline.R
```

This compares your artifacts against the published headline numbers with sensible tolerances and
prints a PASS/FAIL table. See [`EXPECTED_RESULTS.md`](EXPECTED_RESULTS.md) for the full list.

## Gotchas worth knowing

* **Never mix tagged and untagged artifacts, or `_min` with `_v11`.** Scripts and the thesis prefer
  the `_min` twin (v2); the frozen v1.1 outputs carry `_v11`. If a headline number reads the v1.1
  value (RQ3 GeoRisk *t* = 2.87, GeoSentiment in-sample *t* = 2.13) you are on `_v11` artifacts, not
  the minimal build. `verify/verify_headline.R` guards this explicitly.
* **Join on RIC, not ticker.** LSEG rewrites tickers and names retroactively after mergers.
* **Winsorise inside the period.** The pipeline winsorises returns at 0.5/99.5 within each month;
  pre-winsorising the input changes the long–short.
* **The GPR spreadsheet mistypes columns** unless you read it with a high `guess_max`.
* **Reproducibility of the environment** is pinned: Python via `uv.lock`, R versions in
  `pipeline/R/PACKAGE_VERSIONS.md`. See [`ENVIRONMENT.md`](ENVIRONMENT.md).
