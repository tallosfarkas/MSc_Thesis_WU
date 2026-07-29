#!/usr/bin/env bash
# ==============================================================================
# reproduce.sh — turnkey driver for the v2 (minimal-cleaning) reproduction.
#
#   ./reproduce.sh cluster-text      submit the SLURM text chain (corpus -> tokenise
#                                    -> export -> KLR discovery), dependency-chained.
#                                    Run ON the cluster, from the repo root.
#   ./reproduce.sh dictionary        build the locked 9,650-term dictionary from the
#                                    discovery output (python; seconds).
#   ./reproduce.sh cluster-exposure  submit Stage 3 (exposure array + combine).
#   ./reproduce.sh local-analysis    run the mapping + full Stage 5-6 analysis locally
#                                    (needs the market data described in docs/DATA_ACCESS.md).
#   ./reproduce.sh verify            check the headline numbers (verify/verify_headline.R).
#
# The published thesis numbers are the v2 MINIMAL-CLEANING configuration. Its flags
# are exported below and are also the pipeline defaults, so a plain run reproduces v2.
# To reproduce the frozen v1.1 record instead: SW_BP=iter SW_SENTSH=on GEO_TAG=_v11.
#
# SLURM notes (WU cluster; adapt to yours): scripts request --partition=short
# (1-day walltime, no per-job CPU cap). Override at submit time with
# `sbatch --partition=<yours> ...` if needed; do not use `--export=ALL,...`
# (on some clusters it triggers "user env retrieval failed").
# ==============================================================================
set -euo pipefail
cd "$(dirname "$0")"

# ---- v2 minimal-cleaning configuration (the published numbers) ---------------
export SW_BP=off SW_MARKER=on SW_SENTSH=none          # corpus cleaning: minimal
export GEO_FIX=1 GEO_TAG=_min GEO_EXPO=min GEO_TIES=first

CMD="${1:-help}"
say(){ printf '\n== %s ==\n' "$*"; }

case "$CMD" in
  cluster-text)
    say "Stage 1-2: corpus -> tokenise -> export -> KLR discovery (chained)"
    C=$(sbatch --parsable pipeline/cluster/slurm_corpus.sh);          echo "corpus     $C"
    T=$(sbatch --parsable --dependency=afterok:$C pipeline/cluster/slurm_tokenise.sh);       echo "tokenise   $T"
    E=$(sbatch --parsable --dependency=afterok:$C pipeline/cluster/slurm_export_parquet.sh); echo "export     $E"
    P=$(sbatch --parsable --dependency=afterok:$E pipeline/cluster/slurm_klr_py.sh);         echo "discovery  $P"
    echo "monitor: squeue -u \$USER   |   then run: ./reproduce.sh dictionary"
    ;;
  dictionary)
    say "Stage 2c: lock the 9,650-term dictionary (top 9,600 by G2, seeds excluded then re-added)"
    python3 pipeline/python/02c_build_locked_dictionary.py \
        --keyness out/dict/keyness_all_py.csv \
        --seeds   config/seeds.yml \
        --out     config/dictionary_geoeconomic.csv
    n=$(($(wc -l < config/dictionary_geoeconomic.csv) - 1)); echo "dictionary terms: $n (expect 9650)"
    ;;
  cluster-exposure)
    say "Stage 3: exposure array + combine (chained)"
    A=$(sbatch --parsable pipeline/cluster/slurm_exposure.sh);        echo "exposure   $A"
    B=$(sbatch --parsable --dependency=afterok:$A pipeline/cluster/slurm_exposure_combine.sh); echo "combine    $B"
    R=$(sbatch --parsable pipeline/cluster/slurm_event_registry.sh);  echo "registry   $R"
    ;;
  local-analysis)
    say "Stage 3e + 5 + 6: mapping, asset-pricing analysis, results master (local, ~2-4 h)"
    Rscript pipeline/R/03e_map_identifiers.R
    for s in 05_build_analysis_panel 05a_quarterly_from_daily 05m_realization_q1 \
             05e_volatility_q1 05n_event_study 05d_fama_macbeth 05f_panel_fe \
             05r_sentiment_decomposition 05c_download_crsp 05j_crsp_monthly \
             05b_portfolio_sorts 05k_robustness 05q_augmented_spanning 05p_hp_controls \
             05l_tnic_spillover 05t_defense_cuts 14_realtime_dict_oos 06_results_master; do
      echo "-- $s"; Rscript "pipeline/R/${s}.R"
    done
    ;;
  verify)
    say "Headline verification against the published v2 numbers"
    Rscript verify/verify_headline.R out
    ;;
  *)
    sed -n '3,24p' "$0"
    ;;
esac
