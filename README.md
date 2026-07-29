# Geoeconomic Exposure and the Cross-Section of Individual Stock Returns

Replication package for the MSc Quantitative Finance thesis at WU Vienna
(author: Farkas Tallós; supervisor: Prof. Otto Randl; submitted August 2026).

This repository contains **all code needed to reproduce the thesis end to end**: the
firm-level geoeconomic exposure measure built from earnings-call transcripts, the asset-pricing
tests, the robustness suite, and the thesis tables and figures.

> **The raw text and return data cannot be redistributed.** LSEG StreetEvents transcripts and
> CRSP/Compustat are licensed. See [`docs/DATA_ACCESS.md`](docs/DATA_ACCESS.md) for exactly which
> datasets are required, where to obtain them, and the schema each stage expects.

---

## What this project does

Firms increasingly face **firm-specific** geoeconomic shocks: a tariff on their inputs, a
sanctioned counterparty, an export ban on their product. Aggregate indices (GPR, EPU) cannot see
that, because every firm gets the same number on the same day. This project builds a firm-level
measure instead, from what managers actually say on earnings calls, and asks whether it is priced.

The method follows Sautner, van Lent, Vilkov and Zhang (2023, *Journal of Finance*) for climate
exposure, but changes the domain to geoeconomics. A **King–Lam–Roberts (KLR) keyword-discovery**
step grows a 50-bigram seed list into a **9,650-bigram geoeconomic dictionary**
(`config/dictionary_geoeconomic.csv`, shipped here), which yields four firm-quarter measures:

| Measure | What it captures |
|---|---|
| `GeoExposure` | how much geoeconomic talk (raw bigram count / total) |
| `GeoExposureTFIDF` | the same, TF-IDF weighted |
| `GeoRisk` | geoeconomic talk next to a **risk** word in the same sentence |
| `GeoSentiment` | the **tone** of that talk (positive minus negative) |

The analysis is organised as a life cycle: **Realize → Price → Trade**.

## Headline results

Reproducing this package should give (see [`docs/EXPECTED_RESULTS.md`](docs/EXPECTED_RESULTS.md)
for the full check-list and tolerances):

| Stage | Result |
|---|---|
| **RQ1 Realize** | A one-SD rise in geoeconomic talk meets a **−0.53%** same-quarter return (*t* = −5.49). Volatility **falls**, so the call resolves uncertainty. GeoSentiment is the mirror image (**+1.42%**, *t* = 13.79). |
| **RQ2 Price** | Only the **tone** is priced: GeoSentiment **+0.26%/quarter** (*t* = 2.05 Fama–MacBeth, 2.96 panel FE). The count measures are not priced. Same positive sign at the call *and* a quarter ahead ⇒ **underreaction**, not a risk premium. |
| **RQ3 Trade** | GeoRisk long–short **0.26%/month** five-factor alpha (*t* = 2.74, US CRSP, equal-weighted). The GeoSentiment in-sample sort is a null under minimal cleaning (*t* = 1.80); its premium shows up out of sample instead. |
| **Out-of-sample** | Re-learning the dictionary on pre-date text only: the tone premium **survives** and strengthens (*t* = 2.58); the GeoRisk alpha **does not** (*t* = −0.06). |
| **Net of costs** | GeoRisk 3.2% → **1.7%**/yr, GeoSentiment 2.3% → **0.9%**/yr. |

Honest qualifiers, stated in the thesis and reproduced by this code: the priced and tradeable
results are US-concentrated and equal-weighted, the GeoRisk long–short is largely a cross-sector
effect (GICS-sector-neutral −0.05%, *t* = −0.56), illiquidity and reversal absorb much of it, and the
GeoRisk alpha is fragile out of sample.

## Repository layout

```
config/           seed list, risk words, parameters, and the discovered 9,650-term dictionary
pipeline/R/       the analysis pipeline, numbered by stage (00 -> 06)
pipeline/python/  KLR discovery (scikit-learn), with uv.lock for an exact environment
pipeline/cluster/ SLURM submitters + rsync helpers for the WU cluster
thesis/R/         builders that turn the results into the thesis tables and figures
verify/           one script that checks your output against the published numbers
docs/             DATA_ACCESS, ENVIRONMENT, REPRODUCE, EXPECTED_RESULTS
```

Stage prefixes: `00` audit · `01` corpus build · `02a` tokenise · `02b` KLR discovery ·
`02p` export sentences · `03` exposure measurement · `04` validation ·
`05` asset-pricing analysis · `06` results master + figures.

## Quickstart

```bash
# 1. environments (see docs/ENVIRONMENT.md)
cd pipeline/python && uv sync --frozen && cd ../..
Rscript -e 'install.packages(c("data.table","fixest","sandwich","ggplot2","arrow","jsonlite"))'

# 2. put the licensed data where the pipeline expects it (docs/DATA_ACCESS.md)

# 3. run the pipeline — ./reproduce.sh drives the whole thing with the published
#    v2 minimal-cleaning flags baked in (docs/REPRODUCE.md has the detail):
./reproduce.sh cluster-text       # on a SLURM cluster: corpus -> tokenise -> discovery (chained)
./reproduce.sh dictionary         # lock the 9,650-term dictionary
./reproduce.sh cluster-exposure   # Stage 3 exposure array + combine
./reproduce.sh local-analysis     # mapping + Stage 5-6 (local, ~2-4 h)

# 4. check your numbers against the thesis
./reproduce.sh verify
```

Full instructions, including which stages need the cluster and roughly how long each takes, are in
**[`docs/REPRODUCE.md`](docs/REPRODUCE.md)**.

## Compute

The text stages are the expensive part. Corpus build and tokenisation are per-year SLURM arrays;
KLR discovery is a single large-memory job; exposure measurement is an array over years. The
asset-pricing stages (`05*`) run comfortably on a laptop once the firm-quarter panel exists.
Indicative sizes, 2002–2025: 508,966 raw transcript rows, 456,996 unique calls after de-duplication,
452,512 in the analysis sample; the daily event study touches 43.1M daily return rows and 12.4M
event-day observations.

## Versioning

All published numbers come from the **v2 minimal-cleaning** configuration
(`SW_BP=off SW_MARKER=on SW_SENTSH=none GEO_FIX=1 GEO_TAG=_min GEO_EXPO=min GEO_TIES=first`), which
writes `*_min` artifacts, together with the v8.2 CRSP identifier map. Scripts and the thesis
(`thesis_v2.Rnw`) prefer the `_min` twin of any results file. The single lever separating this from
the frozen v1.1 record (`_v11`) is corpus-stage boilerplate removal, which v2 drops; see
`docs/EXPECTED_RESULTS.md` for the v1.1→v2 map. Do not mix `_min` and `_v11` artifacts.

## Citation

See `CITATION.cff`. Code is MIT-licensed (`LICENSE`). The dictionary is released for research use;
the underlying transcripts and returns remain the property of their licensors.
