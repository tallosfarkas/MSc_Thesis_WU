# Expected results

> **Reproduction evidence.** These are the numbers of the **v2 minimal-cleaning build** — the
> configuration the submitted thesis reports from (`thesis_v2.Rnw`, `SUF="_min"`).
>
> * **`verify/verify_headline.R`: 14/14 PASS** against the minimal artifacts.
> * The minimal analysis reproduced **bit-identically** on the WU cluster (from-zero run; the
>   firm-quarter panel came back with 442,543 rows and every measure `identical`).
> * The dictionary is a **fixed shipped input**; a from-scratch cluster discovery run reproduced it
>   **9,650/9,650 (100.000%)** and the KLR discovery stage was byte-identical (`dictionary_v1_py.csv`
>   md5 match, `n_sentences_total` and keyness counts identical).
> * The out-of-sample re-discovery and the industry/underreaction cuts are computed on the **v8.2
>   CRSP crosswalk**.
>
> So a replicator running this code on the same data, with the minimal-cleaning flags, should get
> exactly these figures, not merely similar ones.

The numbers a correct run should produce from the **v2 minimal-cleaning configuration**
(`SW_BP=off SW_MARKER=on SW_SENTSH=none`, `GEO_FIX=1 GEO_TAG=_min GEO_TIES=first`, v8.2 CRSP map).
`verify/verify_headline.R` checks the headline subset automatically.

Tolerances: *t*-statistics ±0.05, coefficients ±0.02pp unless noted. Small drift is expected if
your data vintage differs (LSEG restates transcripts; CRSP revises returns).

> **What separates this from the frozen v1.1 record.** v1.1 (`GEO_TAG=_v11`) removed safe-harbour
> boilerplate at the corpus stage; v2 does not (one sklearn tokeniser and nothing aggressive,
> faithful to Sautner 2023 and Hassan 2025). That single lever is the only methodological
> difference. It moves exactly one headline cell — the RQ3 GeoSentiment in-sample alpha, which was a
> tokenisation-alignment artifact at *t* = 2.13 and is a null at *t* = 1.80 once the cleaning is
> minimal. Everything load-bearing holds; see the v1.1→v2 map at the end.

## Dictionary

| Item | Expected |
|---|---|
| Seed bigrams | 50 |
| Discovered bigrams | 9,600 |
| **Total dictionary** | **9,650** |
| Pruned variant (robustness) | 4,586 |

## RQ1 — Realization (contemporaneous, LSEG, firm + quarter FE)

Per one-standard-deviation change in the measure, same quarter.

| Measure | Return | *t* |
|---|---|---|
| GeoExposure | −0.53% | −5.49 |
| GeoExposure (TF-IDF) | −0.50% | −5.08 |
| GeoRisk | −0.45% | −5.14 |
| **GeoSentiment** | **+1.42%** | **+13.79** |

Volatility **falls** (it does not spike): idiosyncratic volatility −2 bps (*t* = −2.87) for
GeoExposure. Falling volatility alongside a falling return means the decline is not idiosyncratic
risk being relabelled.

Daily event study (market model, Q5−Q1 CAAR): GeoRisk −0.30% over the [−1,+1] event window
(*t* = −5.60), then a +0.33% reversal over [+2,+20] (*t* = 4.15).

Split-half stability: *t* = −5.95 and −4.69 in the two sample halves.

## RQ2 — Pricing (Fama–MacBeth, Newey–West 4 lags, LSEG quarterly)

| Measure | λ (%/quarter) | *t* |
|---|---|---|
| GeoExposure | 0.23 | 0.99 |
| GeoExposure (TF-IDF) | 0.22 | 0.88 |
| GeoRisk | 0.04 | 0.54 |
| **GeoSentiment** | **0.26** | **2.05** |

Panel FE agrees and is stronger: GeoSentiment *t* = 2.96. On US CRSP returns the tone premium is
weak (*t* = 0.76) — the return-source asymmetry discussed in the thesis Limitations.

Tone decomposition (Fama–MacBeth): net 0.22 (*t* = 2.19), positive-tone leg 0.23 (*t* = 1.62),
negative-tone leg 0.04 (*t* = 0.32). Size split: small firms *t* = 2.03, large firms *t* = 1.33
(underreaction is concentrated, and only significant, in small firms).

## RQ3 — Strategy (quintile long–short, US CRSP monthly, FF5 alpha, NW)

| Measure | EW α (%/mo) | *t* | VW α | *t* |
|---|---|---|---|---|
| GeoExposure | 0.02 | 0.08 | −0.14 | −0.74 |
| GeoExposure (TF-IDF) | 0.02 | 0.08 | −0.13 | −0.67 |
| **GeoRisk** | **0.26** | **2.74** | 0.06 | 0.52 |
| GeoSentiment | 0.19 | 1.80 | 0.09 | 0.83 |

Only the equal-weighted GeoRisk long–short clears 5%. The GeoSentiment in-sample alpha is a **null**
(*t* = 1.80) under minimal cleaning — this is the one cell that differs from v1.1, and it is why the
thesis leads RQ3 with the out-of-sample tests below rather than the in-sample sort.

## Out-of-sample (the strict test)

Re-discovering the dictionary on pre-date text only (expanding-window vintages), then trading each
vintage the following year, US CRSP:

| Measure | In-sample α (*t*) | OOS, re-learned dictionary α (*t*) |
|---|---|---|
| GeoRisk | 0.26 (2.74) | −0.02 (**−0.06**) — dies |
| GeoSentiment | 0.19 (1.80) | 0.77 (**2.58**) — survives, stronger |

The tone premium survives the strictest look-ahead test; the risk-count alpha does not. The gentler
fixed-dictionary walk-forward OOS (real-time factor betas, 36-month warm-up) passes for both:
GeoRisk *t* = 2.23, GeoSentiment *t* = 2.36. On the global LSEG sample the re-learned tone strategy
is fragile (*t* = 0.38), mirroring the return-source asymmetry.

Split-half for the contemporaneous RQ1: −5.95 / −4.69.

## Robustness (GeoRisk L/S, CRSP unless noted)

| Cut | GeoRisk L/S result |
|---|---|
| FF5 baseline | 0.26% (*t* = 2.74) |
| + momentum (UMD) | 0.27% (*t* = 2.80) |
| + BAB, QMJ | 0.25% (*t* = 2.55) |
| + illiquidity, reversal | 0.14% (*t* = 1.35) |
| + all factors (US) | 0.19% (*t* = 1.74) — residual survives at 10% |
| + all factors (global LSEG) | 0.05% (*t* = 0.61) — vanishes |
| SIC2-adjusted | 0.05% (*t* = 0.71) |
| SIC1-division-adjusted | 0.10% (*t* = 1.02) |
| **GICS-sector-neutral** | **−0.05% (*t* = −0.56)** — largely a cross-sector effect |

Value-weighting removes the alpha (VW *t* = 0.52). Across the full 48-cell sort grid (universe ×
measure × 5/10 bins × EW/VW, `monthly_q3_grid_min.json`) the only cell that clears 5% is US GeoRisk,
quintile, equal-weighted (0.17%/month, *t* = 2.38); every value-weighted cell and every global-sample
cell is insignificant. The alpha is therefore specific to one corner of the grid, which is why the
thesis presents RQ3 as an asset-pricing test rather than a tradeable product.

## Trading costs

Market-cap-decreasing cost model (base 5 bps + slope·√(refcap/mcap)) applied to measured turnover:

| Strategy | Gross %/yr | Net %/yr |
|---|---|---|
| GeoRisk | 3.2 | **1.7** |
| GeoSentiment | 2.3 | **0.9** |

Both stay positive; the tone strategy is roughly halved.

## Other validation

| Check | Expected |
|---|---|
| Quarterly correlation with GPR threat index | ρ ≈ 0.25 |
| Real-time dictionary recoverability (top-1000 overlap) | 79% at the 2012 vintage, rising to 96% by 2024 (`divergence$by_vintage$cover_top1000`) |
| Language filter: non-English calls dropped | ≈0.7% |
| Sample | 2002–2025; 276 monthly observations on CRSP; ~442.5k firm-months in the minimal analysis panel |

## v1.1 → v2 map (only one cell flips)

| Result | v1.1 (`_v11`) | v2 (`_min`) |
|---|---|---|
| RQ1 GeoExposure realization *t* | −5.24 | −5.49 |
| RQ1 GeoSentiment realization *t* | 12.87 | 13.79 |
| RQ2 GeoSentiment pricing *t* | 2.13 | 2.05 |
| RQ3 GeoRisk in-sample *t* | 2.87 | 2.74 |
| RQ3 GeoRisk walk-forward OOS *t* | 2.60 | 2.23 |
| RQ3 GeoSentiment walk-forward OOS *t* | 2.37 | 2.36 |
| RQ3 GeoRisk re-discovery OOS *t* | 0.60 | −0.06 |
| RQ3 GeoSentiment re-discovery OOS *t* | 2.07 | 2.58 |
| **RQ3 GeoSentiment in-sample *t*** | **2.13** | **1.80 (null)** |

The single fragile cell (RQ3 GeoSentiment in-sample) flips from a tokenisation-alignment artifact to
its true null; everything load-bearing holds, and the tone-vs-risk-count split is sharper.
