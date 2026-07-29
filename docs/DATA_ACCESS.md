# Data access

None of the raw inputs can be redistributed here. All of them are either licensed (LSEG, CRSP,
Compustat) or better taken fresh from the original source (Fama–French factors, GPR, EPU). This
file lists exactly what you need, where it comes from, and the schema each pipeline stage expects,
so you can drop your own copies in and run.

Put everything under a `data/` directory at the repository root. `data/` is git-ignored.

---

## 1. Earnings-call transcripts (required, licensed)

**Source:** LSEG (Refinitiv) StreetEvents transcripts, via a university subscription.
**Coverage used in the thesis:** 2002-01-01 to 2025-03-31, English-language calls.
**Scale:** ~300k calls, ~21k unique firms.

**What the pipeline actually reads** is one parsed R file per year:

```
data/parsed/TParsed_<year>.RData         # contains `meta.parsed`: one row per speaker TURN
```

If your LSEG delivery is raw XML, parse it into that shape first (one row per speaker turn, with
the fields below); `pipeline/R/01_build_corpus.R` is the single consumer of this file and documents
every column it touches, so it is the only place you would need to adapt.

What the pipeline needs from each transcript:

| Field | Used for |
|---|---|
| transcript id | deduplication |
| RIC (Reuters Instrument Code) | the join key to prices; **join on RIC, not ticker** |
| company name | reporting only |
| call datetime (UTC) | assigning the call to a fiscal quarter, and the daily event study |
| section (presentation / Q&A) | operator turns are dropped |
| speaker + turn text | the text that is tokenised |

> **Identifier warning.** LSEG retroactively rewrites `companyTicker` and `companyName` after
> mergers and renames. Join on **RIC**. The thesis maps RIC → PERMNO through a CRSP/Compustat
> link (see §3); that map is versioned (v8.2 in the published run).

`pipeline/R/01_build_corpus.R` turns each `TParsed_<year>.RData` into the per-year corpus
(`out/corpus/`), applying the minimal-cleaning defaults (`SW_BP=off SW_MARKER=on SW_SENTSH=none`).

## 2. Returns and accounting (required, licensed)

**Source:** WRDS.

| Dataset | Used for | Lands in |
|---|---|---|
| CRSP monthly stock file (`permno`, `date`, `ret`, `prc`, `shrout`) | RQ3 monthly long–short, market cap | `data/raw/crsp/crsp_monthly.*` |
| CRSP daily stock file | the daily event study (RQ1) | `data/raw/crsp/crsp_daily.*` |
| CRSP/Compustat Merged linking table | RIC/gvkey → PERMNO map | `data/raw/crsp/ccm_link.*` |
| Compustat fundamentals (annual) | book-to-market and controls | `data/raw/compustat/` |

Returns are winsorised at 0.5/99.5 **within each period** by the pipeline; do not pre-winsorise.

## 3. Identifier map (required, derived)

The published run uses the **v8.2** LSEG↔CRSP map. `pipeline/R/03e_map_identifiers.R` builds it
from the CCM link table plus name/ticker history. Expect roughly a 70–75% match rate from LSEG
firms to PERMNOs; the unmatched tail is mostly non-US listings, which is why the global LSEG
sample is larger than the US CRSP sample. This asymmetry matters for interpreting RQ2 vs RQ3 and
is discussed in the thesis Limitations.

## 4. Factors and macro series (required, free)

| Series | Source | Notes |
|---|---|---|
| Fama–French 5 factors + momentum, monthly | Ken French Data Library | file must expose `MktRF, SMB, HML, RMW, CMA, RF` and a `Date` column |
| Betting-Against-Beta, Quality-Minus-Junk | AQR data library | used only in the illiquidity/reversal robustness table |
| Geopolitical Risk index (GPR, GPRT) | Caldara & Iacoviello | spreadsheet; read with `guess_max` set high or columns mistype |
| Economic Policy Uncertainty (EPU) | Baker, Bloom & Davis | monthly US index |

Place these under `data/inputs/`. The pipeline expects
`data/inputs/ff5_factors_monthly.csv` specifically.

> FRED blocks scripted downloads from some networks; the thesis pulled the market series via
> `yfinance` where needed and cached them under `data/inputs/macro/`.

## 5. Loughran–McDonald master dictionary (required, free, not shipped)

Used for the sentiment and risk word lists. Download the current
**Loughran–McDonald Master Dictionary** and place it at:

```
config/lm_master_dictionary.csv
```

It is ~9 MB and third-party, so it is deliberately not vendored here.

## 6. What *is* shipped

| File | Why it can be shared |
|---|---|
| `config/dictionary_geoeconomic.csv` | the 9,650-bigram dictionary this project **produces** (50 seeds + 9,600 discovered) |
| `config/dictionary_geoeconomic_pruned.csv` | the 4,586-term pruned variant used as robustness |
| `config/seeds.yml` | the 50 seed bigrams that start KLR discovery |
| `config/risk_words*.yml`, `config/stopwords_*.yml`, `config/params.yml` | method parameters |

Shipping the dictionary means you can reproduce every result from **Stage 3 onward** without
re-running the expensive discovery step, and you can compare your own discovered dictionary
against the published one.

## Licensing summary

The code in this repository is MIT-licensed. The geoeconomic dictionary is released for research
use. The transcripts, returns and accounting data remain the property of LSEG, CRSP, Compustat and
their respective licensors, and are not redistributed in any form.
