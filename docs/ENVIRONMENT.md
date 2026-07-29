# Environment

Two runtimes: **R** for the pipeline and the thesis artifacts, **Python** for the KLR keyword
discovery step (Stage 2b). Both are pinned.

## Python (Stage 2b only)

Pinned exactly via `uv.lock`, so a `uv sync --frozen` reproduces the discovery environment
byte-for-byte, locally or on the cluster.

```bash
cd pipeline/python
uv sync --frozen        # creates .venv with the locked versions
```

Requires Python `>=3.11,<3.14`. Key pins (see `pipeline/python/pyproject.toml`):

| Package | Version |
|---|---|
| scikit-learn | 1.9.0 |
| scipy | 1.17.1 |
| numpy | 2.4.6 |
| pandas | 3.0.3 |
| pyarrow | 24.0.0 |
| pyyaml | 6.0.3 |

> The scikit-learn generation matters. The discovery step's ensemble behaviour differs across
> major sklearn versions; 1.9 is what produced the published dictionary.

## R (everything else)

**R 4.5.1.** Full captured version list in `pipeline/R/PACKAGE_VERSIONS.md`. The versions that
actually affect results:

| Package | Version | Used for |
|---|---|---|
| data.table | 1.18.2.1 | all panel work |
| quanteda | 4.3.1 | tokenisation, document-feature matrix |
| quanteda.textmodels | 0.9.10 | KLR discovery (R route) |
| quanteda.textstats | 0.97.2 | keyness statistics |
| arrow | 24.0.0 | daily parquet in the event study |
| LiblineaR | 2.10-24 | the linear classifiers in discovery |
| Matrix | 1.7-4 | sparse dfm |
| jsonlite | 2.0.0 | results serialisation |

Also needed for the asset-pricing stages and thesis artifacts: `fixest`, `sandwich`, `lubridate`,
`ggplot2`, `yaml`.

```bash
Rscript -e 'install.packages(c("data.table","quanteda","quanteda.textmodels","quanteda.textstats",
  "arrow","jsonlite","fixest","sandwich","lubridate","ggplot2","yaml","LiblineaR"))'
```

On the WU cluster: `module load r` gives R 4.5.1, with a personal library at `~/libs/R_libs`.

### A note on `arrow`

`05n_event_study.R` reads a ~1 GB daily parquet through `arrow`. A broken or mismatched `arrow`
build is the single most common local failure; if it will not read the parquet, run that one stage
on the cluster and pull the result back.

## Thesis document (optional)

Only needed if you want to rebuild the manuscript rather than just the numbers:

* TeX Live with `latexmk`, `biber`, `biblatex`
* R packages `knitr` and `xtable`

```bash
cd docs/thesis
Rscript -e 'knitr::knit("thesis.Rnw", output="thesis_rnw.tex")'
latexmk -pdf thesis_rnw.tex
```

Every inline number in the manuscript is pulled live from the results artifacts via `\Sexpr`, so
the document cannot drift from the data: if a number changes upstream, the text changes with it.

## Determinism

* Set `SW_BP=off SW_MARKER=on SW_SENTSH=none GEO_FIX=1 GEO_TAG=_min GEO_EXPO=min GEO_TIES=first`
  before any stage. These are the v2 minimal-cleaning settings the thesis reports from.
* Where a random tie-break or seed is involved, the pipeline averages over seeds and records the
  seed count in the output JSON (`n_seeds`).
* `Date.now()`-style nondeterminism is avoided in the analysis scripts; all date handling is
  explicit and driven by the data.
