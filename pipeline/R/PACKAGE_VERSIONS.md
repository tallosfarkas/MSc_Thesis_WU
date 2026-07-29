# R package versions (WU cluster, pipeline execution environment)

Captured 2026-06-03 from `installed.packages()` on `wucluster` (the machine that
runs Stages 1, 2a, 2b-R, 3, 4). This is the reproducibility record for the R side
until a full `renv.lock` is generated (see `renv_snapshot.R`; deferred so it does
not disturb the live v3 run).

- **R version: 4.5.1**

| Package | Version |
|---|---|
| arrow | 24.0.0 |
| cld2 | 1.2.6 |
| data.table | 1.18.2.1 |
| jsonlite | 2.0.0 |
| LiblineaR | 2.10-24 |
| Matrix | 1.7-4 |
| quanteda | 4.3.1 |
| quanteda.textmodels | 0.9.10 |
| quanteda.textstats | 0.97.2 |
| ranger | 0.18.0 |
| RcppParallel | 5.1.11-2 |
| stringi | 1.8.7 |
| stringr | 1.6.0 |
| yaml | 2.3.12 |

To regenerate this list: `Rscript pipeline/R/renv_snapshot.R --versions-only`.
