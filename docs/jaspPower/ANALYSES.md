# Analyses

This file maps each statistical test mode to its backend implementation and option behavior.

## Analysis Inventory

| UI value (`test`) | UI label | R runner (expected) | Core solver path | Status |
| --- | --- | --- | --- | --- |
| `independentSamplesTTest` | Independent Samples T-Test | `.runTtestIS` | `.pwrT2NRatio` / `.pwrT2NTest` | Active |
| `pairedSamplesTTest` | Paired Samples T-Test | `.runTtestPS` | `pwr::pwr.t.test(type="paired")` | Active |
| `oneSampleTTest` | One Sample T-Test | `.runTtestOneS` | `pwr::pwr.t.test(type="one.sample")` | Active |
| `oneSampleZTest` | One Sample Z-Test | `.runZtestOneS` | `pwr::pwr.norm.test` | Active |
| `oneSampleProportion` | One Sample Proportion Test | `.runTest1P` | `.pwrPTest` | Active |
| `twoSamplesProportion` | Two Samples Proportion Test | `.runTest2P` | `.pwr2P2NTest` | Active |
| `oneSampleVarianceRatio` | One Sample Variance Ratio Test | `.runTest1Var` | `.pwrVarTest` | Active |
| `twoSamplesVarianceRatio` | Two Samples Variance Ratio Test | `.runTest2Var` | `.pwr2Var2NTest` | Active |
| `oneSamplePoisson` | One Sample Poisson Rate Test | `.runTest1Pois` | `.pwrPoisTest` | Hidden/inconsistent |
| `twoSamplesPoisson` | Two Samples Poisson Rate Test | `.runTest2Pois` | `.pwr2Pois2NTest` | Hidden/inconsistent |

Poisson rows are hidden in `inst/qml/Power.qml` and dispatcher behavior is currently inconsistent; see [OPEN_QUESTIONS.md](./OPEN_QUESTIONS.md).

## Shared Option Families

All active UI analyses share:

- `calculation`: `sampleSize`, `power`, `effectSize`
- `alpha`
- `sampleSize` (single-N input; two-group designs derive `N2` via ratio)
- `alternative`
- plot toggles: `powerContour`, `powerDemonstration`, `powerByEffectSize`, `powerBySampleSize`, `text`

Design-specific options:

- T/Z: `effectSize`
- Proportion: `baselineProportion`, `comparisonProportion`, computed Cohen's h
- Variance ratio: `varianceRatio` (copied into `effectSize` in `.checkOptions`)
- Unequal-group designs: `sampleSizeRatio`

## Per-Analysis Notes

### Independent Samples T-Test

- File: `R/pwr_ttestis.R`
- Solves unequal-group case using custom helpers in `commonPower.R`.
- Main table uses `N1`, `N2`, `|delta|`, `Power`, `alpha`.
- Includes synthetic dataset generation for two groups.

### Paired Samples T-Test

- File: `R/pwr_ttestps.R`
- Uses `pwr::pwr.t.test(..., type = "paired")`.
- Single `N` table layout.
- Synthetic dataset generation included.

### One Sample T-Test

- File: `R/pwr_ttestos.R`
- Uses `pwr::pwr.t.test(..., type = "one.sample")`.
- Single `N` table layout.
- Synthetic dataset generation included.

### One Sample Z-Test

- File: `R/pwr_z.R`
- Uses `pwr::pwr.norm.test`.
- Same output pattern as one-sample t-test.
- Synthetic dataset generation included.

### One Sample Proportion Test

- File: `R/pwr_1p.R`
- Uses arcsine-transformed effect size (`h`) through `.pwrPTest`.
- Supports directional handling for two-sided effect-size solving via `effectDirection` option.
- Synthetic dataset generator exists in R.

### Two Samples Proportion Test

- File: `R/pwr_2p.R`
- Uses `.pwr2P2NTest` with group-size ratio.
- Returns `N1`, `N2`, `p1`, `p2`, effect-size (`h`), power, alpha.
- Synthetic dataset generator exists in R.

### One Sample Variance Ratio Test

- File: `R/pwr_1var.R`
- Uses variance ratio `rho` as effect metric.
- Text and plot labels switch terminology from effect size to variance ratio.
- Synthetic dataset generation included.

### Two Samples Variance Ratio Test

- File: `R/pwr_2var.R`
- Uses `.pwr2Var2NTest` with sample-size ratio.
- Similar structure to independent t-test, but with variance ratio effect metric.
- Synthetic dataset generation included.

### Poisson Tests (Hidden)

- Files: `R/pwr_1pois.R`, `R/pwr_2pois.R`
- Intended solvers: `.pwrPoisTest`, `.pwr2Pois2NTest`.
- Implementation appears mid-migration (legacy option names and inconsistencies).
- Not exposed in active UI.

## Option Matrix (UI-exposed analyses)

| Option | T tests | Z | 1 proportion | 2 proportions | 1 variance ratio | 2 variance ratio |
| --- | --- | --- | --- | --- | --- | --- |
| `effectSize` input | Yes | Yes | Computed/derived | Computed/derived | Backed by `varianceRatio` | Backed by `varianceRatio` |
| `baselineProportion` | No | No | Yes | Yes | No | No |
| `comparisonProportion` | No | No | Yes | Yes | No | No |
| `varianceRatio` | No | No | No | No | Yes | Yes |
| `sampleSizeRatio` | independent only | No | No | Yes | No | Yes |
| `effectDirection` (for two-sided effect-size solve) | No | No | Yes | Yes | Yes | Yes |
| `powerContour`/`curve`/`dist` | Yes | Yes | Yes | Yes | Yes | Yes |

## Cross-Checks

- UI options source: `inst/qml/Power.qml`
- Routing source: `R/Power.R`
- Per-analysis implementation: `R/pwr_*.R`
- Test coverage: `tests/testthat/test-Power.R`
