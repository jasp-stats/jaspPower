# Architecture

## High-Level Layers

1. Module registration
   - `inst/Description.qml`
   - Declares one analysis (`Power`) mapped to R function `Power`.

2. UI/options layer
   - `inst/qml/Power.qml`
   - Declares all controls and option names used in R.

3. Dispatcher and option validation
   - `R/Power.R`
   - `Power(...)` routes `options$test` to test-specific runner.
   - `.checkOptions(...)` normalizes and validates some combinations.

4. Shared R helpers
   - `R/commonPower.R`
   - `R/commonPowerPlotting.R`
   - Contains option normalization (`.prepareStats`), intro text, solver wrappers, and plotting helpers.

5. Analysis-specific pipelines
   - `R/pwr_ttestis.R`
   - `R/pwr_ttestps.R`
   - `R/pwr_ttestos.R`
   - `R/pwr_z.R`
   - `R/pwr_1p.R`
   - `R/pwr_2p.R`
   - `R/pwr_1var.R`
   - `R/pwr_2var.R`
   - `R/pwr_1pois.R`
   - `R/pwr_2pois.R`

6. Regression tests
   - `tests/testthat/test-Power.R`
   - Snapshot assets in `tests/testthat/_snaps/Power/`.

## End-to-End Execution Flow

### 1) Entry and dispatch

- JASP calls exported `Power(jaspResults, dataset, options)`.
- `Power` applies `.checkOptions(options)`.
- Then branch-dispatches by `options$test`.

### 2) Analysis run function pattern

Each analysis file follows a consistent top-level sequence:

1. `stats <- .prepareStats(options)`
2. `results <- try(.compute...)`
3. `.checkResults(results)`
4. `.initPowerTab...`
5. Optional `.initPowerESTab...` when `options$text`
6. `.populateIntro(...)`
7. Optional plot preparation + explanatory text
8. Optional synthetic dataset generation

This pattern appears in all `.run*` functions (`.runTtestIS`, `.runTest1P`, `.runTest2Var`, etc.).

### 3) Calculation strategy

Most analyses implement three branches keyed by `options$calculation`:

- `sampleSize`: solve for `n`
- `effectSize`: solve for effect parameter (`d`, `p1`, `rho`, etc.)
- `power`: solve for power

Shared solver helpers in `commonPower.R` wrap or extend `pwr` behavior, especially for:

- unequal group sizes (`.pwrT2NTest`, `.pwrT2NRatio`)
- proportion tests (`.pwrPTest`, `.pwr2P2NTest`)
- variance ratio tests (`.pwrVarTest`, `.pwr2Var2NTest`)
- Poisson rate tests (`.pwrPoisTest`, `.pwr2Pois2NTest`)

### 4) Output assembly model

`jaspResults` accumulates typed JASP objects:

- `createJaspTable(...)`
- `createJaspPlot(...)`
- `createJaspHtml(...)`
- `createJaspContainer(...)`

Each object registers option dependencies via `$dependOn(...)` so JASP invalidates and recomputes output when inputs change.

### 5) Plot generation model

- Analysis functions build numeric `state` lists (grids, curves, limits, target values).
- Shared plot constructors in `commonPowerPlotting.R` convert state to `ggplot2` objects:
  - `.plotPowerContour`
  - `.plotPowerCurveES`
  - `.plotPowerCurveN`
  - `.plotPowerDist`
- Theme/palette defaults are centralized in `.pwrPlotDefaultSettings`.

### 6) Synthetic data export model

Implemented in most non-Poisson analyses as `.generateDataset*` functions.

Common behavior:

- validate `savePath` extension (`.csv` or `.txt` in most files)
- optionally apply seed (`setSeed` + `seed`)
- write dataset to disk (typically via `write.csv`)
- add generated data and metadata tables to `datasetcont`

## Architectural Consistency Notes

Strong consistency:

- file naming and function naming convention
- shared run/compute/init/populate/prepare/generate pipeline
- standardized output object ids in `jaspResults`

Observed inconsistencies:

- Poisson code uses a different option naming scheme than QML/common path in many places (`calc` vs `calculation`, `p0/p1/n` vs `baselineProportion/comparisonProportion/sampleSize`).
- Dispatcher maps Poisson tests to `.runTest2Var` instead of Poisson runners.

See [OPEN_QUESTIONS.md](./OPEN_QUESTIONS.md).

## Related Docs

- R function map: [R_CODE_MAP.md](./R_CODE_MAP.md)
- UI bindings: [UI_QML_MAP.md](./UI_QML_MAP.md)
- Analysis-by-analysis behavior: [ANALYSES.md](./ANALYSES.md)
