# R Code Map

## Exported Entry Point

- `NAMESPACE` exports `Power` only.
- `R/Power.R`:
  - `Power(jaspResults, dataset, options)` dispatches to analysis runner by `options$test`.
  - `.checkOptions(options)` validates alternative/effect constraints for t/proportion/variance families.

## Shared Helper Files

### `R/commonPower.R`

Core responsibilities:

- normalize option payload: `.prepareStats`
- add shared intro HTML: `.populateIntro`
- error handling: `.checkResults`, `.errorMessageUnsolvable`
- contour reshaping: `.transformContourMatrix`
- solver helpers:
  - `.pwrT2NTest`, `.pwrT2NRatio`
  - `.pwrPTest`, `.pwr2P2NTest`
  - `.pwrVarTest`, `.pwr2Var2NTest`
  - `.pwrPoisTest`, `.pwr2Pois2NTest`

### `R/commonPowerPlotting.R`

Core responsibilities:

- global plot defaults: `.pwrPlotDefaultSettings`
- shared segment helper: `.segment`
- plot builders:
  - `.plotPowerContour`
  - `.plotPowerCurveES`
  - `.plotPowerCurveN`
  - `.plotPowerDist`

## Analysis File Function Families

Each analysis file follows the same function groups:

1. `.run...`
2. `.compute...`
3. `.initPowerTab...`
4. `.initPowerESTab...`
5. `.populate...Text...`
6. `.populatePowerTab...` and `.populatePowerESTab...`
7. `.preparePowerContour...`
8. `.preparePowerCurveES...`
9. `.preparePowerCurveN...`
10. `.preparePowerDist...`
11. `.generateDataset...` (not consistently present)

## Per-Analysis Backend Map

| Analysis | File | Runner | Compute | Main solver calls |
| --- | --- | --- | --- | --- |
| Independent t | `R/pwr_ttestis.R` | `.runTtestIS` | `.computeTtestIS` | `.pwrT2NRatio`, `.pwrT2NTest` |
| Paired t | `R/pwr_ttestps.R` | `.runTtestPS` | `.computeTtestPS` | `pwr::pwr.t.test(type="paired")` |
| One-sample t | `R/pwr_ttestos.R` | `.runTtestOneS` | `.computeTtestOneS` | `pwr::pwr.t.test(type="one.sample")` |
| One-sample z | `R/pwr_z.R` | `.runZtestOneS` | `.computeZtestOneS` | `pwr::pwr.norm.test` |
| One proportion | `R/pwr_1p.R` | `.runTest1P` | `.computeTest1P` | `.pwrPTest` |
| Two proportions | `R/pwr_2p.R` | `.runTest2P` | `.computeTest2P` | `.pwr2P2NTest` |
| One variance ratio | `R/pwr_1var.R` | `.runTest1Var` | `.computeTest1Var` | `.pwrVarTest` |
| Two variance ratio | `R/pwr_2var.R` | `.runTest2Var` | `.computeTest2Var` | `.pwr2Var2NTest` |
| One Poisson | `R/pwr_1pois.R` | `.runTest1Pois` | `.computeTest1Pois` | `.pwrPoisTest` |
| Two Poisson | `R/pwr_2pois.R` | `.runTest2Pois` | `.computeTest2Pois` | `.pwr2Pois2NTest` |

## Common `jaspResults` Keys

Most files write these keys:

- `intro`
- `powertab`
- `powerEStab`
- `tabText`
- `powerContour`
- `contourText`
- `powerCurveES`
- `curveESText`
- `powerCurveN`
- `curveNText`
- `powerDist`
- `distText`
- `datasetcont`

Container internals for synthetic data usually include:

- `generatedData`
- `characteristics`
- `posthocpower`

## Dispatcher Mapping Snapshot

Current mapping in `R/Power.R`:

- correct routes for t/z/proportion/variance analyses
- Poisson test values are routed to `.runTest2Var` instead of Poisson runners
- `anova` references `.runAnova`, but ANOVA implementation is not present in this repository

## Naming and Contract Consistency

Consistent families (t/z/proportion/variance non-Poisson):

- use `options$calculation`
- use `sampleSize`, `sampleSizeRatio`, `baselineProportion`, `comparisonProportion`
- convert alternatives with `.prepareStats` (`twoSided` -> `two.sided`)

Poisson files diverge from this contract in many places:

- expect `options$calc`, `options$p0`, `options$p1`, `options$n`, `options$n_ratio`, `options$alt`
- include implementation remnants like debug `print(...)` and unresolved TODO comments

See [OPEN_QUESTIONS.md](./OPEN_QUESTIONS.md).
