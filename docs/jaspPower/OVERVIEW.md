# Overview

## Module Purpose

`jaspPower` provides one analysis (`Power`) that computes one of three targets:

- required sample size
- achieved power
- minimum detectable effect size

The module is registered in `inst/Description.qml` with `func: "Power"` and no data requirement (`requiresData: false`).

## User-Facing Surface

The UI is defined in `inst/qml/Power.qml` and currently exposes 8 statistical test modes:

1. Independent Samples T-Test (`independentSamplesTTest`)
2. Paired Samples T-Test (`pairedSamplesTTest`)
3. One Sample T-Test (`oneSampleTTest`)
4. One Sample Z-Test (`oneSampleZTest`)
5. One Sample Proportion Test (`oneSampleProportion`)
6. Two Samples Proportion Test (`twoSamplesProportion`)
7. One Sample Variance Ratio Test (`oneSampleVarianceRatio`)
8. Two Samples Variance Ratio Test (`twoSamplesVarianceRatio`)

Poisson tests are present in R (`oneSamplePoisson`, `twoSamplesPoisson`) but commented out in the QML dropdown.

## Shared Workflow (User Perspective)

1. Select test family.
2. Choose calculation target (`sampleSize`, `power`, `effectSize`).
3. Enter design/effect/alpha parameters.
4. Choose plot outputs:
   - contour
   - curve by effect size
   - curve by sample size
   - power demonstration
   - explanatory text
5. Optionally export synthetic dataset (UI currently hidden for proportion tests).

## Output Types

Common outputs across analyses:

- Introductory explanatory HTML block (`intro`)
- Main A Priori table (`powertab`)
- Optional "power by effect size" style table (`powerEStab`)
- Optional explanatory HTML per plot/table
- Optional plots (`powerContour`, `powerCurveES`, `powerCurveN`, `powerDist`)
- Optional synthetic dataset container (`datasetcont`)

## Dependencies

From `DESCRIPTION`:

- `pwr`
- `viridis`
- `jaspBase`
- `jaspGraphs`

## Known Scope Boundaries

- ANOVA is not implemented (placeholder in QML and dispatcher).
- Poisson support is partially implemented but inconsistent (see [OPEN_QUESTIONS.md](./OPEN_QUESTIONS.md)).

## Related Docs

- Architecture and data flow: [ARCHITECTURE.md](./ARCHITECTURE.md)
- Analysis-level details: [ANALYSES.md](./ANALYSES.md)
- UI bindings: [UI_QML_MAP.md](./UI_QML_MAP.md)
