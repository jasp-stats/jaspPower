# Open Questions

This file tracks implementation uncertainties and follow-up probes discovered during source review.

## 1) Poisson dispatch routing is inconsistent

- Evidence:
  - `R/Power.R:21-24` routes both `oneSamplePoisson` and `twoSamplesPoisson` to `.runTest2Var(...)` instead of Poisson runners.
- Question:
  - Should these branches call `.runTest1Pois(...)` and `.runTest2Pois(...)`, or are Poisson analyses intentionally disabled/incomplete?
- Follow-up:
  - Confirm intended behavior with maintainers, then add regression tests for dispatch mapping.

## 2) Poisson tests are hidden in UI but present in backend

- Evidence:
  - `inst/qml/Power.qml:40-41` comments out Poisson test entries.
  - Poisson implementation files exist: `R/pwr_1pois.R`, `R/pwr_2pois.R`.
- Question:
  - Is Poisson support intentionally staged behind a feature flag, or should UI and backend be re-aligned?
- Follow-up:
  - Decide whether to remove dead/incomplete Poisson code or complete and expose it.

## 3) Intro HTML population appears accidentally disabled in most analyses

- Evidence:
  - Several files contain `## Populate tables and plots.populateIntro(jaspResults, options)` on one line, which comments out `.populateIntro(...)`:
    - `R/pwr_ttestis.R:16`
    - `R/pwr_ttestps.R:16`
    - `R/pwr_ttestos.R:16`
    - `R/pwr_z.R:16`
    - `R/pwr_2p.R:16`
    - `R/pwr_1var.R:16`
    - `R/pwr_2var.R:16`
    - `R/pwr_1pois.R:16`
    - `R/pwr_2pois.R:16`
  - `R/pwr_1p.R:16-17` contains the expected two-line form and does call `.populateIntro(...)`.
- Question:
  - Should intro text be shown for all analyses, or only selected ones?
- Follow-up:
  - If intro is intended globally, split comment/call into separate lines and add a test asserting `intro` existence across modes.

## 4) Poisson option contract diverges from QML/common path

- Evidence:
  - UI emits `calculation`, `sampleSize`, `sampleSizeRatio`, `baselineProportion`, `comparisonProportion`, etc. (`inst/qml/Power.qml`).
  - Poisson code heavily references legacy names: `calc`, `p0`, `p1`, `n`, `n_ratio`, `alt` (for example `R/pwr_1pois.R:79-87,95`; `R/pwr_2pois.R:296`).
- Question:
  - Is there an untracked migration layer for Poisson options, or is this code path currently broken?
- Follow-up:
  - Normalize Poisson files to `.prepareStats`/current option naming used by other analyses.

## 5) Poisson one-sample file has unresolved symbol issues

- Evidence:
  - `.populatePowerTabTest1Pois` references `calc` without defining it locally (`R/pwr_1pois.R:497-505`).
  - `.populatePowerESTabTest1Pois` receives `r` but uses `results` (`R/pwr_1pois.R:509-519`).
  - Dataset generation path includes TODO marker before `.generateDatasetTest1Pois(...)` call (`R/pwr_1pois.R:42-44`).
- Question:
  - Are these unfinished refactors, and should Poisson paths be excluded until stabilized?
- Follow-up:
  - Fix symbol usage and add minimal functional tests before enabling Poisson UI exposure.

## 6) Debug output is committed in Poisson two-sample text path

- Evidence:
  - `print("===> pre-calc")`, `print("===> post-calc")`, and `print(probs_es)` in `R/pwr_2pois.R:231-243`.
- Question:
  - Are these temporary diagnostics that should be removed?
- Follow-up:
  - Remove debug prints or gate them behind an explicit debug flag.

## 7) One-proportion function naming contains duplicated suffixes

- Evidence:
  - `R/pwr_1p.R` defines names like:
    - `.populateContourTextTest1PTest1P` (`R/pwr_1p.R:208`)
    - `.populatePowerCurveESTextTest1PTest1P` (`R/pwr_1p.R:229`)
    - `.populatePowerESTabTest1PTest1P` (`R/pwr_1p.R:461`)
- Question:
  - Is duplicated `Test1P` naming intentional for backward compatibility, or accidental drift?
- Follow-up:
  - Consider cleanup/aliasing strategy if renaming to avoid breaking references.

## 8) ANOVA path exists in dispatcher/UI visibility checks but has no implementation

- Evidence:
  - Dispatcher calls `.runAnova(...)` for `test == "anova"` (`R/Power.R:25-26`).
  - ANOVA option is commented out in UI (`inst/qml/Power.qml:42`), but `visible: test.currentValue !== 'anova'` remains (`inst/qml/Power.qml:49`).
  - No `.runAnova` definition exists in `R/*.R`.
- Question:
  - Should ANOVA stubs be removed, or is implementation planned and partially scaffolded?
- Follow-up:
  - Align dispatcher/UI with implemented scope to avoid dead branches.

## 9) Data Generation UI/back-end mismatch for proportion tests

- Evidence:
  - Data generation parameter group hidden for proportion tests via `test.currentIndex != 4 && test.currentIndex != 5` (`inst/qml/Power.qml:330`).
  - Proportion back-end includes dataset generators (`R/pwr_1p.R`, `R/pwr_2p.R`).
- Question:
  - Is hiding proportion data-generation controls intentional due UX concerns, or an accidental UI restriction?
- Follow-up:
  - Clarify intended product behavior and either expose controls or remove unused generation paths.

## 10) Test coverage gaps for unstable/hidden paths

- Evidence:
  - `tests/testthat/test-Power.R` covers visible t/z/proportion/variance modes, but not Poisson and not ANOVA.
- Question:
  - Should Poisson work be blocked from release until minimal tests are added?
- Follow-up:
  - Add dispatch and smoke tests for Poisson once option contracts are normalized.
