# Testing

## Test Assets

Primary test file:

- `tests/testthat/test-Power.R`

Snapshot directory:

- `tests/testthat/_snaps/Power/`

CI workflows:

- `.github/workflows/unittests.yml`
- `.github/workflows/i18nCheck.yml`
- `.github/workflows/translations.yml`

## What Current Tests Cover

`test-Power.R` includes:

1. Numeric sanity checks for each currently UI-visible analysis
   - independent t
   - paired t
   - one-sample t
   - one-sample z
   - one-sample proportion
   - two-sample proportion
   - one-sample variance ratio
   - two-sample variance ratio

2. Snapshot plot regression checks
   - contour
   - curve by effect size
   - curve by N
   - power demonstration

3. Explanatory table checks for selected scenarios
   - power-by-effect-size table text bins
   - A Priori result table values

4. OS-specific snapshot handling
   - contour snapshots use `mac` or `default` variants.

## Notably Uncovered Areas

- Poisson analyses (`pwr_1pois.R`, `pwr_2pois.R`) are not covered by tests.
- ANOVA path is not implemented/tested.
- Synthetic dataset export paths are not directly asserted in `test-Power.R`.
- Many edge/invalid-option combinations are validated only by runtime errors, not explicit tests.

## Local Test Commands

Typical commands (requires JASP R dependencies such as `jaspBase`, `jaspGraphs`, `jaspTools`):

```bash
Rscript -e "testthat::test_file('tests/testthat/test-Power.R')"
```

Or run all tests:

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

If running through package tooling in a prepared JASP dev environment:

```bash
Rscript -e "devtools::test()"
```

## CI Behavior Summary

- Unit tests run on pushes/PRs with relevant code/test/dependency path filters.
- i18n check runs on pushes/PRs for R/QML/po/yml changes.
- Translation generation runs on schedule and manual dispatch.

## Practical Validation Guidance For Future Changes

1. For UI text/visibility changes, run `test-Power.R` and inspect snapshots.
2. For solver changes, add numeric table assertions per analysis and alternative.
3. For Poisson work, add dedicated tests first to prevent regressions while normalizing option contracts.
