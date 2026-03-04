# UI QML Map

Primary file: `inst/qml/Power.qml`

## Top-Level Structure

`Form` contains 4 main blocks:

1. Test selector (`DropDown name: "test"`)
2. Parameters section
3. Plots section
4. Data Generation section

## Control Inventory

### Test Selection

- `test` (`DropDown`)
  - Visible values: independent/paired/one-sample t, one-sample z, one/two proportion, one/two variance ratio.
  - Poisson and ANOVA entries are commented out.

### Parameters Section

- `calculation` (`DropDown`): sample size, power, effect size.
- `effectDirection` (`DropDown`): only shown for proportion/variance analyses when solving effect size under two-sided alternative.
- `baselineProportion` (`DoubleField`): proportion tests only.
- `comparisonProportion` (`DoubleField`): proportion tests only.
- `effectSize` (`DoubleField`): t/z tests.
- `varianceRatio` (`DoubleField`): variance-ratio tests.
- `power` (`DoubleField`)
- `alpha` (`DoubleField`)
- `sampleSize` (`IntegerField`)
- `sampleSizeRatio` (`DoubleField`): independent t, two-proportion, two-variance modes.
- `alternative` (`DropDown`)
  - t/z: two-sided or greater
  - proportion/variance: two-sided, less, greater

### Plots Section

Boolean toggles:

- `powerContour`
- `powerDemonstration`
- `powerByEffectSize`
- `powerBySampleSize`
- `text`

### Data Generation Section

`Group id: parameters` is hidden for proportion tests (`test.currentIndex != 4 && test.currentIndex != 5`).

Fields:

- `firstGroupMean`
- `secondGroupMean`
- `testValue`
- `firstGroupSd`
- `populationSd`
- `secondGroupSd`
- `effectDirectionSyntheticDataset` (`RadioButtonGroup`)
- `savePath` (`FileSelector`)
- `saveDataset` (`CheckBox`)
- `SetSeed{}` (emits `setSeed` and `seed` options)

## Visibility and Binding Patterns

1. Branching by `test.currentIndex`
   - Most field visibility logic is index-based (not value-based).
   - This is brittle if test ordering changes.

2. Dynamic labels
   - Several labels switch text by test type, including proportion and variance symbols.

3. Calculation-driven enable/disable
   - Inputs disabled when they are the solved target (for example, power disabled when solving power).

4. Data generation mismatch
   - QML hides data generation parameters for proportion tests, while R still has `.generateDatasetTest1P` and `.generateDatasetTest2P`.

## Option Name Contract With R

QML emits these core option names:

- `test`, `calculation`, `effectDirection`, `baselineProportion`, `comparisonProportion`
- `effectSize`, `varianceRatio`, `power`, `alpha`
- `sampleSize`, `sampleSizeRatio`, `alternative`
- `powerContour`, `powerDemonstration`, `powerByEffectSize`, `powerBySampleSize`, `text`
- `firstGroupMean`, `secondGroupMean`, `testValue`, `firstGroupSd`, `populationSd`, `secondGroupSd`
- `effectDirectionSyntheticDataset`, `savePath`, `saveDataset`, `setSeed`, `seed`

Poisson R files use a different naming contract in many places (`calc`, `p0`, `p1`, `n`, `n_ratio`, `alt`), which does not match current QML.

## Related Docs

- Backend option usage: [R_CODE_MAP.md](./R_CODE_MAP.md)
- Behavior by analysis: [ANALYSES.md](./ANALYSES.md)
- Known UI/backend mismatches: [OPEN_QUESTIONS.md](./OPEN_QUESTIONS.md)
