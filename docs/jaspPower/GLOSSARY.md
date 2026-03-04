# Glossary

## Module Terms

- Power analysis: Procedure to evaluate design sensitivity for detecting effects.
- A Priori Power Analysis: Main output table showing solved target and user-defined inputs.
- Power by effect size table: Heuristic banding table that maps effect-size ranges to power bands.
- Power contour: 2D surface over sample size and effect size with power encoded by color.
- Power curve by effect size: 1D curve of power as effect size changes with design fixed.
- Power curve by N: 1D curve of power as sample size changes with effect fixed.
- Power demonstration: Distribution plot illustrating alpha, power, and effect separation.

## Option/Parameter Terms

- `calculation`: Which quantity to solve (`sampleSize`, `power`, `effectSize`).
- `alpha`: Type I error rate.
- `alternative`: Alternative hypothesis direction.
- `sampleSize`: Base N input (for two-group designs this is group 1 size).
- `sampleSizeRatio`: Ratio used to derive second-group size.
- `power`: Target or computed statistical power.
- `effectSize`: Generic effect field used mainly for t/z tests.
- `varianceRatio`: Effect input for variance-ratio analyses.
- `baselineProportion`: Null/baseline proportion in proportion tests.
- `comparisonProportion`: Alternative/comparison proportion in proportion tests.
- `effectDirection`: UI hint used when solving effect size under two-sided alternatives (proportion/variance families).

## Effect Metrics

- Cohen's delta (`delta`): standardized mean difference used by t/z implementations.
- Cohen's h (`h`): arcsine-transformed difference between two proportions.
- Variance ratio (`rho`): ratio of variances for variance-ratio tests.
- Poisson ratio/rate parameters (`lambda0`, `lambda1`): baseline and comparison rates in Poisson helpers.

## Internal Object Names

- `jaspResults`: analysis result tree built from tables, plots, html, and state objects.
- `powertab`: main A Priori table object.
- `powerEStab`: effect-size interpretation table object.
- `datasetcont`: synthetic dataset container object.

## Internal Pipeline Terms

- `run` function: top-level analysis-specific orchestrator (for example `.runTtestIS`).
- `compute` function: computes solved values for the selected target.
- `init` function: creates table/plot objects and declares dependencies.
- `populate` function: fills table/html values.
- `prepare` function: builds plot state and returns plot objects.
