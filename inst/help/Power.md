Power Analysis
==========================

The module "Power Analysis" allows you to conduct analyses of statistical power. In statistics, power refers to the 'ability of a test to detect an effect of a particular size' (Field et al., 2012). The module allows you to compute (1) the necessary sample size to achieve a given power, (2) the power of detecting a particular effect, given a set sample size and (3) the minimum effect size, that could be detected with a given power and sample size.

This module is based upon [jpower](https://github.com/richarddmorey/jpower) by Richard Moorey.

### Input
-------

- Statistical Test: The statistical test for which to conduct the power analysis.

#### Parameters (t-tests)
- I want to calculate the ...
  - Sample Size N: Calculate the necessary sample size to achieve a given power.
  - Power: Calculate the power of detecting a particular effect, given a set sample size and effect size.
  - Effect size: Calculate the minimum effect size, that could be detected with a given power and sample size.
- Minimal effect size of interest (δ): The minimal effect size using Cohen's d, that would be interesting to detect.
- Minimum desired power (1-β): The minimum desired probability of detecting an effect (statistical power). β refers to the probability of conducting a Type I error (false negative), therefore power is defined as the opposite i.e. 1-β.
- Sample size / Sample size per group (N): The given sample size.
- Sample size ratio (N₁/N₂): Ratio between first and second groups sample size (independent samples t-test only).
- Type I error rate (α): The Type I error rate (false positive) threshold which will be used when running tests later.
- Alternative Hypothesis: Whether a one-sided or two-sided hypothesis will be tested.

#### Display
- Power contour plot: Include a power contour plot in results? (see Output for detailed explanation)
- Power curve by effect size: Include a plot showing the power curve for different values of effect size in results? (see Output for detailed explanation)
- Power curve by N: Include a plot showing the power curve for different values of sample size in results? (see Output for detailed explanation)
- Power demonstration: Include a explanatory demonstration of power in results?
- Explanatory text: Should explanatory text and additional information be shown with the results?

### Output
-------

#### Introduction
- A Priori Power Analysis (table): This table shows the statistic (sample size, power or effect size) the user chose to calculate alongside the user-defined values they entered.
- Power by Effect Size (table): This table illustrates how statistical power would change with different effect sizes. (Only shown when explanatory text is enabled)

#### Plots
- Power Contour: This plot illustrates how power changes depending on the combination of sample size (X-Axis) and effect size (Y-Axis). Statistical power is denoted by color, the black curved line corresponds to user-defined value of power.
- Power Curve Plots
  - Power Curve by Effect Size: This curve illustrates how statistical power changes for different values of effect size. The dashed lines highlight the user-defined / calculated values of effect size and power.
  - Power Curve by N: This curve illustrates how statistical power changes for different values of sample size. The dashed lines highlight the user-defined / calculated values of sample size and power.
- Power Demonstration: This plot demonstrates how and why effect size and statistical power are related to each other.

### References
-------
- Morey, R. (2021). Jpower [R]. https://github.com/richarddmorey/jpower (Original work published 2017)
- Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using R. Sage.

### R Packages
---
- pwr
- ggplot2

