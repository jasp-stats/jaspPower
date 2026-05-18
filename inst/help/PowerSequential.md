Group Sequential Design
=======================

The "Group Sequential Design" analysis computes classical frequentist group sequential designs using the `gsDesign` R package. It supports one-sided, symmetric two-sided, and asymmetric two-sided designs with binding or non-binding futility bounds.

### Input
-------

#### Design
- Design type: Selects the group sequential design type.
- Number of looks: Number of planned analyses, including the final analysis.
- Type I error rate: One-sided alpha used by `gsDesign`.
- Power: Target power under the alternative hypothesis.
- Sample size input:
  - Generic information ratio: Reports information ratios relative to a fixed design.
  - Fixed design sample size: Scales the group sequential design from a fixed design sample size.
  - Standardized effect size: Computes the fixed and group sequential information from a standardized effect size.
- Look schedule: Equally spaced information or a custom increasing schedule of information fractions.

#### Boundaries
- Upper boundary: Efficacy boundary family.
- Upper boundary parameter: Parameter for Wang-Tsiatis or spending-function boundaries.
- Lower boundary: Futility boundary family for asymmetric two-sided designs.
- Lower boundary parameter: Parameter for the lower spending function.
- For asymmetric designs, O'Brien-Fleming and Pocock choices are implemented as spending functions.

#### Plots
- Stopping boundaries: Display stopping boundaries across planned analyses.
- Boundary crossing probabilities: Display cumulative boundary crossing probabilities under H0 and H1.
- Explanatory text: Display a short explanation of the design.

#### Advanced Options
- Generate R Code: Display the `gsDesign::gsDesign()` call for the current settings.

### Output
-------

- Group Sequential Design: Summary of the requested design, target error rates, effect size, fixed design information, maximum group sequential information, and boundary families.
- Stopping Boundaries: Boundary values, nominal p-values, and error spending by look.
- Boundary Crossing Probabilities: Stagewise and cumulative boundary crossing probabilities under H0 and H1, including the non-binding upper-bound Type I error curve for non-binding futility designs.
- Stopping Boundaries plot: Z-boundaries by information fraction.
- Boundary Crossing Probabilities plot: Cumulative stopping probabilities by look.
- R Code: Copyable R code for reproducing the main `gsDesign` call.

### References
-------

- Anderson, K. M. (2026). gsDesign: Group Sequential Design. R package.
- Jennison, C., & Turnbull, B. W. (2000). Group sequential methods with applications to clinical trials. Chapman and Hall.

### R Packages
---
- gsDesign
- ggplot2
