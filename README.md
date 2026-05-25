<div align="right">

[![Unit Tests](https://github.com/jasp-stats/jaspPower/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspPower/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspPower/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspPower)

<br>
<b>Maintainer:</b> <a href="https://github.com/FBartos/">František Bartoš</a>

</div>

# The Power Module

## Overview

<img src='inst/icons/power.svg' width='149' height='173' align='right'/>

**JASP Power module** is an add-on module for JASP that provides tools for planning studies under classical and Bayesian evidence criteria. The module supports fixed-sample classical power analysis, classical group-sequential design (GSD), fixed-sample Bayes factor design analysis, and sequential Bayes factor design analysis. It can be used to calculate required sample sizes, achieved power or evidence probabilities, detectable effect sizes, interim stopping boundaries, and expected sample sizes under planned designs.

The module includes classical power analyses for common t-tests, z-tests, proportion tests, and variance-ratio tests. It also provides group-sequential designs with efficacy and futility boundaries, multiple endpoint scales, and look-by-look operating characteristics. The Bayesian analyses support Bayes factor planning for fixed and sequential designs, including analysis and design priors, evidence thresholds, probabilities of conclusive or misleading evidence, prior and decision-probability plots, and optional generated reports.

## R Packages

<img src='https://www.r-project.org/logo/Rlogo.svg' width='100' height='78' align='right' alt='R logo'/>

The power and design functionality is served by several R packages:

- **pwr** - Classical fixed-sample power calculations ([pwr on CRAN](https://cran.r-project.org/package=pwr))
- **gsDesign** - Classical group-sequential design calculations ([gsDesign on CRAN](https://cran.r-project.org/package=gsDesign))
- **bfpwr** - Power and sample-size calculations for Bayes factor analyses ([bfpwr on CRAN](https://cran.r-project.org/package=bfpwr))

The classical power module is originally based upon [richarddmorey/jpower](https://github.com/richarddmorey/jpower/commit/3825ec1c368669c3cb1168e292b465e1d5141a2f).

## Analyses

The organization of the analyses within the Power module in JASP is as follows:

```
--- Power
    -- Classical
       - Power
       - Group Sequential Design
    -- Bayesian
       - Bayes Factor Design
       - Bayes Factor Sequential Design
```
