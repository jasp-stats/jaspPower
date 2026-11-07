# jaspPower Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspPower (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.).
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspPower 0.98.0
## Added
* One-sided analysis priors with a point null for z-test Bayes factor designs (normal, point, and normal-moment priors), in fixed and sequential designs and in the observed-data analysis.
* Sequential z-test designs with a one-sided normal prior can use either a point null or the directional null (θ ≤ θ₀ vs. θ > θ₀).
* Integration grid points setting for the log-scale MVN integration of sequential designs.

## Changed
* Updated bfpwr to version 0.3 (commit `cf36a7f`): sequential sample-size searches use the new bfpwr interface, z-test sequential designs are computed and reported on the original parameter scale, and results change slightly due to more accurate numerical defaults.
* Fixed-sample z-test designs now use the prior direction (default: greater). Previously the direction was hidden for z-tests and the calculations were always two-sided.
* One sample, paired, and independent samples z-tests have a Scale setting. Standardized (default) plans and analyses the standardized mean difference (Cohen's d) like the t-tests, with the standard deviation treated as known; observed data take the same inputs as the t-tests and use the observed standard deviation. Raw keeps the previous behaviour: mean (differences) in the units of the outcome with a known standard deviation. Breaking change: z-test analyses saved with a known standard deviation other than 1 must select Raw.
* Independent-samples z-test designs use the exact allocation ratio in all calculations; the reported N₂ is rounded up.
* "Design sample size based on" only changes the displayed results and no longer reruns the sample-size searches.
* Normal-moment analysis priors default to a mode of 0.5 (mode 0.5, or spread 0.354 = 0.5/√2), matching the default point analysis prior and design priors.
* Power analysis: the effect direction of the synthetic dataset now defaults to greater (x̄₁ > x̄₂ or x̄ > μ₀), matching the positive effect size and the one-sided alternative, like the other direction defaults in the module.
* Clearer labels: normal-moment priors list their actual mode(s), the t-test critical value search range names its scale, and "Exact integration over all regions" is only enabled where it has an effect.

## Fixed
* Fixed-sample t-test designs with a nonzero null value no longer shift the analysis prior location twice.
* Fixed-sample sample-size searches: targets already met at the minimum sample size, point analysis priors outside the sample-size range, and too small independent z-test sample sizes.
* Results that depend on an unreached planning target (combined sample size, decision probabilities, report, plots) are left empty or explained instead of showing values at the search bound.
* Sequential directional "less" designs show the stopping boundaries on the original z scale (they were mirrored).
* Sequential independent z-test designs no longer fail for small allocation ratios, and the sample-size search shows bfpwr's diagnostic when it fails.
* Observed-data Bayes factors are computed on the log scale, so very strong evidence still yields a decision; a binomial success value that does not occur in the data is reported.
* The report states the sample size each probability was evaluated at for every "design sample size based on" choice, and the sequential report gives the look schedule of each design prior.
* The fixed-design minimum/maximum sample-size fields stay ordered (a QML error prevented the update).
* The analysis prior parameter fields line up with each other and with the dropdowns (each row was laid out separately).
* The observed-data variable selection is half as tall and lists only the variables the selected test uses, starting at the top.
* Group sequential designs with invalid inputs show the validation message instead of a fatal plot error.
* Subjects + events survival designs reject a null hazard ratio other than 1 from the R interface instead of ignoring it.
* Equal alternative and null hazard ratios in survival group sequential designs give a clear message instead of a uniroot error.
* Subjects + events designs with exponential entry explain non-finite sample sizes (γ = 0 or γ equal to a group's event-plus-dropout hazard).
* The integration grid setting is disabled for group sequential survival accrual designs, where gsDesign ignores it, and no longer appears in their R code.
* Grammar of the group sequential design explanation.
* The binary-endpoint help no longer claims that a baseline event probability of 0.5 is conservative for group sequential designs.

---

# jaspPower 0.97.1
## Changed
* Simplified general z-test Bayes factor designs to use a specified unit information standard deviation (UISD), with sample-size schedules for sequential designs.
* Clarified sequential-design explanations and stopping-boundary labels, and included UISD in general z-test reports.

## Fixed
* Improved precision and spending-function formatting in generated R code.
* Improved prior-plot legends.
* Corrected a Spanish translation placeholder.

---

# jaspPower 0.97
## Added
* Group Sequential Design
* Bayes Factor Design
* Bayes Factor Sequential Design

---


# jaspPower 0.96.6
## Added
* Fixed a bug in the power demonstration graph of the one sample z-test. Now n correctly determines the precision (width) of the distributions, making the visual consistent with the numerical result.  
