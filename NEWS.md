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

## Changed
* Updated bfpwr to version 0.3 (commit `cf36a7f`): sequential sample-size searches use the new bfpwr interface, z-test sequential designs are computed and reported on the original parameter scale, and results change slightly due to more accurate numerical defaults.
* Fixed-sample z-test designs now use the prior direction (default: greater). Previously the direction was hidden for z-tests and the calculations were always two-sided.
* One sample, paired, and independent samples z-tests have a Scale setting. Standardized (default) plans and analyses the standardized mean difference (Cohen's d) like the t-tests, with the standard deviation treated as known; observed data take the same inputs as the t-tests and use the observed standard deviation. Raw keeps the previous behaviour: mean (differences) in the units of the outcome with a known standard deviation. Breaking change: z-test analyses saved with a known standard deviation other than 1 must select Raw.
* Normal-moment analysis priors default to a mode of 0.5 (mode 0.5, or spread 0.354 = 0.5/√2), matching the default point analysis prior and design priors.
* Power analysis: the effect direction of the synthetic dataset now defaults to greater (x̄₁ > x̄₂ or x̄ > μ₀), matching the positive effect size and the one-sided alternative, like the other direction defaults in the module.

## Fixed
* GUI and backend improvements and fixes

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
