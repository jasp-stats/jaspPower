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

# jaspPower (development version)
## Added
* One-sided analysis priors with a point null for z-test Bayes factor designs (normal, point, and normal-moment priors), in fixed and sequential designs and in the observed-data analysis.
* Sequential z-test designs with a one-sided normal prior can use either a point null or the directional null (θ ≤ θ₀ vs. θ > θ₀).
* Integration grid points setting for the log-scale MVN integration of sequential designs.

## Changed
* Updated bfpwr to version 0.3 (commit `cf36a7f`): sequential sample-size searches use the new bfpwr interface, z-test sequential designs are computed and reported on the original parameter scale, and results change slightly due to more accurate numerical defaults.

## Fixed
* Fixed-sample t-test designs with a nonzero null value no longer shift the analysis prior location twice.

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
