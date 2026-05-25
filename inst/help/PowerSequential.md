Group Sequential Design
=======================

The "Group Sequential Design" analysis computes classical frequentist group sequential designs using the `gsDesign` R package. It supports one-sided designs, symmetric two-sided designs, and asymmetric two-sided β-spending designs with binding or non-binding futility bounds.

### Input
-------

#### Design
- Design type: Selects one-sided, symmetric two-sided, or asymmetric two-sided β-spending designs. For asymmetric designs, binding or non-binding controls how futility-boundary crossings are treated for Type I error computation.

#### Parameters
- Number of looks: Number of planned analyses, including the final analysis.
- Type I error rate (α): One-sided upper-bound α. For symmetric two-sided designs, the total Type I error rate is 2α.
- Target power (1 - β): Target power under the alternative hypothesis. For asymmetric β-spending designs, β is also the total lower-bound spending under the alternative.
- Design scale:
  - Information ratio: Reports information ratios relative to a fixed design.
  - Fixed design value: Uses the fixed-design information, sample size, or event count with no interim analyses as the scale for the group sequential design.
  - Information-scale effect (δ): Uses the selected effect scale to derive the canonical information-scale effect. δ is the signal-to-noise ratio for one unit of information. It is not generally Cohen's d.
  - Canonical effect (δ): Uses the value directly as the canonical information-scale effect.
  - Standardized mean difference (one sample / paired): Treats the design as a one-sample normal design. For paired designs, this is the mean paired difference divided by the SD of paired differences.
  - Mean difference (one sample / paired): Computes `delta = mean difference / SD`, where SD is the outcome SD for a one-sample design or the SD of paired differences for a paired design.
  - Standardized mean difference or mean difference (independent samples): Computes the fixed total sample size using the allocation ratio. The table values labeled total sample size are observations across both groups. The independent-samples standardized mean difference is not the same as canonical δ; with equal allocation, its canonical δ would be half the standardized mean difference.
  - Two-sample binary endpoint: Computes the fixed total sample size from either directly entered event probabilities p1 and p2, or from a baseline event probability p1 and an alternative effect. Direct p1/p2 input defaults to p1 = 0.5 and p2 = 0.4 regardless of the selected effect scale. Baseline + effect input defaults to p1 = 0.5; this is conservative for risk-difference planning because it maximizes Bernoulli variance. The default ratio-scale example effect is 1.3 for RR and OR. Enter RR0 and OR0 on the ratio scale; equality is 1.
  - Survival hazard ratio: Uses an experimental/control hazard ratio. Events-only mode computes required events only. Subjects + events mode also estimates expected enrolled subjects from accrual assumptions and supports HR0 = 1 only. Group sequential accrual mode computes the survival group sequential design directly from accrual and study-time assumptions and supports non-null HR0. The default HR example effect is 1.3.
  - Survival accrual inputs: Control hazard, dropout hazard, accrual duration, study duration, minimum follow-up, and accrual-rate inputs must use the same time unit. HR < 1 indicates a lower experimental hazard when lower hazards are beneficial. In survival modes, information means events, not enrolled participants. The control-hazard and study-duration defaults follow the selected survival information method.
- Look schedule: Equally spaced information or a custom increasing schedule of information fractions. This is information time, not calendar time. Custom schedules must contain K values ending in 1, or K - 1 interim values strictly below 1. The default custom schedule is generated from the number of looks.

#### Boundaries
- Upper boundary: Efficacy boundary family.
- Upper boundary parameter: Parameter for Wang-Tsiatis or spending-function boundaries. Wang-Tsiatis defaults to Δ = 0.25. Hwang-Shih-DeCani uses γ in [-40, 40] and defaults to γ = -4 for the upper boundary. Kim-DeMets power uses ρ in (0, 50] and defaults to ρ = 1.
- Lower boundary: Futility boundary family for asymmetric two-sided designs.
- Lower boundary parameter: Parameter for the lower spending function. Hwang-Shih-DeCani uses γ in [-40, 40] and defaults to γ = -2 for the lower boundary. Kim-DeMets power uses ρ in (0, 50] and defaults to ρ = 1.
- For asymmetric designs, O'Brien-Fleming and Pocock choices are implemented as Lan-DeMets spending functions.

#### Tables
- Design summary: Display the main design summary and effect scale conversions when applicable.
- Sample size / event details: Display endpoint-specific allocation, subject, and analysis-time details when available.
- Look schedule and stopping boundaries: Display the planned information schedule, Z-boundaries, and one-sided nominal p-values by look.
- Boundary crossing probabilities: Display final, cumulative, and stagewise crossing probabilities under H₀ and H₁.

#### Plots
- Stopping boundaries: Display stopping boundaries across planned analyses.
- Boundary crossing probabilities: Display cumulative boundary crossing probabilities under H₀ and H₁.
- Explanatory text: Display a short explanation of the design.

#### Advanced Options
- Generate R Code: Display copyable R code for reproducing the current settings.
- Integration grid points: Integer from 1 to 80 controlling the numerical integration grid. The default is 18. Larger values can improve numerical integration accuracy but slow computation.
- Scope: The interface exposes the common design inputs for one-sided and two-sided designs, α, β, information timing, upper and lower boundary families, numerical integration grid points, and selected normal, binary, and survival endpoint calculations. The survival accrual interface is scalar/single-pattern: piecewise hazards, strata, separate experimental dropout hazards, and user-supplied upper/lower crossing times are not exposed. More specialized design arguments such as additional two-sided design types, overrun, and manually specified information at each look are not exposed.

### Output
-------

- Group Sequential Design: Main summary of the requested design, planned looks, target error rates, planning metric, fixed-design scale, maximum group sequential scale, expected value under H₀ and H₁, and inflation ratio.
- Sample Size / Event Details: Endpoint-specific detail that is not already in the main summary. For independent-samples and binary designs, this table splits total sample size by group using the allocation ratio. For survival accrual designs, it reports fixed-design and maximum/final enrolled-subject and analysis-time quantities derived from the accrual assumptions. The table is omitted when the design has no additional endpoint-specific detail.
- Final Boundary Crossing Probabilities: Final cumulative probabilities of crossing the efficacy bound, crossing the lower efficacy or futility bound when relevant, and reaching the final look without a boundary crossing.
- Effect Scale Conversions: Shown only when the selected endpoint scale is mapped or converted before constructing the group sequential design. The table reports the canonical δ used by the sequential calculations and, for raw mean-difference inputs, the corresponding standardized mean difference.
- Look Schedule: Planned information fraction and planned information, total sample size, or events at each look. In group sequential accrual survival mode, this table also reports expected analysis time, expected enrolled subjects, and expected accrued events by look. Expected analysis time is measured from trial start and uses the same time unit as the survival accrual inputs.
- Stopping Boundaries: Relevant Z-boundaries and lower-tail or upper-tail nominal p-values by look. One-sided designs show only the efficacy bound. Symmetric two-sided designs label the lower bound as a lower efficacy bound; asymmetric designs label it as a futility bound. Table footnotes summarize the information fractions, error-rate targets, and boundary families used to construct the cutoffs.
- Cumulative Boundary Crossing Probabilities by Look: Cumulative crossing probabilities through each look, with H₀ and H₁ shown as column groups.
- New Boundary Crossings by Look: Stagewise probabilities for boundary crossings that first occur at each look, with H₀ and H₁ shown as column groups.
- Non-Binding Type I Error Accounting: Shown only for non-binding futility designs. These probabilities evaluate Type I error for the efficacy rule when crossing the futility bound would not force stopping.
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
