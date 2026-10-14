#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Group Sequential Design
#'
#' @param alpha, One-sided upper-bound Type I error rate. For symmetric two-sided designs, the total Type I error rate is 2α.
#' @param binaryAlternativeEffect, Alternative risk difference p1 - p2. With the default baseline event probability 0.5, the default difference 0.1 implies p2 = 0.4.
#' @param binaryBaselineEventRate, Event probability for group 1 under the alternative hypothesis. A value near 0.5 is conservative for risk-difference planning because it gives the largest Bernoulli variance.
#' @param binaryEffectScale, Scale used to derive the second event probability and to set the binary fixed-design sample-size calculation.
#' @param binaryEventRateGroup1, Event probability in group 1 under the alternative hypothesis.
#' @param binaryEventRateGroup2, Event probability in group 2 under the alternative hypothesis.
#' @param binaryInputMode, Choose whether to enter both event probabilities directly or enter a baseline event probability and an alternative effect.
#' @param binaryNullOddsRatio, Null odds ratio. Use 1 for equal event probabilities.
#' @param binaryNullRiskDifference, Null risk difference p1 - p2. Use 0 for equal event probabilities.
#' @param binaryNullRiskRatio, Null risk ratio p1 / p2. Use 1 for equal event probabilities.
#' @param boundariesPlot, Display stopping boundaries across analyses.
#'    Defaults to \code{TRUE}.
#' @param colorPalette, Choose the color palette used for boundaries in plots.
#' @param crossingProbabilitiesPlot, Display cumulative boundary crossing probabilities under H₀ and H₁.
#'    Defaults to \code{FALSE}.
#' @param crossingProbabilitiesTable, Display final, cumulative, and stagewise boundary crossing probabilities under H₀ and H₁.
#'    Defaults to \code{FALSE}.
#' @param designSummary, Display the main design summary and effect scale conversions when applicable.
#'    Defaults to \code{TRUE}.
#' @param designType, Selects one-sided, symmetric two-sided, or asymmetric two-sided β-spending designs. Binding or non-binding controls Type I error computation after a lower-bound crossing.
#' @param effectScale, Scale used to convert the planned effect to the information scale. Endpoint-specific binary and survival choices first compute fixed-design sample size or events.
#' @param effectSize, Canonical information-scale effect. This is not necessarily Cohen's d.
#' @param effectStandardDeviation, For one-sample designs, use the outcome SD; for paired designs, use the SD of paired differences; for independent samples, use the common outcome SD.
#' @param fixedSampleSize, Fixed-design value with no interim analyses. For simple sample-size designs this is N; for information-based or event-driven designs, enter information units or events.
#' @param generateRCode, Display copyable R code for the current settings.
#'    Defaults to \code{FALSE}.
#' @param gridPoints, Integer from 1 to 80 controlling the numerical integration grid. Default is 18; larger values can improve accuracy but slow computation.
#' @param hazardRatio, Experimental/control hazard ratio under the alternative hypothesis. The default example effect is 1.3.
#' @param legendPosition, Choose where the plot legend is shown.
#' @param lowerBoundary, Futility boundary family for asymmetric β-spending designs. Lower-bound spending is computed under the alternative hypothesis.
#' @param lowerBoundaryParameter, γ parameter for Hwang-Shih-DeCani spending; allowable range is [-40, 40]. Default is -4 for the upper boundary and -2 for the lower boundary.
#' @param naturalEffectSize, Absolute effect on the natural scale. Direction is ignored for information planning.
#' @param nullHazardRatio, Hazard ratio specified by the null hypothesis.
#' @param numberOfLooks, Number of planned analyses, including the final analysis.
#' @param power, Target power under the alternative hypothesis. gsDesign uses β = 1 - power and adjusts the maximum information to achieve this power; for asymmetric β-spending designs, β is also the total lower-bound spending under the alternative.
#' @param sampleSizeAllocationRatio, Ratio of group 2 to group 1 sample size. For survival endpoints, treat group 2 as experimental and group 1 as control.
#' @param sampleSizeMode, Controls whether the design is scaled by relative information, a fixed-design value, or an effect-specific sample-size calculation.
#' @param sampleSizeSummary, Display endpoint-specific allocation, subject, and analysis-time details when available.
#'    Defaults to \code{TRUE}.
#' @param stoppingBoundariesTable, Display the information schedule, Z-boundaries, and nominal p-values by look.
#'    Defaults to \code{TRUE}.
#' @param survivalAccrualDuration, Enrollment duration in the same time unit as the survival hazards and study duration.
#' @param survivalAccrualRate, Enrollment-rate pattern. With fixed study duration and minimum follow-up, the calculation rescales this value to the required accrual rate; the default scalar 1 gives constant accrual.
#' @param survivalControlHazard, Control-group event hazard per time unit. Use the same time unit as accrual, study duration, follow-up, and dropout hazards. Defaults to 0.0833 for subjects + events and 0.1155 for group sequential accrual.
#' @param survivalDropoutHazard, Equal dropout hazard for both groups, per time unit. Use 0 for no dropout.
#' @param survivalEntry, Patient entry distribution during the enrollment period.
#' @param survivalEntryGamma, Non-zero γ for exponential entry; positive values are convex and negative values are concave. The default is a non-zero starting value.
#' @param survivalGsSurvMethod, Method used for the fixed-design survival calculation inside the group sequential accrual design.
#' @param survivalInformationMethod, Selects whether survival information is planned as events only, as fixed-design subjects plus events, or directly as a group sequential survival design with study-time accrual assumptions.
#' @param survivalMinimumFollowup, Minimum follow-up duration in the same time unit as the study duration.
#' @param survivalStudyDuration, Maximum study duration. Use the same time unit as the control hazard and accrual duration. Defaults to 24 for subjects + events and 18 for group sequential accrual.
#' @param text, Display a short explanation of the design.
#'    Defaults to \code{TRUE}.
#' @param timing, Increasing information fractions. Supply K values ending in 1, or K - 1 interim values strictly below 1. The default schedule is generated from K.
#' @param timingMode, Controls the relative timing of interim analyses on the planned information scale; this is information time, not calendar time.
#' @param upperBoundary, Efficacy boundary family. For one-sided and symmetric designs, O'Brien-Fleming and Pocock are boundary types; for asymmetric designs they are Lan-DeMets spending functions.
#' @param upperBoundaryParameter, Parameter for the selected upper boundary.
GroupSequentialDesign <- function(
          data = NULL,
          version = "1",
          alpha = 0.025,
          binaryAlternativeEffect = 0.1,
          binaryBaselineEventRate = 0.5,
          binaryEffectScale = "riskDifference",
          binaryEventRateGroup1 = 0.5,
          binaryEventRateGroup2 = 0.4,
          binaryInputMode = "baselineEffect",
          binaryNullOddsRatio = 1,
          binaryNullRiskDifference = 0,
          binaryNullRiskRatio = 1,
          boundariesPlot = TRUE,
          colorPalette = "colorblind",
          crossingProbabilitiesPlot = FALSE,
          crossingProbabilitiesTable = FALSE,
          designSummary = TRUE,
          designType = "oneSided",
          effectScale = "canonicalDelta",
          effectSize = 0.5,
          effectStandardDeviation = 1,
          fixedSampleSize = 100,
          generateRCode = FALSE,
          gridPoints = 18,
          hazardRatio = 1.3,
          legendPosition = "right",
          lowerBoundary = "hwangShihDeCani",
          lowerBoundaryParameter = -2,
          naturalEffectSize = 0.5,
          nullHazardRatio = 1,
          numberOfLooks = 3,
          plotHeight = 320,
          plotWidth = 480,
          power = 0.9,
          sampleSizeAllocationRatio = 1,
          sampleSizeMode = "generic",
          sampleSizeSummary = TRUE,
          stoppingBoundariesTable = TRUE,
          survivalAccrualDuration = 12,
          survivalAccrualRate = 1,
          survivalControlHazard = 0.1155,
          survivalDropoutHazard = 0,
          survivalEntry = "unif",
          survivalEntryGamma = 0.1,
          survivalGsSurvMethod = "LachinFoulkes",
          survivalInformationMethod = "events",
          survivalMinimumFollowup = 6,
          survivalStudyDuration = 24,
          text = TRUE,
          timing = "0.33, 0.67, 1",
          timingMode = "even",
          upperBoundary = "obrienFleming",
          upperBoundaryParameter = 0.25) {

   defaultArgCalls <- formals(jaspPower::GroupSequentialDesign)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("binaryEffectScale", "binaryInputMode", "colorPalette", "designType", "effectScale", "legendPosition", "lowerBoundary", "sampleSizeMode", "survivalEntry", "survivalGsSurvMethod", "survivalInformationMethod", "timingMode", "upperBoundary")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspPower", "GroupSequentialDesign", "GroupSequentialDesign.qml", options, version, TRUE))
}