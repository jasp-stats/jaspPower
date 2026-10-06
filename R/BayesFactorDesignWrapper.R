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

#' Bayes Factor Design
#'
#' Bayes Factor design allows you to design fixed sample size experiments for conclusive evidence.
#'
#' @param analysisPriorDirection, Direction of the analysis prior under H₁.
#' @param analysisPriorDistribution, Prior distribution under H₁ used to compute the Bayes factor from the data.
#' @param analysisPriorDistributionFigure, Plot the analysis prior used to compute the Bayes factor from the data.
#'    Defaults to \code{TRUE}.
#' @param analysisPriorFailures, Failure parameter of the beta analysis prior under H₁.
#' @param analysisPriorLocation, Point location of the H₁ analysis prior.
#' @param analysisPriorMean, Mean of the normal H₁ analysis prior.
#' @param analysisPriorMode, Mode of the non-local moment prior under H₁.
#' @param analysisPriorScale, Scale of the normal H₁ analysis prior.
#' @param analysisPriorSpread, Spread parameter of the non-local moment prior under H₁.
#' @param analysisPriorSuccesses, Success parameter of the beta analysis prior under H₁.
#' @param binomialDesignNullPriorDistribution, Design prior under H₀ used to evaluate binomial study outcomes when the null hypothesis is true.
#' @param binomialDesignPriorDistribution, Design prior under H₁ used to evaluate binomial study outcomes when the alternative hypothesis is true.
#' @param calculationTarget, Choose whether to find the sample size needed for a target probability of conclusive evidence, or to evaluate that probability at a selected sample size.
#' @param colorPalette, Choose the color palette used for evidence outcomes and design priors in plots.
#' @param combineDesignAnalysisPriorFigures, Display design and analysis prior distributions in the same figure for comparison.
#'    Defaults to \code{FALSE}.
#' @param combineH1H0Figures, Display curves for the H₁ and H₀ design priors in one figure instead of separate figures.
#'    Defaults to \code{FALSE}.
#' @param conclusiveEvidenceThresholdH0, Bayes factor threshold that defines conclusive evidence for H₀.
#' @param conclusiveEvidenceThresholdH1, Bayes factor threshold that defines conclusive evidence for H₁.
#' @param curvePoints, Number of points used to draw probability and prior curves.
#' @param decisionProbabilities, Show probabilities of evidence for H₁, evidence for H₀, and inconclusive evidence under each design prior.
#'    Defaults to \code{TRUE}.
#' @param decisionProbabilitiesByEffectSize, Plot how conclusive, misleading, and inconclusive evidence probabilities change across design-prior effect sizes.
#'    Defaults to \code{FALSE}.
#' @param decisionProbabilitiesBySampleSize, Plot how conclusive, misleading, and inconclusive evidence probabilities change as the fixed sample size increases.
#'    Defaults to \code{FALSE}.
#' @param designNullPriorDistribution, Design prior under H₀ used to evaluate study outcomes when the null hypothesis is true.
#' @param designNullPriorFailures, Failure parameter of the beta H₀ design prior.
#' @param designNullPriorLowerTruncation, Lower truncation point for the beta H₀ design prior.
#' @param designNullPriorMean, Location or mean of the H₀ design prior.
#' @param designNullPriorStandardDeviation, Standard deviation of the normal H₀ design prior.
#' @param designNullPriorSuccesses, Success parameter of the beta H₀ design prior.
#' @param designNullPriorUpperTruncation, Upper truncation point for the beta H₀ design prior.
#' @param designNullProportion, Point proportion assumed under the H₀ design prior.
#' @param designPriorDistribution, Design prior under H₁ used to evaluate study outcomes when the alternative hypothesis is true.
#' @param designPriorDistributionFigure, Plot the design prior used to evaluate hypothetical study outcomes.
#'    Defaults to \code{TRUE}.
#' @param designPriorFailures, Failure parameter of the beta H₁ design prior.
#' @param designPriorLowerTruncation, Lower truncation point for the beta H₁ design prior.
#' @param designPriorMean, Location or mean of the H₁ design prior.
#' @param designPriorStandardDeviation, Standard deviation of the normal H₁ design prior.
#' @param designPriorSuccesses, Success parameter of the beta H₁ design prior.
#' @param designPriorUpperTruncation, Upper truncation point for the beta H₁ design prior.
#' @param designProportion, Point proportion assumed under the H₁ design prior.
#' @param designSampleSizeBasis, Controls whether output is evaluated at the sample size required for each design hypothesis, for both design hypotheses, or for one selected design hypothesis.
#' @param designSpecification, Show the analysis and design priors used for computing and evaluating the Bayes factor design.
#'    Defaults to \code{TRUE}.
#' @param designSummary, Show the main fixed-design result: the required sample size or the achieved probability of conclusive evidence.
#'    Defaults to \code{TRUE}.
#' @param explanatoryText, Append brief interpretation notes to each displayed Bayesian result.
#'    Defaults to \code{TRUE}.
#' @param generalZParameterization, Scale used for the general z-test approximation, such as standardized mean difference, Fisher's z, or a log effect measure.
#' @param generateRCode, Generate R code that reproduces the Bayes factor design calculation.
#'    Defaults to \code{FALSE}.
#' @param generateReport, Generate a short report describing the design, priors, thresholds, and achieved probabilities.
#'    Defaults to \code{FALSE}.
#' @param generateReportLatexFormattedOutput, Format the generated report with LaTeX notation for use in manuscripts and preregistrations.
#'    Defaults to \code{FALSE}.
#' @param knownStandardDeviation, Known population standard deviation used for z-test sample size planning.
#' @param legendPosition, Choose where the plot legend is shown.
#' @param logSampleSizeAxis, Use a logarithmic x-axis to inspect changes across small and large sample sizes.
#'    Defaults to \code{TRUE}.
#' @param maximumSampleSize, Largest sample size considered when searching for the target probability of conclusive evidence.
#' @param minimumSampleSize, Smallest sample size considered when searching for the target probability of conclusive evidence.
#' @param nullPriorDistribution, Prior specification for H₀ used to compute the Bayes factor from the observed data.
#' @param nullProportion, Proportion specified by the null hypothesis for the binomial test.
#' @param nullValue, Parameter value specified by the null hypothesis.
#' @param observedCohensD, Observed standardized mean difference used to compute the observed Bayes factor.
#' @param observedDataAnalysisInput, Choose whether to enter observed data as summary statistics or select variables from the dataset.
#' \itemize{
#'   \item \code{"columns"}
#'   \item \code{"summaryStatistics"}
#' }
#' @param observedDependentVariable, Observed outcome variable for the independent-samples Bayes factor.
#' @param observedEffectSize, Observed effect estimate on the scale selected for the general z-test approximation.
#' @param observedFailures, Number of observed failures used to compute the observed Bayes factor.
#' @param observedGroupingVariable, Grouping variable that defines the two independent samples.
#' @param observedInputType, Choose which summary statistics are available for the observed t-test result.
#' \itemize{
#'   \item \code{"tAndN"}
#'   \item \code{"meansAndSDs"}
#'   \item \code{"meanDiffAndSD"}
#'   \item \code{"cohensD"}
#'   \item \code{"meanAndSD"}
#' }
#' @param observedMean, Observed mean used to compute the observed Bayes factor.
#' @param observedMean1, Observed mean in group 1.
#' @param observedMean2, Observed mean in group 2.
#' @param observedMeanDifference, Observed paired mean difference used to compute the observed Bayes factor.
#' @param observedProportionVariable, Observed variable containing success and failure values for the binomial test.
#' @param observedSampleSize, Observed sample size used to compute the observed Bayes factor.
#' @param observedSampleSizeGroup1, Observed sample size in group 1.
#' @param observedSampleSizeGroup2, Observed sample size in group 2.
#' @param observedSd, Observed standard deviation used to compute the observed Bayes factor.
#' @param observedSd1, Observed standard deviation in group 1.
#' @param observedSd2, Observed standard deviation in group 2.
#' @param observedSdDifference, Observed standard deviation of paired differences.
#' @param observedStandardError, Standard error of the observed effect estimate.
#' @param observedSuccessValue, Value in the selected variable that is counted as a success.
#' @param observedSuccesses, Number of observed successes used to compute the observed Bayes factor.
#' @param observedT, Observed t statistic used to compute the observed Bayes factor.
#' @param observedVariable, Observed scale variable for the one-sample or general z-test analysis.
#' @param observedVariablePairs, Observed paired variables used to compute the paired-samples Bayes factor.
#' @param probabilityOfConclusiveEvidenceUnderH0, Target probability of obtaining conclusive evidence for H₀ when data are generated under the H₀ design prior.
#' @param probabilityOfConclusiveEvidenceUnderH1, Target probability of obtaining conclusive evidence for H₁ when data are generated under the H₁ design prior.
#' @param sampleSize, Fixed sample size at which the probability of conclusive, misleading, and inconclusive evidence is evaluated.
#' @param sampleSizeAllocationRatio, Ratio of the sample size in group 2 to the sample size in group 1.
#' @param statisticalTest, Select the statistical test for which the Bayes factor design is planned.
#' @param tPriorDegreesOfFreedom, Degrees of freedom of the Student-t analysis prior under H₁.
#' @param tPriorLocation, Location of the Cauchy or Student-t analysis prior under H₁.
#' @param tPriorScale, Scale of the Cauchy or Student-t analysis prior under H₁.
#' @param tSearchRangeLower, Lower bound of the custom integration range for t-test calculations.
#' @param tSearchRangeMode, Choose whether the integration range for t-test calculations is selected automatically or supplied manually.
#' @param tSearchRangeUpper, Upper bound of the custom integration range for t-test calculations.
#' @param unitInformationSd, Standard deviation of the estimator at one unit of information for the general z-test approximation.
BayesFactorDesign <- function(
          data = NULL,
          version = "0.97.1",
          analysisPriorDirection = "greater",
          analysisPriorDistribution = "cauchy",
          analysisPriorDistributionFigure = TRUE,
          analysisPriorFailures = 1,
          analysisPriorLocation = 0.5,
          analysisPriorMean = 0,
          analysisPriorMode = 1,
          analysisPriorScale = 0.707,
          analysisPriorSpread = 0.707,
          analysisPriorSuccesses = 1,
          binomialDesignNullPriorDistribution = "point",
          binomialDesignPriorDistribution = "point",
          calculationTarget = "sampleSize",
          colorPalette = "colorblind",
          combineDesignAnalysisPriorFigures = FALSE,
          combineH1H0Figures = FALSE,
          conclusiveEvidenceThresholdH0 = 10,
          conclusiveEvidenceThresholdH1 = 10,
          curvePoints = 100,
          decisionProbabilities = TRUE,
          decisionProbabilitiesByEffectSize = FALSE,
          decisionProbabilitiesBySampleSize = FALSE,
          designNullPriorDistribution = "point",
          designNullPriorFailures = 1,
          designNullPriorLowerTruncation = 0,
          designNullPriorMean = 0,
          designNullPriorStandardDeviation = 0.1,
          designNullPriorSuccesses = 1,
          designNullPriorUpperTruncation = 0.5,
          designNullProportion = 0.5,
          designPriorDistribution = "point",
          designPriorDistributionFigure = TRUE,
          designPriorFailures = 1,
          designPriorLowerTruncation = 0,
          designPriorMean = 0.5,
          designPriorStandardDeviation = 0.1,
          designPriorSuccesses = 1,
          designPriorUpperTruncation = 1,
          designProportion = 0.6,
          designSampleSizeBasis = "eachDesignHypothesis",
          designSpecification = TRUE,
          designSummary = TRUE,
          explanatoryText = TRUE,
          generalZParameterization = "standardizedMeanDifference",
          generateRCode = FALSE,
          generateReport = FALSE,
          generateReportLatexFormattedOutput = FALSE,
          knownStandardDeviation = 1,
          legendPosition = "right",
          logSampleSizeAxis = TRUE,
          maximumSampleSize = 10000,
          minimumSampleSize = 10,
          nullPriorDistribution = "point",
          nullProportion = 0.5,
          nullValue = 0,
          observedCohensD = 0,
          observedDataAnalysisInput = "summaryStatistics",
          observedDependentVariable = list(types = list(), value = ""),
          observedEffectSize = 0,
          observedFailures = 0,
          observedGroupingVariable = list(types = list(), value = ""),
          observedInputType = "tAndN",
          observedMean = 0,
          observedMean1 = 0,
          observedMean2 = 0,
          observedMeanDifference = 0,
          observedProportionVariable = list(types = list(), value = ""),
          observedSampleSize = 0,
          observedSampleSizeGroup1 = 0,
          observedSampleSizeGroup2 = 0,
          observedSd = 1,
          observedSd1 = 1,
          observedSd2 = 1,
          observedSdDifference = 1,
          observedStandardError = 0,
          observedSuccessValue = 1,
          observedSuccesses = 0,
          observedT = 0,
          observedVariable = list(types = list(), value = ""),
          observedVariablePairs = list(),
          plotHeight = 320,
          plotWidth = 480,
          probabilityOfConclusiveEvidenceUnderH0 = 0.8,
          probabilityOfConclusiveEvidenceUnderH1 = 0.8,
          sampleSize = 100,
          sampleSizeAllocationRatio = 1,
          statisticalTest = "independentSamplesTTest",
          tPriorDegreesOfFreedom = 1,
          tPriorLocation = 0,
          tPriorScale = 0.707,
          tSearchRangeLower = -5,
          tSearchRangeMode = "adaptive",
          tSearchRangeUpper = 5,
          unitInformationSd = 1) {

   defaultArgCalls <- formals(jaspPower::BayesFactorDesign)
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

   optionsWithFormula <- c("analysisPriorDirection", "analysisPriorDistribution", "binomialDesignNullPriorDistribution", "binomialDesignPriorDistribution", "calculationTarget", "colorPalette", "designNullPriorDistribution", "designPriorDistribution", "designSampleSizeBasis", "generalZParameterization", "legendPosition", "nullPriorDistribution", "observedDependentVariable", "observedGroupingVariable", "observedProportionVariable", "observedVariable", "observedVariablePairs", "statisticalTest", "tSearchRangeMode")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspPower", "BayesFactorDesign", "BayesFactorDesign.qml", options, version, TRUE))
}