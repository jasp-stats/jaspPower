BayesFactorDesign <- function(jaspResults, dataset, options) {
  settings    <- .bfdPrepareSettings(options)
  computation <- .bfdCachedComputation(jaspResults, settings)
  settings    <- computation[["settings"]]
  validation  <- computation[["validation"]]
  result      <- computation[["result"]]

  if (options[["designSummary"]])
    .bfdResultsTable(jaspResults, settings, result)

  if (options[["decisionProbabilities"]])
    .bfdDesignOutcomeTable(jaspResults, settings, result)

  if (options[["designSpecification"]])
    .bfdPriorsTable(jaspResults, settings, validation)

  if (.bfdObservedAnalysisReady(dataset, options, settings))
    .bfdObservedAnalysisTable(jaspResults, dataset, options, settings, key = "evidenceObservedAnalysis", position = 16)

  if (options[["explanatoryText"]])
    .bfdText(jaspResults, settings, result)

  if (options[["generateReport"]])
    .bfdReport(jaspResults, settings, result)

  if (options[["generateRCode"]])
    .bfdRCode(jaspResults, settings, result, validation)

  if (options[["decisionProbabilitiesByEffectSize"]])
    .bfdEffectSizePlot(jaspResults, settings, result)

  if (options[["decisionProbabilitiesBySampleSize"]])
    .bfdSampleSizePlot(jaspResults, settings, result)

  if (.bfdPriorPlotRequested(options))
    .bfdPriorPlot(jaspResults, settings, validation)

  return()
}

.bfdDesignDependencies <- c(
  "statisticalTest", "calculationTarget", "conclusiveEvidenceThresholdH1", "conclusiveEvidenceThresholdH0",
  "probabilityOfConclusiveEvidenceUnderH1", "probabilityOfConclusiveEvidenceUnderH0",
  "designSampleSizeBasis", "sampleSize", "sampleSizeAllocationRatio", "knownStandardDeviation",
  "generalZParameterization", "unitInformationSd",
  "analysisPriorDirection", "nullPriorDistribution", "nullValue", "nullProportion",
  "analysisPriorDistribution", "analysisPriorLocation", "analysisPriorMean",
  "analysisPriorScale", "analysisPriorSpread", "analysisPriorMode", "tPriorLocation",
  "tPriorScale", "tPriorDegreesOfFreedom", "analysisPriorSuccesses", "analysisPriorFailures",
  "designNullPriorDistribution", "designNullPriorMean", "designNullPriorStandardDeviation",
  "binomialDesignNullPriorDistribution", "designNullProportion",
  "designNullPriorSuccesses", "designNullPriorFailures", "designNullPriorLowerTruncation",
  "designNullPriorUpperTruncation", "designPriorDistribution", "designPriorMean", "designPriorStandardDeviation", "binomialDesignPriorDistribution",
  "designProportion", "designPriorSuccesses", "designPriorFailures",
  "designPriorLowerTruncation", "designPriorUpperTruncation", "minimumSampleSize",
  "maximumSampleSize", "tSearchRangeMode", "tSearchRangeLower", "tSearchRangeUpper"
)

.bfdReportDependencies <- c(.bfdDesignDependencies, "generateReport", "generateReportLatexFormattedOutput")
.bfdRCodeDependencies  <- c(.bfdDesignDependencies, "generateRCode")
.bfdTextDependencies   <- c(.bfdDesignDependencies, "explanatoryText")
.bfdSummaryDesignDependencies        <- c(.bfdDesignDependencies, "designSummary")
.bfdSummaryEvidenceDependencies      <- c(.bfdDesignDependencies, "decisionProbabilities")
.bfdSummarySpecificationDependencies <- c(.bfdDesignDependencies, "designSpecification")

.bfdSampleSizePlotDependencies <- c(
  .bfdDesignDependencies, "decisionProbabilitiesBySampleSize", "combineH1H0Figures",
  "curvePoints", "logSampleSizeAxis", "legendPosition", "colorPalette"
)
.bfdSampleSizePlotDataDependencies <- c(.bfdDesignDependencies, "curvePoints", "logSampleSizeAxis")
.bfdEffectSizePlotDependencies <- c(
  .bfdDesignDependencies, "decisionProbabilitiesByEffectSize", "combineH1H0Figures",
  "curvePoints", "legendPosition", "colorPalette"
)
.bfdEffectSizePlotDataDependencies <- c(.bfdDesignDependencies, "curvePoints")
.bfdPriorPlotDependencies <- c(
  .bfdDesignDependencies, "designPriorDistributionFigure", "analysisPriorDistributionFigure", "combineDesignAnalysisPriorFigures",
  "curvePoints", "legendPosition", "colorPalette"
)
.bfdPriorPlotDataDependencies <- c(
  "statisticalTest", "analysisPriorDirection", "nullPriorDistribution", "nullValue", "nullProportion",
  "analysisPriorDistribution", "analysisPriorLocation", "analysisPriorMean", "analysisPriorScale",
  "analysisPriorSpread", "analysisPriorMode", "tPriorLocation", "tPriorScale", "tPriorDegreesOfFreedom",
  "designNullPriorDistribution", "designNullPriorMean", "designNullPriorStandardDeviation",
  "binomialDesignNullPriorDistribution", "designNullProportion",
  "designNullPriorSuccesses", "designNullPriorFailures", "designNullPriorLowerTruncation",
  "designNullPriorUpperTruncation", "designPriorDistribution", "designPriorMean", "designPriorStandardDeviation",
  "binomialDesignPriorDistribution", "designProportion", "designPriorSuccesses",
  "designPriorFailures", "designPriorLowerTruncation", "designPriorUpperTruncation", "curvePoints"
)

.bfdObservedDependencies <- c(
  "observedDataAnalysisInput", "observedInputType",
  "observedSampleSize", "observedSampleSizeGroup1", "observedSampleSizeGroup2", "observedMean", "observedMean1",
  "observedMean2", "observedSd", "observedSd1", "observedSd2",
  "observedMeanDifference", "observedSdDifference", "observedEffectSize",
  "observedStandardError", "observedT", "observedCohensD",
  "observedSuccesses", "observedFailures",
  "observedVariable", "observedVariablePairs",
  "observedDependentVariable", "observedGroupingVariable",
  "observedProportionVariable", "observedSuccessValue"
)

.bfdCachedComputation <- function(jaspResults, settings) {
  state <- jaspResults[["evidenceComputation"]]
  if (!is.null(state) && !is.null(state$object))
    return(.bfdApplyCurrentSettings(state$object, settings, .bfdDisplaySettingNames))

  state <- createJaspState()
  state$dependOn(.bfdDesignDependencies)
  jaspResults[["evidenceComputation"]] <- state

  validation <- try(.bfdValidateSettings(settings), silent = TRUE)
  result     <- if (jaspBase::isTryError(validation)) validation else try(.bfdComputeResult(settings), silent = TRUE)

  state$object <- list(
    settings   = settings,
    validation = validation,
    result     = result
  )

  return(.bfdApplyCurrentSettings(state$object, settings, .bfdDisplaySettingNames))
}

.bfdDisplaySettingNames <- c(
  "plotPoints", "logSampleSize", "legendPosition", "colorPalette",
  "mergeH1H0Figures", "priorPlotDesign", "priorPlotAnalysis",
  "priorPlotMerge", "reportLatex"
)

.bfdPrepareSettings <- function(options) {
  test <- options[["statisticalTest"]]

  settings <- list(
    test                 = test,
    testLabel            = .bfdTestLabel(test),
    testType             = .bfdTestType(test),
    isIndependentSamples = grepl("independentSamples", test, fixed = TRUE),
    isTTest              = grepl("TTest", test, fixed = TRUE),
    isGeneralZ           = identical(test, "generalZApproximation"),
    isZTest              = grepl("ZTest", test, fixed = TRUE) || identical(test, "generalZApproximation"),
    isBinomial           = identical(test, "oneSampleProportion"),
    calculation          = options[["calculationTarget"]],
    bf10Threshold        = options[["conclusiveEvidenceThresholdH1"]],
    bf01Threshold        = options[["conclusiveEvidenceThresholdH0"]],
    targetPowerH1        = options[["probabilityOfConclusiveEvidenceUnderH1"]],
    targetPowerH0        = options[["probabilityOfConclusiveEvidenceUnderH0"]],
    designSampleSizeBasis = options[["designSampleSizeBasis"]],
    planningTargets      = .bfdPlanningTargets(),
    sampleSize           = options[["sampleSize"]],
    sampleSizeRatio      = options[["sampleSizeAllocationRatio"]],
    rangeMin             = options[["minimumSampleSize"]],
    rangeMax             = options[["maximumSampleSize"]],
    plotPoints           = options[["curvePoints"]],
    logSampleSize        = options[["logSampleSizeAxis"]],
    legendPosition       = options[["legendPosition"]],
    colorPalette         = options[["colorPalette"]],
    mergeH1H0Figures     = options[["combineH1H0Figures"]],
    priorPlotDesign      = options[["designPriorDistributionFigure"]],
    priorPlotAnalysis    = options[["analysisPriorDistributionFigure"]],
    priorPlotMerge       = options[["combineDesignAnalysisPriorFigures"]],
    reportLatex          = options[["generateReportLatexFormattedOutput"]]
  )

  if (settings[["isBinomial"]]) {
    settings <- .bfdAddBinomialSettings(settings, options)
  } else {
    settings <- .bfdAddContinuousSettings(settings, options)
  }

  return(settings)
}

.bfdPlanningTargets <- function() {
  return(c("h1", "h0"))
}

.bfdResultTargets <- function(settings) {
  if (identical(settings[["calculation"]], "sampleSize"))
    return(settings[["planningTargets"]])

  return(c("h1", "h0"))
}

.bfdPlanningTargetText <- function(settings) {
  labels <- vapply(settings[["planningTargets"]], .bfdTargetLabel, character(1))
  return(paste(labels, collapse = ", "))
}

.bfdAddContinuousSettings <- function(settings, options) {
  alternative <- options[["analysisPriorDirection"]]

  settings[["nullValue"]]         <- options[["nullValue"]]
  settings[["standardDeviation"]] <- options[["knownStandardDeviation"]]
  settings[["generalZParameterization"]] <- options[["generalZParameterization"]]
  settings[["unitInformationSd"]]        <- options[["unitInformationSd"]]
  settings[["alternative"]]       <- switch(alternative, twoSided = "two.sided", alternative)
  settings[["n1"]]                <- options[["sampleSize"]]
  settings[["n2"]]                <- if (settings[["isIndependentSamples"]]) {
    ceiling(options[["sampleSize"]] * options[["sampleSizeAllocationRatio"]])
  } else {
    options[["sampleSize"]]
  }

  if (settings[["isZTest"]]) {
    settings <- .bfdAddContinuousZAnalysisPrior(settings, options)
  } else {
    settings                  <- .bfdAddContinuousTAnalysisPrior(settings, options)
    settings[["drangeMode"]]  <- options[["tSearchRangeMode"]]
    settings[["drangeLower"]] <- options[["tSearchRangeLower"]]
    settings[["drangeUpper"]] <- options[["tSearchRangeUpper"]]
  }

  settings <- .bfdAddContinuousDesignPriors(settings, options)

  return(settings)
}

.bfdAddBinomialSettings <- function(settings, options) {
  settings[["nullPriorDistribution"]] <- options[["nullPriorDistribution"]]
  settings[["nullProportion"]]        <- options[["nullProportion"]]
  settings[["analysisPriorSuccesses"]] <- options[["analysisPriorSuccesses"]]
  settings[["analysisPriorFailures"]]  <- options[["analysisPriorFailures"]]
  settings[["sampleSize"]]             <- options[["sampleSize"]]
  settings[["n1"]]                     <- options[["sampleSize"]]
  settings[["n2"]]                     <- NA_integer_

  settings[["binomialDesignPrior"]]  <- options[["binomialDesignPriorDistribution"]]
  settings[["designProportion"]]     <- options[["designProportion"]]
  settings[["designPriorSuccesses"]] <- options[["designPriorSuccesses"]]
  settings[["designPriorFailures"]]  <- options[["designPriorFailures"]]
  settings[["designPriorLower"]]     <- options[["designPriorLowerTruncation"]]
  settings[["designPriorUpper"]]     <- options[["designPriorUpperTruncation"]]

  settings <- .bfdAddBinomialDesignPriors(settings, options)

  return(settings)
}

.bfdComputeResult <- function(settings) {
  .bfdValidateSettings(settings)

  targets       <- .bfdResultTargets(settings)
  targetResults <- lapply(targets, .bfdComputeTargetResult, settings = settings)
  targetErrors  <- vapply(targetResults, .bfdTargetResultError, character(1))
  names(targetErrors) <- targets
  targetErrors <- targetErrors[nzchar(targetErrors)]

  targetResults <- do.call(rbind, targetResults)
  n1Values <- targetResults[["n1"]]
  n2Values <- targetResults[["n2"]]
  probabilityValues <- targetResults[["probability"]]

  return(list(
    n1            = if (all(is.na(n1Values))) NA_integer_ else max(n1Values, na.rm = TRUE),
    n2            = if (all(is.na(n2Values))) NA_integer_ else max(n2Values, na.rm = TRUE),
    probability   = if (all(is.na(probabilityValues))) NA_real_ else min(probabilityValues, na.rm = TRUE),
    targetResults = targetResults,
    targetErrors  = targetErrors
  ))
}

.bfdComputeTargetResult <- function(target, settings) {
  under <- target

  result <- try({
    n1 <- if (settings[["calculation"]] == "sampleSize") {
      .bfdFindSampleSize(settings, target = target, under = under)
    } else {
      settings[["sampleSize"]]
    }

    .bfdValidateSampleSize(settings, n1)

    data.frame(
      under       = under,
      target      = target,
      n1          = n1,
      n2          = .bfdSampleSizeSecondGroup(settings, n1),
      probability = .bfdEvidenceProbability(settings, n1 = n1, target = target, under = under),
      stringsAsFactors = FALSE
    )
  }, silent = TRUE)

  if (!jaspBase::isTryError(result))
    return(result)

  row <- data.frame(
    under       = under,
    target      = target,
    n1          = NA_integer_,
    n2          = NA_integer_,
    probability = NA_real_,
    stringsAsFactors = FALSE
  )
  attr(row, "error") <- trimws(.bfdCleanError(result))

  return(row)
}

.bfdTargetResultError <- function(result) {
  error <- attr(result, "error", exact = TRUE)
  if (is.null(error))
    return("")

  return(trimws(error))
}

.bfdEvidenceProbability <- function(settings, n1 = settings[["n1"]], target,
                                   under = "h1", designPriorMean = NULL, designPriorSd = NULL) {
  k         <- .bfdEventK(settings, target)
  lowerTail <- .bfdLowerTail(target)

  if (settings[["isBinomial"]]) {
    designArguments <- if (is.null(designPriorMean)) .bfdBinomialDesignArguments(settings, under = under) else list(dp = designPriorMean)
    return(.bfdEvidenceProbabilityBinomial(settings, n1, k, lowerTail, designArguments))
  }

  if (is.null(designPriorMean)) {
    designPrior     <- .bfdContinuousDesignPriorForUnder(settings, under)
    designPriorMean <- designPrior[["mean"]]
    designPriorSd   <- designPrior[["sd"]]
  } else if (is.null(designPriorSd)) {
    designPriorSd <- .bfdContinuousDesignPriorForUnder(settings, under)[["sd"]]
  }

  if (settings[["isZTest"]])
    return(.bfdEvidenceProbabilityZ(settings, n1, k, lowerTail, designPriorMean, designPriorSd))

  return(.bfdEvidenceProbabilityT(settings, n1, k, lowerTail, designPriorMean, designPriorSd))
}

.bfdContinuousDesignPriorForUnder <- function(settings, under) {
  if (under == "h0")
    return(settings[["designPriorUnderH0"]])

  return(settings[["designPriorUnderH1"]])
}

.bfdEvidenceProbabilityZ <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  unitSd <- .bfdZUnitStandardDeviation(settings, n1)

  if (settings[["analysisPriorDistribution"]] %in% c("point", "normal")) {
    return(bfpwr::pbf01(
      k          = k,
      n          = n1,
      usd        = unitSd,
      null       = settings[["nullValue"]],
      pm         = settings[["analysisPriorMean"]],
      psd        = settings[["analysisPriorSd"]],
      dpm        = designPriorMean,
      dpsd       = designPriorSd,
      lower.tail = lowerTail
    ))
  }

  return(bfpwr::pnmbf01(
    k          = k,
    n          = n1,
    usd        = unitSd,
    null       = settings[["nullValue"]],
    psd        = settings[["momentPriorSpread"]],
    dpm        = designPriorMean,
    dpsd       = designPriorSd,
    lower.tail = lowerTail
  ))
}

.bfdEvidenceProbabilityT <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  n2 <- .bfdSampleSizeSecondGroup(settings, n1)

  return(bfpwr::ptbf01(
    k           = k,
    n           = n1,
    n1          = n1,
    n2          = n2,
    null        = settings[["nullValue"]],
    plocation   = settings[["tPriorLocationRelative"]],
    pscale      = settings[["tPriorScale"]],
    pdf         = settings[["tPriorDf"]],
    dpm         = designPriorMean,
    dpsd        = designPriorSd,
    type        = settings[["testType"]],
    alternative = settings[["alternative"]],
    lower.tail  = lowerTail,
    drange      = .bfdTSearchRange(settings, n1, k)
  ))
}

.bfdEvidenceProbabilityBinomial <- function(settings, n, k, lowerTail, designArguments = NULL) {
  if (is.null(designArguments))
    designArguments <- .bfdBinomialDesignArguments(settings)

  return(do.call(
    what = bfpwr::pbinbf01,
    args = c(
      list(
        k          = k,
        n          = n,
        p0         = settings[["nullProportion"]],
        type       = settings[["nullPriorDistribution"]],
        a          = settings[["analysisPriorSuccesses"]],
        b          = settings[["analysisPriorFailures"]],
        lower.tail = lowerTail
      ),
      designArguments
    )
  ))
}

.bfdFindSampleSize <- function(settings, target, under) {
  minimumN <- .bfdMinimumSampleSize(settings)
  maximumN <- ceiling(settings[["rangeMax"]])

  if (minimumN >= maximumN)
    stop(gettext("The minimum sample size must be smaller than the maximum sample size."))

  packageN <- try(.bfdFindSampleSizeWithBfpwr(settings, minimumN, maximumN, target = target, under = under), silent = TRUE)

  if (!jaspBase::isTryError(packageN) && length(packageN) == 1 && is.finite(packageN)) {
    n <- ceiling(packageN)
  } else {
    n <- .bfdFindSampleSizeBySearch(settings, minimumN, maximumN, target = target, under = under)
  }

  n <- max(minimumN, min(maximumN, n))
  n <- .bfdAdjustSampleSize(settings, n, minimumN, maximumN, target = target, under = under)

  return(n)
}

.bfdFindSampleSizeWithBfpwr <- function(settings, minimumN, maximumN, target, under) {
  nrange <- c(minimumN, maximumN)
  k      <- .bfdEventK(settings, target)
  lowerTail <- .bfdLowerTail(target)

  if (settings[["isBinomial"]]) {
    return(do.call(
      what = bfpwr::nbinbf01,
      args = c(
        list(
          k          = k,
          power      = .bfdTargetPower(settings, target),
          p0         = settings[["nullProportion"]],
          type       = settings[["nullPriorDistribution"]],
          a          = settings[["analysisPriorSuccesses"]],
          b          = settings[["analysisPriorFailures"]],
          lower.tail = lowerTail,
          nrange     = nrange
        ),
        .bfdBinomialDesignArguments(settings, under = under)
      )
    ))
  }

  designPrior <- .bfdContinuousDesignPriorForUnder(settings, under)

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("point", "normal")) {
    return(bfpwr::nbf01(
      k          = k,
      power      = .bfdTargetPower(settings, target),
      usd        = .bfdZUnitStandardDeviation(settings, minimumN),
      null       = settings[["nullValue"]],
      pm         = settings[["analysisPriorMean"]],
      psd        = settings[["analysisPriorSd"]],
      dpm        = designPrior[["mean"]],
      dpsd       = designPrior[["sd"]],
      nrange     = nrange,
      lower.tail = lowerTail
    ))
  }

  if (settings[["isZTest"]]) {
    return(bfpwr::nnmbf01(
      k          = k,
      power      = .bfdTargetPower(settings, target),
      usd        = .bfdZUnitStandardDeviation(settings, minimumN),
      null       = settings[["nullValue"]],
      psd        = settings[["momentPriorSpread"]],
      dpm        = designPrior[["mean"]],
      dpsd       = designPrior[["sd"]],
      nrange     = nrange,
      lower.tail = lowerTail
    ))
  }

  if (settings[["isIndependentSamples"]] && settings[["sampleSizeRatio"]] != 1)
    stop(gettext("Sample-size search for unequal group sizes is handled internally."))

  if (settings[["drangeMode"]] == "custom")
    stop(gettext("Sample-size search for custom t search ranges is handled internally."))

  if (!isTRUE(all.equal(settings[["nullValue"]], 0)))
    stop(gettext("Sample-size search for nonzero null values is handled internally."))

  return(bfpwr::ntbf01(
    k           = k,
    power       = .bfdTargetPower(settings, target),
    null        = settings[["nullValue"]],
    plocation   = settings[["tPriorLocationRelative"]],
    pscale      = settings[["tPriorScale"]],
    pdf         = settings[["tPriorDf"]],
    type        = settings[["testType"]],
    alternative = settings[["alternative"]],
    dpm         = designPrior[["mean"]],
    dpsd        = designPrior[["sd"]],
    lower.tail  = lowerTail,
    nrange      = nrange
  ))
}

.bfdFindSampleSizeBySearch <- function(settings, minimumN, maximumN, target, under) {
  targetPower <- .bfdTargetPower(settings, target)
  lowerProbability <- .bfdEvidenceProbability(settings, n1 = minimumN, target = target, under = under)
  if (is.finite(lowerProbability) && lowerProbability >= targetPower)
    return(minimumN)

  upperProbability <- .bfdEvidenceProbability(settings, n1 = maximumN, target = target, under = under)
  if (!is.finite(upperProbability) || upperProbability < targetPower)
    stop(gettext("Target conclusive evidence probability is not reached within the selected sample-size range."))

  lower <- minimumN
  upper <- maximumN

  while ((upper - lower) > 1) {
    midpoint            <- floor((lower + upper) / 2)
    midpointProbability <- .bfdEvidenceProbability(settings, n1 = midpoint, target = target, under = under)

    if (is.finite(midpointProbability) && midpointProbability >= targetPower) {
      upper <- midpoint
    } else {
      lower <- midpoint
    }
  }

  return(upper)
}

.bfdAdjustSampleSize <- function(settings, n, minimumN, maximumN, target, under) {
  targetPower <- .bfdTargetPower(settings, target)
  while (n <= maximumN && .bfdEvidenceProbability(settings, n1 = n, target = target, under = under) < targetPower)
    n <- n + 1

  if (n > maximumN)
    stop(gettext("Target conclusive evidence probability is not reached within the selected sample-size range."))

  while (n > minimumN && .bfdEvidenceProbability(settings, n1 = n - 1, target = target, under = under) >= targetPower)
    n <- n - 1

  return(n)
}

.bfdResultsTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "evidenceResults",
    title        = gettext("Bayes Factor Design"),
    position     = 1,
    dependencies = .bfdSummaryDesignDependencies
  )
  if (is.null(table))
    return()

  .bfdAddResultsTableColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute Bayes factor design: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.bfdResultsRows(settings, result))
  .bfdAddDesignPriorErrorFootnotes(table, result[["targetErrors"]])

  generalZUisdFootnote <- .bfdGeneralZKnownUisdFootnote(settings)
  if (!is.null(generalZUisdFootnote))
    table$addFootnote(generalZUisdFootnote)

  if (settings[["calculation"]] == "sampleSize")
    table$addFootnote(gettext("Due to rounding of the sample size, Pr(Conclusive Evidence) can deviate from the target probability."))

  combinedSampleSizeFootnote <- .bfdCombinedSampleSizeFootnote(settings, result)
  if (!is.null(combinedSampleSizeFootnote))
    table$addFootnote(combinedSampleSizeFootnote)

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested N\u2082/N\u2081 sample-size ratio."))
}

.bfdAddResultsTableColumns <- function(table, settings) {
  computed    <- gettext("Computed")
  userDefined <- gettext("User Defined")

  table$addColumnInfo(name = "under", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "decisionRule", title = gettext("Decision Rule"), type = "string", overtitle = userDefined)

  if (settings[["calculation"]] == "sampleSize") {
    table$addColumnInfo(name = "targetProbability", title = gettext("Target Probability"), type = "number", overtitle = userDefined)

    if (settings[["isIndependentSamples"]]) {
      table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer", overtitle = computed)
      table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer", overtitle = computed)
    } else {
      table$addColumnInfo(name = "n", title = "N", type = "integer", overtitle = computed)
    }
    table$addColumnInfo(name = "probability", title = gettext("Achieved Probability"), type = "number", overtitle = computed)
  } else {
    if (settings[["isIndependentSamples"]]) {
      table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer", overtitle = userDefined)
      table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer", overtitle = userDefined)
    } else {
      table$addColumnInfo(name = "n", title = "N", type = "integer", overtitle = userDefined)
    }
    table$addColumnInfo(name = "probability", title = gettext("Achieved Probability"), type = "number", overtitle = computed)
  }
}

.bfdResultsRows <- function(settings, result) {
  rows <- result[["targetResults"]]
  n1   <- if (identical(settings[["calculation"]], "sampleSize")) {
    vapply(rows[["under"]], function(under) .bfdResultN1ForBasis(settings, result, under), numeric(1))
  } else {
    rows[["n1"]]
  }

  out <- data.frame(
    under             = vapply(rows[["under"]], .bfdUnderLabel, character(1)),
    decisionRule      = vapply(rows[["target"]], function(target) .bfdDecisionRuleLabel(settings, target), character(1)),
    probability       = mapply(
      function(target, under, n) .bfdResultProbabilityAtN(settings, target, under, n),
      rows[["target"]],
      rows[["under"]],
      n1
    ),
    targetProbability = vapply(rows[["target"]], function(target) .bfdTargetPower(settings, target), numeric(1)),
    stringsAsFactors  = FALSE
  )

  if (settings[["isIndependentSamples"]]) {
    out[["n1"]] <- n1
    out[["n2"]] <- .bfdSampleSizeSecondGroup(settings, n1)
  } else {
    out[["n"]] <- n1
  }

  if (settings[["calculation"]] != "sampleSize")
    out[["targetProbability"]] <- NULL

  columnOrder <- c(
    "under",
    "decisionRule",
    if (settings[["calculation"]] == "sampleSize") "targetProbability",
    if (settings[["isIndependentSamples"]]) c("n1", "n2") else "n",
    "probability"
  )
  out <- out[, columnOrder, drop = FALSE]
  row.names(out) <- NULL

  return(out)
}

.bfdResultProbabilityAtN <- function(settings, target, under, n1) {
  if (length(n1) != 1 || !is.finite(n1))
    return(NA_real_)

  probability <- try(.bfdEvidenceProbability(settings, n1 = n1, target = target, under = under), silent = TRUE)
  if (jaspBase::isTryError(probability))
    return(NA_real_)

  return(probability)
}

.bfdDesignOutcomeTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "evidenceDesignOutcome",
    title        = gettext("Bayes Factor Decision Probabilities"),
    position     = 2,
    dependencies = .bfdSummaryEvidenceDependencies
  )
  if (is.null(table))
    return()

  .bfdAddDesignOutcomeColumns(
    table,
    underTitle       = gettext("Design Prior"),
    overtitle        = gettext("Bayes Factor Decision"),
    nullTitle        = gettext("Evidence for H\u2080"),
    alternativeTitle = gettext("Evidence for H\u2081")
  )

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute Bayes factor decision probabilities: %1$s", .bfdCleanError(result)))
    return()
  }

  rows <- try(.bfdDesignOutcomeRows(settings, result), silent = TRUE)
  if (jaspBase::isTryError(rows)) {
    table$setError(gettextf("Unable to compute Bayes factor decision probabilities: %1$s", .bfdCleanError(rows)))
    return()
  }

  table$setData(rows)
  .bfdAddDesignPriorErrorFootnotes(table, attr(rows, "errors", exact = TRUE))
  table$addFootnote(.bfdDesignOutcomeSampleSizeFootnote(settings, result))
}

.bfdDesignOutcomeRows <- function(settings, result) {
  h1Outcome <- .bfdDesignOutcomeForUnder(settings, result, "h1")
  h0Outcome <- .bfdDesignOutcomeForUnder(settings, result, "h0")

  rows <- .bfdDesignOutcomeRowsFromOutcomes(
    .bfdDesignOutcomeValues(h1Outcome),
    .bfdDesignOutcomeValues(h0Outcome),
    underLabels = c(.bfdUnderLabel("h1"), .bfdUnderLabel("h0"))
  )
  errors <- c(h1 = .bfdDesignOutcomeError(h1Outcome), h0 = .bfdDesignOutcomeError(h0Outcome))
  attr(rows, "errors") <- errors[nzchar(errors)]

  return(rows)
}

.bfdCombinedSampleSizeFootnote <- function(settings, result) {
  if (!identical(settings[["calculation"]], "sampleSize"))
    return(NULL)

  n1 <- result[["n1"]]
  if (length(n1) != 1 || !is.finite(n1))
    return(NULL)

  if (settings[["isIndependentSamples"]]) {
    n2 <- result[["n2"]]
    if (length(n2) != 1 || !is.finite(n2))
      return(NULL)

    return(gettextf(
      "To satisfy both design priors in a single fixed design, use N\u2081 = %1$s and N\u2082 = %2$s (total N = %3$s).",
      n1,
      n2,
      n1 + n2
    ))
  }

  return(gettextf("To satisfy both design priors in a single fixed design, use N = %1$s.", n1))
}

.bfdDesignOutcomeSampleSizeFootnote <- function(settings, result) {
  if (!identical(settings[["calculation"]], "sampleSize"))
    return(.bfdDesignOutcomeSampleSizeFootnoteForN(settings, result[["n1"]]))

  if (identical(settings[["designSampleSizeBasis"]], "eachDesignHypothesis")) {
    h1N <- .bfdResultN1ForUnder(result, "h1")
    h0N <- .bfdResultN1ForUnder(result, "h0")
    if (!is.finite(h1N) || !is.finite(h0N))
      return(gettext("Probabilities are evaluated at the fixed-design sample size required for the corresponding design prior under H\u2081 or H\u2080."))

    if (settings[["isIndependentSamples"]]) {
      h1N2 <- .bfdSampleSizeSecondGroup(settings, h1N)
      h0N2 <- .bfdSampleSizeSecondGroup(settings, h0N)
      return(gettextf(
        "Probabilities are evaluated at the fixed-design sample size required for each design prior: under H\u2081, N\u2081 = %1$s and N\u2082 = %2$s; under H\u2080, N\u2081 = %3$s and N\u2082 = %4$s.",
        h1N,
        h1N2,
        h0N,
        h0N2
      ))
    }

    return(gettextf(
      "Probabilities are evaluated at the fixed-design sample size required for each design prior: under H\u2081, N = %1$s; under H\u2080, N = %2$s.",
      h1N,
      h0N
    ))
  }

  n1 <- .bfdResultN1ForBasis(settings, result, "h1")
  return(.bfdDesignOutcomeSampleSizeFootnoteForN(settings, n1))
}

.bfdDesignOutcomeSampleSizeFootnoteForN <- function(settings, n1) {
  if (length(n1) != 1 || !is.finite(n1))
    return(gettext("Probabilities are evaluated at the selected fixed-design sample size. Rows use the corresponding design prior under H\u2081 or H\u2080."))

  if (settings[["isIndependentSamples"]]) {
    n2 <- .bfdSampleSizeSecondGroup(settings, n1)
    if (length(n2) != 1 || !is.finite(n2))
      return(gettext("Probabilities are evaluated at the selected fixed-design sample sizes. Rows use the corresponding design prior under H\u2081 or H\u2080."))

    return(gettextf(
      "Probabilities are evaluated at the selected fixed-design sample sizes: N\u2081 = %1$s and N\u2082 = %2$s (total N = %3$s). Rows use the corresponding design prior under H\u2081 or H\u2080.",
      n1,
      n2,
      n1 + n2
    ))
  }

  return(gettextf(
    "Probabilities are evaluated at the selected fixed-design sample size: N = %1$s. Rows use the corresponding design prior under H\u2081 or H\u2080.",
    n1
  ))
}

.bfdDesignOutcomeForUnder <- function(settings, result, under) {
  basisTarget <- .bfdDesignSampleSizeBasisTarget(settings, under)
  if (!is.null(basisTarget)) {
    targetError <- .bfdTargetError(result, basisTarget)
  } else {
    targetError <- .bfdTargetError(result, under)
  }
  if (!is.null(targetError))
    return(.bfdDesignOutcomeErrorResult(targetError))

  n1 <- .bfdResultN1ForBasis(settings, result, under)
  if (length(n1) == 0 || !is.finite(n1))
    return(.bfdDesignOutcomeErrorResult(gettext("Sample size is not available.")))

  outcome <- try(.bfdDesignOutcomeProbabilities(settings, n1, under = under), silent = TRUE)
  if (jaspBase::isTryError(outcome))
    return(.bfdDesignOutcomeErrorResult(trimws(.bfdCleanError(outcome))))

  return(list(values = outcome, error = NULL))
}

.bfdDesignOutcomeErrorResult <- function(error) {
  list(
    values = c(null = NA_real_, undecided = NA_real_, alternative = NA_real_),
    error  = error
  )
}

.bfdDesignOutcomeValues <- function(outcome) {
  outcome[["values"]]
}

.bfdDesignOutcomeError <- function(outcome) {
  error <- outcome[["error"]]
  if (is.null(error))
    return("")

  return(trimws(error))
}

.bfdTargetError <- function(result, under) {
  errors <- result[["targetErrors"]]
  if (is.null(errors) || !under %in% names(errors))
    return(NULL)

  return(errors[[under]])
}

.bfdHasTargetErrors <- function(result) {
  return(length(result[["targetErrors"]]) > 0)
}

.bfdAddDesignPriorErrorFootnotes <- function(table, errors) {
  if (length(errors) == 0)
    return()

  for (under in names(errors)) {
    table$addFootnote(
      gettextf("%1$s: %2$s", .bfdUnderLabel(under), errors[[under]]),
      symbol = gettext("Error:")
    )
  }
}

.bfdResultN1ForUnder <- function(result, under) {
  rows <- result[["targetResults"]]
  index <- which(rows[["under"]] == under)
  if (length(index) == 0)
    return(result[["n1"]])

  return(rows[["n1"]][index[1]])
}

.bfdDesignSampleSizeBasisTarget <- function(settings, under) {
  if (!identical(settings[["calculation"]], "sampleSize"))
    return(NULL)

  switch(settings[["designSampleSizeBasis"]],
    eachDesignHypothesis = under,
    bothDesignHypotheses = NULL,
    alternativeHypothesis = "h1",
    nullHypothesis        = "h0",
    under
  )
}

.bfdResultN1ForBasis <- function(settings, result, under) {
  if (!identical(settings[["calculation"]], "sampleSize"))
    return(result[["n1"]])

  basisTarget <- .bfdDesignSampleSizeBasisTarget(settings, under)
  if (is.null(basisTarget))
    return(result[["n1"]])

  return(.bfdResultN1ForUnder(result, basisTarget))
}

.bfdDesignOutcomeProbabilities <- function(settings, n1, under) {
  alternative <- .bfdDesignOutcomeEvidenceProbability(settings, n1, target = "h1", under = under)
  null        <- .bfdDesignOutcomeEvidenceProbability(settings, n1, target = "h0", under = under)
  undecided   <- 1 - alternative - null

  return(c(
    null        = .bfdClampProbability(null),
    undecided   = .bfdClampProbability(undecided),
    alternative = .bfdClampProbability(alternative)
  ))
}

.bfdDesignOutcomeEvidenceProbability <- function(settings, n1, target, under) {
  return(.bfdEvidenceProbability(settings, n1 = n1, target = target, under = under))
}

.bfdPriorsTable <- function(jaspResults, settings, validation) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "evidencePriors",
    title        = gettext("Design Specification"),
    position     = 3,
    dependencies = .bfdSummarySpecificationDependencies
  )
  if (is.null(table))
    return()

  .bfdAddPriorsTableColumns(table)

  if (jaspBase::isTryError(validation)) {
    table$setError(gettextf("Unable to describe priors: %1$s", .bfdCleanError(validation)))
    return()
  }

  table$setData(.bfdPriorsRows(settings))
}

.bfdObservedAnalysisTable <- function(jaspResults, dataset, options, settings, key, position, sequential = FALSE,
                                     dependencies = .bfdDesignDependencies) {
  if (!is.null(jaspResults[[key]]))
    return()

  showSummaryTable <- options[["observedDataAnalysisInput"]] == "columns"

  container <- createJaspContainer(title = gettext("Observed Data Analysis"))
  container$dependOn(c(dependencies, .bfdObservedDependencies))
  container$position <- position
  jaspResults[[key]] <- container

  resultTable <- createJaspTable(title = gettext("Analysis Result"))
  resultTable$position <- 1
  resultTable$showSpecifiedColumnsOnly <- TRUE
  container[["result"]] <- resultTable
  .bfdObservedAddResultColumns(resultTable, settings)

  if (showSummaryTable) {
    summaryTable <- createJaspTable(title = gettext("Summary Statistics"))
    summaryTable$position <- 2
    summaryTable$showSpecifiedColumnsOnly <- TRUE
    container[["summaryStatistics"]] <- summaryTable
  }

  summary <- try(.bfdObservedSummary(dataset, options, settings), silent = TRUE)
  if (jaspBase::isTryError(summary)) {
    message <- gettextf("Unable to summarize observed data: %1$s", .bfdCleanError(summary))
    resultTable$setError(message)
    if (showSummaryTable)
      summaryTable$setError(message)
    return()
  }

  if (showSummaryTable)
    .bfdObservedFillSummaryTable(summaryTable, settings, summary)

  bayesFactor <- try(.bfdObservedBayesFactor(settings, summary), silent = TRUE)
  if (jaspBase::isTryError(bayesFactor)) {
    resultTable$setError(gettextf("Unable to compute observed Bayes factor: %1$s", .bfdCleanError(bayesFactor)))
    return()
  }

  resultTable$setData(.bfdObservedResultRow(settings, summary, bayesFactor, sequential))

  if (settings[["isTTest"]])
    resultTable$addFootnote(gettext("For t-tests, the estimate is the standardized mean difference on the analysis scale."))

  if (isTRUE(sequential))
    resultTable$addFootnote(gettext("Sequential decisions use the configured BF thresholds for the current observed data."))
}

.bfdObservedAddResultColumns <- function(table, settings) {
  table$addColumnInfo(name = "test", title = gettext("Statistical Test"), type = "string")
  table$addColumnInfo(
    name  = "statistic",
    title = .bfdObservedStatisticTitle(settings),
    type  = if (settings[["isBinomial"]]) "integer" else "number"
  )
  table$addColumnInfo(name = "bf10", title = "BF\u2081\u2080", type = "number")
  table$addColumnInfo(name = "bf01", title = "BF\u2080\u2081", type = "number")
  table$addColumnInfo(name = "decision", title = gettext("Decision"), type = "string")
}

.bfdObservedResultRow <- function(settings, summary, bayesFactor, sequential) {
  return(data.frame(
    test      = .bfdObservedTestLabel(settings),
    statistic = .bfdObservedStatisticValue(settings, summary),
    bf10      = bayesFactor[["bf10"]],
    bf01      = bayesFactor[["bf01"]],
    decision  = .bfdObservedDecision(settings, bayesFactor, sequential),
    stringsAsFactors = FALSE
  ))
}

.bfdObservedTestLabel <- function(settings) {
  switch(
    settings[["test"]],
    independentSamplesTTest = gettext("Independent Samples T-Test"),
    pairedSamplesTTest      = gettext("Paired Samples T-Test"),
    oneSampleTTest          = gettext("One Sample T-Test"),
    independentSamplesZTest = gettext("Independent Samples Z-Test"),
    pairedSamplesZTest      = gettext("Paired Samples Z-Test"),
    oneSampleZTest          = gettext("One Sample Z-Test"),
    oneSampleProportion     = gettext("One Sample Proportion Test"),
    generalZApproximation   = gettext("General (z-approximation)"),
    settings[["testLabel"]]
  )
}

.bfdObservedStatisticValue <- function(settings, summary) {
  if (settings[["isBinomial"]])
    return(summary[["successes"]])

  return(.bfdObservedValue(summary, "testStatistic"))
}

.bfdObservedFillSummaryTable <- function(table, settings, summary) {
  .bfdObservedAddSummaryColumns(table, settings, summary)
  table$setData(.bfdObservedSummaryRows(settings, summary))
}

.bfdObservedAddSummaryColumns <- function(table, settings, summary) {
  if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "group", title = gettext("Group"), type = "string")
    table$addColumnInfo(name = "n", title = "N", type = "integer")
    if (.bfdObservedSummaryHasAny(summary, c("mean1", "mean2")))
      table$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    if (.bfdObservedSummaryHasAny(summary, c("sd1", "sd2")))
      table$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
    return()
  }

  table$addColumnInfo(name = "variable", title = gettext("Variable"), type = "string")

  if (settings[["isBinomial"]]) {
    table$addColumnInfo(name = "successes", title = gettext("Successes"), type = "integer")
    table$addColumnInfo(name = "trials", title = gettext("Trials"), type = "integer")
    table$addColumnInfo(name = "proportion", title = gettext("Proportion"), type = "number")
    return()
  }

  if (.bfdObservedSummaryHasAny(summary, "n"))
    table$addColumnInfo(name = "n", title = "N", type = "integer")

  if (settings[["testType"]] == "paired") {
    if (.bfdObservedSummaryHasAny(summary, "meanDifference"))
      table$addColumnInfo(name = "meanDifference", title = gettext("Mean Difference"), type = "number")
    if (.bfdObservedSummaryHasAny(summary, "sdDifference"))
      table$addColumnInfo(name = "sdDifference", title = gettext("SD Difference"), type = "number")
  } else {
    if (.bfdObservedSummaryHasAny(summary, "mean"))
      table$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    if (.bfdObservedSummaryHasAny(summary, "sd"))
      table$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
  }

  table$addColumnInfo(name = "estimate", title = .bfdObservedEstimateTitle(settings), type = "number")
  table$addColumnInfo(name = "standardError", title = gettext("SE"), type = "number")
}

.bfdObservedSummaryRows <- function(settings, summary) {
  if (settings[["isBinomial"]])
    return(.bfdObservedBinomialSummaryRows(summary))

  if (settings[["isIndependentSamples"]])
    return(.bfdObservedIndependentSummaryRows(summary))

  if (settings[["testType"]] == "paired")
    return(.bfdObservedPairedSummaryRows(summary))

  return(.bfdObservedOneSampleSummaryRows(settings, summary))
}

.bfdObservedBinomialSummaryRows <- function(summary) {
  return(data.frame(
    variable   = summary[["variable"]],
    successes  = summary[["successes"]],
    trials     = summary[["trials"]],
    proportion = summary[["proportion"]],
    stringsAsFactors = FALSE
  ))
}

.bfdObservedIndependentSummaryRows <- function(summary) {
  groups <- c(.bfdObservedValue(summary, "group1"), .bfdObservedValue(summary, "group2"))
  groups[is.na(groups)] <- c(gettext("Group 1"), gettext("Group 2"))[is.na(groups)]

  rows <- data.frame(
    group = groups,
    n     = c(summary[["n1"]], summary[["n2"]]),
    stringsAsFactors = FALSE
  )

  if (.bfdObservedSummaryHasAny(summary, c("mean1", "mean2")))
    rows[["mean"]] <- c(.bfdObservedValue(summary, "mean1"), .bfdObservedValue(summary, "mean2"))

  if (.bfdObservedSummaryHasAny(summary, c("sd1", "sd2")))
    rows[["sd"]] <- c(.bfdObservedValue(summary, "sd1"), .bfdObservedValue(summary, "sd2"))

  return(rows)
}

.bfdObservedPairedSummaryRows <- function(summary) {
  row <- data.frame(variable = summary[["variable"]], stringsAsFactors = FALSE)

  if (.bfdObservedSummaryHasAny(summary, "n"))
    row[["n"]] <- summary[["n"]]

  if (.bfdObservedSummaryHasAny(summary, "meanDifference"))
    row[["meanDifference"]] <- summary[["meanDifference"]]

  if (.bfdObservedSummaryHasAny(summary, "sdDifference"))
    row[["sdDifference"]] <- summary[["sdDifference"]]

  row[["estimate"]]      <- summary[["estimate"]]
  row[["standardError"]] <- summary[["standardError"]]
  return(row)
}

.bfdObservedOneSampleSummaryRows <- function(settings, summary) {
  row <- data.frame(variable = summary[["variable"]], stringsAsFactors = FALSE)

  if (.bfdObservedSummaryHasAny(summary, "n"))
    row[["n"]] <- summary[["n"]]

  if (.bfdObservedSummaryHasAny(summary, "mean"))
    row[["mean"]] <- .bfdObservedValue(summary, "mean")
  if (.bfdObservedSummaryHasAny(summary, "sd"))
    row[["sd"]] <- summary[["sd"]]

  row[["estimate"]]      <- summary[["estimate"]]
  row[["standardError"]] <- summary[["standardError"]]
  return(row)
}

.bfdObservedSummaryHasAny <- function(summary, names) {
  values <- vapply(names, function(name) {
    value <- .bfdObservedValue(summary, name)
    length(value) == 1 && !is.na(value)
  }, logical(1))

  return(any(values))
}

.bfdObservedSummary <- function(dataset, options, settings) {
  input <- options[["observedDataAnalysisInput"]]
  if (input == "columns")
    return(.bfdObservedSummaryFromData(dataset, options, settings))

  return(.bfdObservedSummaryFromOptions(options, settings))
}

.bfdObservedAnalysisReady <- function(dataset, options, settings) {
  input <- options[["observedDataAnalysisInput"]]
  if (input == "columns")
    return(.bfdObservedColumnInputStarted(options, settings))

  return(.bfdObservedSummaryInputStarted(options, settings))
}

.bfdObservedSummaryInputStarted <- function(options, settings) {
  if (settings[["isBinomial"]]) {
    failures  <- .bfdObservedOptionNumber(options, "observedFailures")
    successes <- .bfdObservedOptionNumber(options, "observedSuccesses")
    return(failures > 0 || successes > 0)
  }

  if (settings[["isGeneralZ"]])
    return(.bfdObservedOptionNumber(options, "observedStandardError") > 0)

  if (settings[["isTTest"]])
    return(.bfdObservedTSampleSizeStarted(options, settings))

  if (settings[["isIndependentSamples"]]) {
    n1 <- .bfdObservedOptionNumber(options, "observedSampleSizeGroup1")
    n2 <- .bfdObservedOptionNumber(options, "observedSampleSizeGroup2")
    return(n1 > 0 || n2 > 0)
  }

  return(.bfdObservedOptionNumber(options, "observedSampleSize") > 0)
}

.bfdObservedTSampleSizeStarted <- function(options, settings) {
  if (settings[["isIndependentSamples"]]) {
    n1 <- .bfdObservedOptionNumber(options, "observedSampleSizeGroup1")
    n2 <- .bfdObservedOptionNumber(options, "observedSampleSizeGroup2")
    return(n1 > 0 || n2 > 0)
  }

  return(.bfdObservedOptionNumber(options, "observedSampleSize") > 0)
}

.bfdObservedColumnInputStarted <- function(options, settings) {
  if (settings[["isBinomial"]])
    return(.bfdObservedHasOption(options, "observedProportionVariable"))

  if (settings[["isIndependentSamples"]]) {
    return(any(vapply(c("observedDependentVariable", "observedGroupingVariable"), function(name) {
      .bfdObservedHasOption(options, name)
    }, logical(1))))
  }

  if (settings[["testType"]] == "paired") {
    return(.bfdObservedHasPair(options))
  }

  return(.bfdObservedHasOption(options, "observedVariable"))
}

.bfdObservedOptionNumber <- function(options, name) {
  value <- options[[name]]
  if (is.null(value))
    stop(gettextf("Missing option '%1$s'.", name))

  if (length(value) != 1 || !is.numeric(value) || !is.finite(value))
    stop(gettextf("Option '%1$s' must contain one finite numeric value.", name))

  return(value)
}

.bfdObservedHasOption <- function(options, name) {
  value <- options[[name]]
  if (is.null(value))
    stop(gettextf("Missing option '%1$s'.", name))

  if (length(value) < 1)
    return(FALSE)

  value <- value[[1]]
  return(length(value) == 1 && !is.na(value) && nzchar(value))
}

.bfdObservedSummaryFromOptions <- function(options, settings) {
  source <- gettext("Summary statistics")

  if (settings[["isTTest"]])
    return(.bfdObservedTSummaryFromOptions(options, settings, source))

  if (settings[["isBinomial"]]) {
    counts <- .bfdObservedBinomialCounts(options)

    return(list(
      source     = source,
      variable   = gettext("Summary statistics"),
      successes  = counts[["successes"]],
      trials     = counts[["trials"]],
      proportion = counts[["successes"]] / counts[["trials"]]
    ))
  }

  if (settings[["isGeneralZ"]]) {
    estimate <- .bfdObservedNumber(options, "observedEffectSize", gettext("Estimate"))
    se       <- .bfdObservedPositive(options, "observedStandardError", gettext("SE"))
    summary  <- .bfdObservedZSummary(source, gettext("Summary statistics"), estimate, se)
    summary[["testStatistic"]] <- (estimate - settings[["nullValue"]]) / se
    return(summary)
  }

  if (settings[["isIndependentSamples"]]) {
    n1    <- .bfdObservedInteger(options, "observedSampleSizeGroup1", "N1", minimum = if (settings[["isTTest"]]) 2 else 1)
    n2    <- .bfdObservedInteger(options, "observedSampleSizeGroup2", "N2", minimum = if (settings[["isTTest"]]) 2 else 1)
    mean1 <- .bfdObservedNumber(options, "observedMean1", gettext("Mean 1"))
    mean2 <- .bfdObservedNumber(options, "observedMean2", gettext("Mean 2"))

    if (settings[["isTTest"]]) {
      sd1 <- .bfdObservedPositive(options, "observedSd1", gettext("SD 1"))
      sd2 <- .bfdObservedPositive(options, "observedSd2", gettext("SD 2"))
      return(.bfdObservedIndependentTSummary(source, gettext("Summary statistics"), n1, n2, mean1, mean2, sd1, sd2, settings))
    }

    return(.bfdObservedIndependentZSummary(source, gettext("Summary statistics"), n1, n2, mean1, mean2, settings))
  }

  n <- .bfdObservedInteger(options, "observedSampleSize", "N", minimum = if (settings[["isTTest"]]) 2 else 1)
  if (settings[["testType"]] == "paired") {
    meanDifference <- .bfdObservedNumber(options, "observedMeanDifference", gettext("Mean difference"))
    if (settings[["isTTest"]]) {
      sdDifference <- .bfdObservedPositive(options, "observedSdDifference", gettext("SD difference"))
      return(.bfdObservedPairedTSummary(source, gettext("Summary statistics"), n, meanDifference, sdDifference, settings))
    }

    return(.bfdObservedPairedZSummary(source, gettext("Summary statistics"), n, meanDifference, settings))
  }

  mean <- .bfdObservedNumber(options, "observedMean", gettext("Mean"))
  if (settings[["isTTest"]]) {
    sd <- .bfdObservedPositive(options, "observedSd", gettext("SD"))
    return(.bfdObservedOneSampleTSummary(source, gettext("Summary statistics"), n, mean, sd, settings))
  }

  return(.bfdObservedOneSampleZSummary(source, gettext("Summary statistics"), n, mean, settings))
}

.bfdObservedTSummaryFromOptions <- function(options, settings, source) {
  inputType <- .bfdObservedTInputType(options, settings)

  if (settings[["isIndependentSamples"]])
    return(.bfdObservedIndependentTSummaryFromOptions(options, settings, source, inputType))

  return(.bfdObservedDependentTSummaryFromOptions(options, settings, source, inputType))
}

.bfdObservedTInputType <- function(options, settings) {
  inputType <- options[["observedInputType"]]

  valid <- if (settings[["isIndependentSamples"]]) {
    c("tAndN", "cohensD", "meansAndSDs")
  } else if (settings[["testType"]] == "paired") {
    c("tAndN", "cohensD", "meanDiffAndSD")
  } else {
    c("tAndN", "cohensD", "meanAndSD")
  }

  if (length(inputType) != 1 || is.na(inputType) || !inputType %in% valid)
    stop(gettext("Observed t-test input type is invalid."))

  return(inputType)
}

.bfdObservedIndependentTSummaryFromOptions <- function(options, settings, source, inputType) {
  n1 <- .bfdObservedInteger(options, "observedSampleSizeGroup1", "N1", minimum = 2)
  n2 <- .bfdObservedInteger(options, "observedSampleSizeGroup2", "N2", minimum = 2)

  if (inputType == "meansAndSDs") {
    mean1 <- .bfdObservedNumber(options, "observedMean1", gettext("Mean 1"))
    mean2 <- .bfdObservedNumber(options, "observedMean2", gettext("Mean 2"))
    sd1   <- .bfdObservedPositive(options, "observedSd1", gettext("SD 1"))
    sd2   <- .bfdObservedPositive(options, "observedSd2", gettext("SD 2"))
    return(.bfdObservedIndependentTSummary(source, gettext("Summary statistics"), n1, n2, mean1, mean2, sd1, sd2, settings))
  }

  standardError <- sqrt(1 / n1 + 1 / n2)
  if (inputType == "cohensD") {
    estimate <- .bfdObservedNumber(options, "observedCohensD", gettext("Cohen's d"))
    t        <- (estimate - settings[["nullValue"]]) / standardError
  } else {
    t        <- .bfdObservedNumber(options, "observedT", "t")
    estimate <- settings[["nullValue"]] + t * standardError
  }

  return(.bfdObservedTStatisticSummary(
    source        = source,
    variable      = gettext("Summary statistics"),
    estimate      = estimate,
    standardError = standardError,
    t             = t,
    n1            = n1,
    n2            = n2
  ))
}

.bfdObservedDependentTSummaryFromOptions <- function(options, settings, source, inputType) {
  n <- .bfdObservedInteger(options, "observedSampleSize", "N", minimum = 2)

  if (settings[["testType"]] == "paired" && inputType == "meanDiffAndSD") {
    meanDifference <- .bfdObservedNumber(options, "observedMeanDifference", gettext("Mean difference"))
    sdDifference   <- .bfdObservedPositive(options, "observedSdDifference", gettext("SD difference"))
    return(.bfdObservedPairedTSummary(source, gettext("Summary statistics"), n, meanDifference, sdDifference, settings))
  }

  if (settings[["testType"]] != "paired" && inputType == "meanAndSD") {
    mean <- .bfdObservedNumber(options, "observedMean", gettext("Mean"))
    sd   <- .bfdObservedPositive(options, "observedSd", gettext("SD"))
    return(.bfdObservedOneSampleTSummary(source, gettext("Summary statistics"), n, mean, sd, settings))
  }

  standardError <- 1 / sqrt(n)
  if (inputType == "cohensD") {
    estimate <- .bfdObservedNumber(options, "observedCohensD", gettext("Cohen's d"))
    t        <- (estimate - settings[["nullValue"]]) / standardError
  } else {
    t        <- .bfdObservedNumber(options, "observedT", "t")
    estimate <- settings[["nullValue"]] + t * standardError
  }

  return(.bfdObservedTStatisticSummary(
    source        = source,
    variable      = gettext("Summary statistics"),
    estimate      = estimate,
    standardError = standardError,
    t             = t,
    n             = n
  ))
}

.bfdObservedBinomialCounts <- function(options) {
  successes <- .bfdObservedInteger(options, "observedSuccesses", gettext("Successes"), minimum = 0)
  failures <- .bfdObservedInteger(options, "observedFailures", gettext("Failures"), minimum = 0)
  trials   <- successes + failures
  if (trials < 1)
    stop(gettext("The total number of successes and failures must be at least 1."))

  return(list(successes = successes, trials = trials))
}

.bfdObservedTStatisticSummary <- function(source, variable, estimate, standardError, t, n = NA_integer_,
                                         n1 = NA_integer_, n2 = NA_integer_) {
  summary <- list(
    source        = source,
    variable      = variable,
    estimate      = estimate,
    standardError = standardError,
    testStatistic = t
  )

  if (!is.na(n1) || !is.na(n2)) {
    summary[["n1"]] <- n1
    summary[["n2"]] <- n2
  } else {
    summary[["n"]] <- n
  }

  return(summary)
}

.bfdObservedSummaryFromData <- function(dataset, options, settings) {
  if (is.null(dataset))
    stop(gettext("No dataset is available for column input."))

  source <- gettext("Column data")

  if (settings[["isBinomial"]])
    return(.bfdObservedBinomialSummaryFromData(dataset, options, source))

  if (settings[["isIndependentSamples"]])
    return(.bfdObservedIndependentSummaryFromData(dataset, options, settings, source))

  if (settings[["testType"]] == "paired")
    return(.bfdObservedPairedSummaryFromData(dataset, options, settings, source))

  return(.bfdObservedOneSampleSummaryFromData(dataset, options, settings, source))
}

.bfdObservedBinomialSummaryFromData <- function(dataset, options, source) {
  variable <- .bfdObservedVariableName(options, "observedProportionVariable", gettext("Variable"))
  values   <- .bfdObservedColumn(dataset, variable, gettext("Variable"))
  values   <- values[!is.na(values)]
  if (length(values) < 1)
    stop(gettext("The selected variable contains no observed values."))

  successValue <- options[["observedSuccessValue"]]
  successes    <- sum(as.character(values) == successValue)
  trials       <- length(values)

  return(list(
    source     = source,
    variable   = .bfdObservedDecodeVariable(variable),
    successes  = successes,
    trials     = trials,
    proportion = successes / trials
  ))
}

.bfdObservedIndependentSummaryFromData <- function(dataset, options, settings, source) {
  dependentVariable <- .bfdObservedVariableName(options, "observedDependentVariable", gettext("Dependent variable"))
  groupingVariable  <- .bfdObservedVariableName(options, "observedGroupingVariable", gettext("Grouping variable"))
  y                 <- .bfdObservedColumn(dataset, dependentVariable, gettext("Dependent variable"))
  group             <- .bfdObservedColumn(dataset, groupingVariable, gettext("Grouping variable"))
  complete          <- stats::complete.cases(y, group)
  y                 <- y[complete]
  group             <- group[complete]

  y <- .bfdObservedFiniteNumeric(y, gettext("Dependent variable"))
  group <- droplevels(as.factor(group))
  levels <- levels(group)
  if (length(levels) != 2)
    stop(gettext("The grouping variable must contain exactly two observed groups."))

  values1 <- y[group == levels[1]]
  values2 <- y[group == levels[2]]
  n1      <- length(values1)
  n2      <- length(values2)
  mean1   <- mean(values1)
  mean2   <- mean(values2)
  variable <- gettextf(
    "%1$s by %2$s",
    .bfdObservedDecodeVariable(dependentVariable),
    .bfdObservedDecodeVariable(groupingVariable)
  )

  if (settings[["isTTest"]]) {
    .bfdObservedCheckSampleSize(n1, "N1", minimum = 2)
    .bfdObservedCheckSampleSize(n2, "N2", minimum = 2)
    summary <- .bfdObservedIndependentTSummary(source, variable, n1, n2, mean1, mean2, stats::sd(values1), stats::sd(values2), settings)
    summary[["group1"]] <- levels[1]
    summary[["group2"]] <- levels[2]
    return(summary)
  }

  .bfdObservedCheckSampleSize(n1, "N1", minimum = 1)
  .bfdObservedCheckSampleSize(n2, "N2", minimum = 1)
  summary <- .bfdObservedIndependentZSummary(source, variable, n1, n2, mean1, mean2, settings, stats::sd(values1), stats::sd(values2))
  summary[["group1"]] <- levels[1]
  summary[["group2"]] <- levels[2]
  return(summary)
}

.bfdObservedPairedSummaryFromData <- function(dataset, options, settings, source) {
  variables      <- .bfdObservedPairVariables(options)
  firstVariable  <- variables[[1]]
  secondVariable <- variables[[2]]
  first          <- .bfdObservedColumn(dataset, firstVariable, gettext("First variable"))
  second         <- .bfdObservedColumn(dataset, secondVariable, gettext("Second variable"))
  complete       <- stats::complete.cases(first, second)
  first          <- .bfdObservedFiniteNumeric(first[complete], gettext("First variable"))
  second         <- .bfdObservedFiniteNumeric(second[complete], gettext("Second variable"))
  difference     <- first - second
  n              <- length(difference)
  meanDifference <- mean(difference)
  variable       <- gettextf("%1$s - %2$s", .bfdObservedDecodeVariable(firstVariable), .bfdObservedDecodeVariable(secondVariable))

  if (settings[["isTTest"]]) {
    .bfdObservedCheckSampleSize(n, "N", minimum = 2)
    return(.bfdObservedPairedTSummary(source, variable, n, meanDifference, stats::sd(difference), settings))
  }

  .bfdObservedCheckSampleSize(n, "N", minimum = 1)
  return(.bfdObservedPairedZSummary(source, variable, n, meanDifference, settings, sdDifference = if (n > 1) stats::sd(difference) else NA_real_))
}

.bfdObservedHasPair <- function(options) {
  pairs <- options[["observedVariablePairs"]]
  return(length(pairs) > 0 && any(vapply(pairs, .bfdObservedValidPair, logical(1))))
}

.bfdObservedPairVariables <- function(options) {
  pairs <- options[["observedVariablePairs"]]
  if (length(pairs) > 0) {
    for (pair in pairs) {
      variables <- .bfdObservedPairValues(pair)
      if (length(variables) == 2) {
        if (identical(variables[[1]], variables[[2]]))
          stop(gettext("Variable pair must contain two different variables."))

        return(variables)
      }
    }
  }

  stop(gettext("Variable pair must be selected."))
}

.bfdObservedValidPair <- function(pair) {
  length(.bfdObservedPairValues(pair)) == 2
}

.bfdObservedPairValues <- function(pair) {
  values <- unlist(pair, use.names = FALSE)
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) < 2)
    return(character(0))

  return(as.character(values[1:2]))
}

.bfdObservedOneSampleSummaryFromData <- function(dataset, options, settings, source) {
  variable <- .bfdObservedVariableName(options, "observedVariable", gettext("Variable"))
  values   <- .bfdObservedFiniteNumeric(.bfdObservedColumn(dataset, variable, gettext("Variable")), gettext("Variable"))
  n        <- length(values)
  mean     <- mean(values)
  sd       <- if (n > 1) stats::sd(values) else NA_real_

  if (settings[["isGeneralZ"]]) {
    .bfdObservedCheckSampleSize(n, "N", minimum = 2)
    return(.bfdObservedGeneralZSummary(source, .bfdObservedDecodeVariable(variable), n, mean, sd, settings))
  }

  if (settings[["isTTest"]]) {
    .bfdObservedCheckSampleSize(n, "N", minimum = 2)
    return(.bfdObservedOneSampleTSummary(source, .bfdObservedDecodeVariable(variable), n, mean, sd, settings))
  }

  .bfdObservedCheckSampleSize(n, "N", minimum = 1)
  return(.bfdObservedOneSampleZSummary(source, .bfdObservedDecodeVariable(variable), n, mean, settings, sd = sd))
}

.bfdObservedIndependentTSummary <- function(source, variable, n1, n2, mean1, mean2, sd1, sd2, settings) {
  .bfdObservedCheckPositive(sd1, gettext("SD 1"))
  .bfdObservedCheckPositive(sd2, gettext("SD 2"))

  pooledSd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  .bfdObservedCheckPositive(pooledSd, gettext("Pooled SD"))

  standardError <- sqrt(1 / n1 + 1 / n2)
  estimate      <- (mean1 - mean2) / pooledSd
  t             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n1 = n1, n2 = n2, mean1 = mean1,
    mean2 = mean2, sd1 = sd1, sd2 = sd2, estimate = estimate,
    standardError = standardError, testStatistic = t
  ))
}

.bfdObservedIndependentZSummary <- function(source, variable, n1, n2, mean1, mean2, settings, sd1 = NA_real_, sd2 = NA_real_) {
  standardError <- settings[["standardDeviation"]] * sqrt(1 / n1 + 1 / n2)
  estimate      <- mean1 - mean2
  z             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n1 = n1, n2 = n2, mean1 = mean1,
    mean2 = mean2, sd1 = sd1, sd2 = sd2, estimate = estimate,
    standardError = standardError, testStatistic = z
  ))
}

.bfdObservedPairedTSummary <- function(source, variable, n, meanDifference, sdDifference, settings) {
  .bfdObservedCheckPositive(sdDifference, gettext("SD difference"))
  standardError <- 1 / sqrt(n)
  estimate      <- meanDifference / sdDifference
  t             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, meanDifference = meanDifference,
    sdDifference = sdDifference, estimate = estimate, standardError = standardError,
    testStatistic = t
  ))
}

.bfdObservedPairedZSummary <- function(source, variable, n, meanDifference, settings, sdDifference = NA_real_) {
  standardError <- settings[["standardDeviation"]] / sqrt(n)
  z             <- (meanDifference - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, meanDifference = meanDifference,
    sdDifference = sdDifference, estimate = meanDifference,
    standardError = standardError, testStatistic = z
  ))
}

.bfdObservedOneSampleTSummary <- function(source, variable, n, mean, sd, settings) {
  .bfdObservedCheckPositive(sd, gettext("SD"))
  standardError <- 1 / sqrt(n)
  estimate      <- mean / sd
  t             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, mean = mean, sd = sd,
    estimate = estimate, standardError = standardError, testStatistic = t
  ))
}

.bfdObservedOneSampleZSummary <- function(source, variable, n, mean, settings, sd = NA_real_) {
  standardError <- settings[["standardDeviation"]] / sqrt(n)
  z             <- (mean - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, mean = mean, sd = sd,
    estimate = mean, standardError = standardError, testStatistic = z
  ))
}

.bfdObservedGeneralZSummary <- function(source, variable, n, mean, sd, settings) {
  .bfdObservedCheckPositive(sd, gettext("SD"))
  standardError <- sd / sqrt(n)
  summary <- .bfdObservedZSummary(source, variable, mean, standardError)
  summary[["n"]]             <- n
  summary[["mean"]]          <- mean
  summary[["sd"]]            <- sd
  summary[["testStatistic"]] <- (mean - settings[["nullValue"]]) / standardError

  return(summary)
}

.bfdObservedZSummary <- function(source, variable, estimate, standardError) {
  return(list(
    source = source, variable = variable, n = NA_integer_, mean = NA_real_,
    sd = NA_real_, estimate = estimate, standardError = standardError,
    testStatistic = NA_real_
  ))
}

.bfdObservedBayesFactor <- function(settings, summary) {
  bf01 <- if (settings[["isBinomial"]]) {
    .bfdObservedBinomialBayesFactor(settings, summary)
  } else if (settings[["isTTest"]]) {
    .bfdObservedTBayesFactor(settings, summary)
  } else {
    .bfdObservedZBayesFactor(settings, summary)
  }

  if (!is.finite(bf01) || bf01 <= 0)
    stop(gettext("Observed Bayes factor is not finite."))

  return(list(bf01 = bf01, bf10 = 1 / bf01))
}

.bfdObservedZBayesFactor <- function(settings, summary) {
  estimate <- summary[["estimate"]]
  se       <- summary[["standardError"]]

  if (is.na(summary[["testStatistic"]]))
    summary[["testStatistic"]] <- (estimate - settings[["nullValue"]]) / se

  if (isTRUE(.bfdUsesDirectionalZTest(settings))) {
    null <- settings[["nullValue"]]
    pm   <- settings[["analysisPriorMean"]]
    if (identical(settings[["alternative"]], "less")) {
      estimate <- 2 * null - estimate
      pm       <- 2 * null - pm
    }

    return(bfpwr::dirbf01(
      estimate = estimate,
      se       = se,
      null     = null,
      pm       = pm,
      psd      = settings[["analysisPriorSd"]]
    ))
  }

  if (settings[["analysisPriorDistribution"]] %in% c("point", "normal")) {
    return(bfpwr::bf01(
      estimate = estimate,
      se       = se,
      null     = settings[["nullValue"]],
      pm       = settings[["analysisPriorMean"]],
      psd      = settings[["analysisPriorSd"]]
    ))
  }

  return(bfpwr::nmbf01(
    estimate = estimate,
    se       = se,
    null     = settings[["nullValue"]],
    psd      = settings[["momentPriorSpread"]]
  ))
}

.bfdObservedTBayesFactor <- function(settings, summary) {
  n1 <- if (settings[["isIndependentSamples"]]) summary[["n1"]] else summary[["n"]]
  n2 <- if (settings[["isIndependentSamples"]]) summary[["n2"]] else summary[["n"]]

  return(bfpwr::tbf01(
    t           = summary[["testStatistic"]],
    n           = n1,
    n1          = n1,
    n2          = n2,
    plocation   = settings[["tPriorLocationRelative"]],
    pscale      = settings[["tPriorScale"]],
    pdf         = settings[["tPriorDf"]],
    type        = settings[["testType"]],
    alternative = settings[["alternative"]]
  ))
}

.bfdObservedBinomialBayesFactor <- function(settings, summary) {
  return(bfpwr::binbf01(
    x    = summary[["successes"]],
    n    = summary[["trials"]],
    p0   = settings[["nullProportion"]],
    type = settings[["nullPriorDistribution"]],
    a    = settings[["analysisPriorSuccesses"]],
    b    = settings[["analysisPriorFailures"]]
  ))
}

.bfdObservedDecision <- function(settings, bayesFactor, sequential) {
  if (bayesFactor[["bf10"]] >= settings[["bf10Threshold"]])
    return(if (isTRUE(sequential)) gettext("Stop for H\u2081") else gettext("BF supports H\u2081"))

  if (bayesFactor[["bf01"]] >= settings[["bf01Threshold"]])
    return(if (isTRUE(sequential)) gettext("Stop for H\u2080") else gettext("BF supports H\u2080"))

  if (isTRUE(sequential))
    return(gettext("Continue"))

  return(gettext("Inconclusive"))
}

.bfdObservedVariableName <- function(options, name, label) {
  variable <- options[[name]]
  if (length(variable) != 1 || is.na(variable) || variable == "")
    stop(gettextf("%1$s must be selected.", label))

  return(variable)
}

.bfdObservedColumn <- function(dataset, variable, label) {
  if (!variable %in% names(dataset))
    stop(gettextf("%1$s was not found in the dataset.", label))

  return(dataset[[variable]])
}

.bfdObservedFiniteNumeric <- function(values, label) {
  values <- values[!is.na(values)]
  if (length(values) < 1)
    stop(gettextf("%1$s contains no observed values.", label))

  if (!is.numeric(values))
    stop(gettextf("%1$s must be numeric.", label))

  if (any(!is.finite(values)))
    stop(gettextf("%1$s contains infinite values.", label))

  return(values)
}

.bfdObservedNumber <- function(options, name, label) {
  value <- options[[name]]
  if (length(value) != 1 || !is.numeric(value) || !is.finite(value))
    stop(gettextf("%1$s must be a finite number.", label))

  return(value)
}

.bfdObservedPositive <- function(options, name, label) {
  value <- .bfdObservedNumber(options, name, label)
  .bfdObservedCheckPositive(value, label)
  return(value)
}

.bfdObservedInteger <- function(options, name, label, minimum) {
  value <- .bfdObservedNumber(options, name, label)
  if (value < minimum || abs(value - round(value)) > sqrt(.Machine$double.eps))
    stop(gettextf("%1$s must be an integer of at least %2$s.", label, minimum))

  return(as.integer(round(value)))
}

.bfdObservedCheckSampleSize <- function(n, label, minimum) {
  if (n < minimum)
    stop(gettextf("%1$s must be at least %2$s.", label, minimum))
}

.bfdObservedCheckPositive <- function(value, label) {
  if (!is.finite(value) || value <= 0)
    stop(gettextf("%1$s must be positive.", label))
}

.bfdObservedValue <- function(summary, name) {
  value <- summary[[name]]
  if (is.null(value))
    return(NA)

  return(value)
}

.bfdObservedEstimateTitle <- function(settings) {
  if (settings[["isTTest"]])
    return("SMD")

  return(gettext("Estimate"))
}

.bfdObservedStatisticTitle <- function(settings) {
  if (settings[["isBinomial"]])
    return("x")

  if (settings[["isTTest"]])
    return("t")

  return("z")
}

.bfdObservedDecodeVariable <- function(variable) {
  decoded <- try(jaspBase::decodeColNames(variable), silent = TRUE)
  if (jaspBase::isTryError(decoded))
    return(variable)

  return(decoded)
}

.bfdText <- function(jaspResults, settings, result) {
  html <- .bfdCreateHtml(
    parent       = jaspResults,
    key          = "evidenceText",
    title        = gettext("Explanation"),
    position     = 4,
    dependencies = .bfdTextDependencies
  )
  if (is.null(html))
    return()

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested Bayes factor design could not be completed with the current settings.")
    return()
  }

  if (.bfdHasTargetErrors(result)) {
    html[["text"]] <- gettext("The Bayes factor design could not be completed for all design priors. See the table footnotes for details.")
    return()
  }

  if (settings[["calculation"]] == "sampleSize") {
    sampleText <- gettextf("The smallest sample size satisfying the selected planning targets is %1$s.", .bfdSampleSizeText(settings, result[["n1"]]))
    calculationText <- gettextf(
      "%1$s The corresponding conclusive evidence probabilities are %2$s.",
      sampleText,
      .bfdTargetProbabilityText(result)
    )
  } else {
    h1Result <- .bfdTargetResult(result, "h1")
    h0Result <- .bfdTargetResult(result, "h0")
    sampleText <- gettextf("With %1$s", .bfdSampleSizeText(settings, result[["n1"]]))
    calculationText <- gettextf(
      "%1$s, Pr(Conclusive Evidence) is %2$s for H\u2081 and %3$s for H\u2080.",
      sampleText,
      .bfdFormatNumber(h1Result[["probability"]]),
      .bfdFormatNumber(h0Result[["probability"]])
    )
  }

  html[["text"]] <- paste0(
    "<p>",
    gettextf(
      "This Bayes factor design computes the probability of reaching the selected BF\u2081\u2080 or BF\u2080\u2081 thresholds for %1$s, averaging over the corresponding design prior.",
      settings[["testLabel"]]
    ),
    "</p><p>",
    calculationText,
    "</p>"
  )
}

.bfdTargetProbabilityText <- function(result) {
  rows <- result[["targetResults"]]
  parts <- vapply(seq_len(nrow(rows)), function(i) {
    gettextf("%1$s for %2$s", .bfdFormatNumber(rows[["probability"]][i]), .bfdTargetLabel(rows[["target"]][i]))
  }, character(1))

  return(paste(parts, collapse = ", "))
}

.bfdTargetResult <- function(result, target) {
  rows <- result[["targetResults"]]
  index <- which(rows[["target"]] == target)
  if (length(index) == 0)
    return(rows[1, , drop = FALSE])

  return(rows[index[1], , drop = FALSE])
}

.bfdSampleSizeText <- function(settings, n1) {
  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "N\u2081 = %1$s and N\u2082 = %2$s",
      n1,
      .bfdSampleSizeSecondGroup(settings, n1)
    ))
  }

  return(gettextf("N = %1$s", n1))
}

.bfdReport <- function(jaspResults, settings, result) {
  html <- .bfdCreateHtml(
    parent       = jaspResults,
    key          = "evidenceReport",
    title        = gettext("Report"),
    position     = 0,
    dependencies = .bfdReportDependencies
  )
  if (is.null(html))
    return()

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The report could not be generated because the Bayes factor design could not be completed with the current settings.")
    return()
  }

  if (.bfdHasTargetErrors(result)) {
    html[["text"]] <- gettext("The report could not be generated because the Bayes factor design could not be completed for all design priors.")
    return()
  }

  paragraph <- try(
    paste(
      .bfdReportOpeningSentence(settings, designType = gettext("fixed-sample")),
      .bfdReportFixedPlanningSentence(settings, result),
      .bfdReportThresholdSentence(settings),
      .bfdReportPriorSentence(settings),
      .bfdReportProbabilitySentence(
        settings,
        h1Outcome = .bfdDesignOutcomeProbabilities(settings, .bfdResultN1ForBasis(settings, result, "h1"), under = "h1"),
        h0Outcome = .bfdDesignOutcomeProbabilities(settings, .bfdResultN1ForBasis(settings, result, "h0"), under = "h0")
      ),
      .bfdReportSoftwareSentence()
    ),
    silent = TRUE
  )
  if (jaspBase::isTryError(paragraph)) {
    html[["text"]] <- gettextf("Unable to generate report: %1$s", .bfdCleanError(paragraph))
    return()
  }

  html[["text"]] <- .bfdReportHtml(paragraph, settings)
}

.bfdReportOpeningSentence <- function(settings, designType) {
  gettextf(
    "A %1$s Bayes factor design %2$s a %3$s %4$s %5$s.",
    designType,
    .bfdReportDesignAction(settings),
    .bfdReportAlternativeLabel(settings),
    .bfdReportTestLabel(settings),
    .bfdReportHypothesisClause(settings)
  )
}

.bfdReportDesignAction <- function(settings) {
  if (identical(settings[["calculation"]], "sampleSize"))
    return(gettext("specified"))

  return(gettext("evaluated"))
}

.bfdReportAlternativeLabel <- function(settings) {
  if (.bfdReportIsOneSided(settings))
    return(gettext("one-sided"))

  return(gettext("two-sided"))
}

.bfdReportIsOneSided <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return(identical(settings[["nullPriorDistribution"]], "direction"))

  if (.bfdUsesDirectionalZTest(settings))
    return(TRUE)

  alternative <- settings[["alternative"]]
  return(!is.null(alternative) && !identical(alternative, "two.sided"))
}

.bfdReportTestLabel <- function(settings) {
  switch(settings[["test"]],
    independentSamplesTTest = gettext("independent-samples t test"),
    pairedSamplesTTest      = gettext("paired-samples t test"),
    oneSampleTTest          = gettext("one-sample t test"),
    independentSamplesZTest = gettext("independent-samples z test"),
    pairedSamplesZTest      = gettext("paired-samples z test"),
    oneSampleZTest          = gettext("one-sample z test"),
    oneSampleProportion     = gettext("one-sample proportion test"),
    generalZApproximation   = gettext("general z-approximation"),
    settings[["testLabel"]]
  )
}

.bfdReportHypothesisClause <- function(settings) {
  gettextf(
    "(H\u2081: %1$s %2$s %3$s)",
    .bfdReportParameterSymbol(settings),
    .bfdReportH1Relation(settings),
    .bfdReportNumber(.bfdReportNullValue(settings))
  )
}

.bfdReportH1Relation <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]) && identical(settings[["nullPriorDistribution"]], "direction"))
    return(">")

  alternative <- settings[["alternative"]]
  if (identical(alternative, "less"))
    return("<")
  if (identical(alternative, "greater"))
    return(">")

  return("\u2260")
}

.bfdReportParameterSymbol <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return("p")

  return("\u03B4")
}

.bfdReportNullValue <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return(settings[["nullProportion"]])

  return(settings[["nullValue"]])
}

.bfdReportPriorSentence <- function(settings) {
  paste(
    gettextf(
      "The analysis prior for H\u2080 %1$s, and the analysis prior for H\u2081 %2$s.",
      .bfdReportAnalysisPriorPhrase(settings, "h0"),
      .bfdReportAnalysisPriorPhrase(settings, "h1")
    ),
    gettextf(
      "The design prior for H\u2080 %1$s, and the design prior for H\u2081 %2$s.",
      .bfdReportDesignPriorPhrase(settings, "h0"),
      .bfdReportDesignPriorPhrase(settings, "h1")
    )
  )
}

.bfdReportAnalysisPriorPhrase <- function(settings, under) {
  if (isTRUE(settings[["isBinomial"]]))
    return(.bfdReportBinomialAnalysisPriorPhrase(settings, under))

  if (under == "h0")
    return(.bfdReportContinuousNullPriorPhrase(settings))

  return(.bfdReportContinuousAnalysisPriorPhrase(settings))
}

.bfdReportContinuousNullPriorPhrase <- function(settings) {
  if (.bfdUsesDirectionalZTest(settings)) {
    return(gettextf(
      "used %1$s, truncated to %2$s",
      .bfdReportContinuousZPriorDistribution(settings),
      .bfdReportHypothesisRegion(settings, "h0")
    ))
  }

  return(.bfdReportPointMassPhrase(.bfdReportParameterSymbol(settings), settings[["nullValue"]]))
}

.bfdReportContinuousAnalysisPriorPhrase <- function(settings) {
  if (settings[["isTTest"]]) {
    prior <- if (isTRUE(settings[["analysisPriorIsCauchy"]])) {
      gettextf(
        "%1$s ~ Cauchy(location = %2$s, scale = %3$s)",
        .bfdReportParameterSymbol(settings),
        .bfdReportNumber(settings[["tPriorLocation"]]),
        .bfdReportNumber(settings[["tPriorScale"]])
      )
    } else {
      gettextf(
        "%1$s ~ Student-t(location = %2$s, scale = %3$s, df = %4$s)",
        .bfdReportParameterSymbol(settings),
        .bfdReportNumber(settings[["tPriorLocation"]]),
        .bfdReportNumber(settings[["tPriorScale"]]),
        .bfdReportNumber(settings[["tPriorDf"]])
      )
    }
  } else if (settings[["analysisPriorDistribution"]] == "point") {
    return(.bfdReportPointMassPhrase(.bfdReportParameterSymbol(settings), settings[["analysisPriorMean"]]))
  } else if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    prior <- .bfdReportContinuousZPriorDistribution(settings)
  } else {
    prior <- gettextf(
      "%1$s ~ Normal-moment(spread = %2$s, modes = \u00B1%3$s)",
      .bfdReportParameterSymbol(settings),
      .bfdReportNumber(settings[["momentPriorSpread"]]),
      .bfdReportNumber(settings[["momentPriorMode"]])
    )
  }

  if (.bfdReportIsOneSided(settings))
    return(gettextf("used %1$s, truncated to %2$s", prior, .bfdReportHypothesisRegion(settings, "h1")))

  return(gettextf("used %1$s", prior))
}

.bfdReportContinuousZPriorDistribution <- function(settings) {
  gettextf(
    "%1$s ~ Normal(mean = %2$s, SD = %3$s)",
    .bfdReportParameterSymbol(settings),
    .bfdReportNumber(settings[["analysisPriorMean"]]),
    .bfdReportNumber(settings[["analysisPriorSd"]])
  )
}

.bfdReportBinomialAnalysisPriorPhrase <- function(settings, under) {
  if (under == "h0" && !identical(settings[["nullPriorDistribution"]], "direction"))
    return(.bfdReportPointMassPhrase("p", settings[["nullProportion"]]))

  prior <- .bfdReportBetaPriorDistribution(
    symbol = "p",
    a      = settings[["analysisPriorSuccesses"]],
    b      = settings[["analysisPriorFailures"]]
  )

  if (identical(settings[["nullPriorDistribution"]], "direction"))
    return(gettextf("used %1$s, truncated to %2$s", prior, .bfdReportHypothesisRegion(settings, under)))

  return(gettextf("used %1$s", prior))
}

.bfdReportDesignPriorPhrase <- function(settings, under) {
  if (isTRUE(settings[["isBinomial"]]))
    return(.bfdReportBinomialDesignPriorPhrase(settings, under))

  prior <- .bfdContinuousDesignPriorForUnder(settings, under)
  if (identical(prior[["distribution"]], "point"))
    return(.bfdReportPointMassPhrase(.bfdReportParameterSymbol(settings), prior[["mean"]]))

  return(gettextf(
    "used %1$s ~ Normal(mean = %2$s, SD = %3$s)",
    .bfdReportParameterSymbol(settings),
    .bfdReportNumber(prior[["mean"]]),
    .bfdReportNumber(prior[["sd"]])
  ))
}

.bfdReportBinomialDesignPriorPhrase <- function(settings, under) {
  prior <- .bfdBinomialDesignPriorForUnder(settings, under)
  if (identical(prior[["distribution"]], "point"))
    return(.bfdReportPointMassPhrase("p", prior[["proportion"]]))

  return(gettextf(
    "used %1$s, truncated to %2$s \u2264 p \u2264 %3$s",
    .bfdReportBetaPriorDistribution("p", prior[["a"]], prior[["b"]]),
    .bfdReportNumber(prior[["lower"]]),
    .bfdReportNumber(prior[["upper"]])
  ))
}

.bfdReportPointMassPhrase <- function(symbol, value) {
  gettextf(
    "placed all mass at %1$s = %2$s",
    symbol,
    .bfdReportNumber(value)
  )
}

.bfdReportBetaPriorDistribution <- function(symbol, a, b) {
  gettextf(
    "%1$s ~ Beta(a = %2$s, b = %3$s)",
    symbol,
    .bfdReportNumber(a),
    .bfdReportNumber(b)
  )
}

.bfdReportHypothesisRegion <- function(settings, under) {
  h1Greater <- TRUE
  if (!isTRUE(settings[["isBinomial"]]))
    h1Greater <- identical(settings[["alternative"]], "greater")

  symbol <- .bfdReportParameterSymbol(settings)
  null   <- .bfdReportNumber(.bfdReportNullValue(settings))

  if (h1Greater && under == "h1")
    return(gettextf("%1$s > %2$s", symbol, null))
  if (h1Greater)
    return(gettextf("%1$s \u2264 %2$s", symbol, null))
  if (under == "h1")
    return(gettextf("%1$s < %2$s", symbol, null))

  return(gettextf("%1$s \u2265 %2$s", symbol, null))
}

.bfdReportThresholdSentence <- function(settings) {
  gettextf(
    "The design used evidence thresholds of BF\u2081\u2080 \u2265 %1$s for H\u2081 and BF\u2080\u2081 \u2265 %2$s for H\u2080 (equivalently, BF\u2081\u2080 \u2264 %3$s).",
    .bfdReportNumber(settings[["bf10Threshold"]]),
    .bfdReportNumber(settings[["bf01Threshold"]]),
    .bfdReportReciprocalText(settings[["bf01Threshold"]])
  )
}

.bfdReportFixedPlanningSentence <- function(settings, result) {
  h1N <- .bfdResultN1ForUnder(result, "h1")
  h0N <- .bfdResultN1ForUnder(result, "h0")

  if (!identical(settings[["calculation"]], "sampleSize"))
    return(gettextf("The design evaluated %1$s.", .bfdSampleSizeText(settings, result[["n1"]])))

  if (identical(h1N, h0N))
    return(gettextf(
      "The design selected %1$s to target conclusive evidence probabilities of %2$s under H\u2081 and %3$s under H\u2080.",
      .bfdSampleSizeText(settings, h1N),
      .bfdReportPercent(.bfdTargetPower(settings, "h1")),
      .bfdReportPercent(.bfdTargetPower(settings, "h0"))
    ))

  return(gettextf(
    "The design selected %1$s for the H\u2081 planning target and %2$s for the H\u2080 planning target, with target conclusive evidence probabilities of %3$s and %4$s, respectively.",
    .bfdSampleSizeText(settings, h1N),
    .bfdSampleSizeText(settings, h0N),
    .bfdReportPercent(.bfdTargetPower(settings, "h1")),
    .bfdReportPercent(.bfdTargetPower(settings, "h0"))
  ))
}

.bfdReportProbabilitySentence <- function(settings, h1Outcome, h0Outcome) {
  gettextf(
    "The achieved probability of conclusive evidence was %1$s under H\u2081 and %2$s under H\u2080; the probability of misleading evidence was %3$s under H\u2081 and %4$s under H\u2080.",
    .bfdReportPercent(h1Outcome[["alternative"]]),
    .bfdReportPercent(h0Outcome[["null"]]),
    .bfdReportPercent(h1Outcome[["null"]]),
    .bfdReportPercent(h0Outcome[["alternative"]])
  )
}

.bfdReportSoftwareSentence <- function() {
  gettextf(
    "The design analysis used JASP with the jaspPower module version %1$s and the bfpwr R package version %2$s.",
    .bfdPackageVersion("jaspPower"),
    .bfdPackageVersion("bfpwr")
  )
}

.bfdPackageVersion <- function(package) {
  version <- try(as.character(utils::packageVersion(package)), silent = TRUE)
  if (jaspBase::isTryError(version) || length(version) == 0)
    return(gettext("unknown"))

  return(version)
}

.bfdReportHtml <- function(paragraph, settings = NULL) {
  if (!is.null(settings) && isTRUE(settings[["reportLatex"]]))
    paragraph <- .bfdReportLatexText(paragraph)

  paste0("<p>", .bfdEscapeHtml(paragraph), "</p>")
}

.bfdReportLatexText <- function(paragraph) {
  replacements <- c(
    "BF\u2081\u2080" = "$\\text{BF}_{10}$",
    "BF\u2080\u2081" = "$\\text{BF}_{01}$",
    "H\u2081" = "$H_1$",
    "H\u2080" = "$H_0$",
    "N\u2081" = "$N_1$",
    "N\u2082" = "$N_2$",
    "n_min,1" = "$n_{\\min,1}$",
    "n_min,2" = "$n_{\\min,2}$",
    "n_max,1" = "$n_{\\max,1}$",
    "n_max,2" = "$n_{\\max,2}$",
    "n_min" = "$n_{\\min}$",
    "n_max" = "$n_{\\max}$",
    "n\u2081" = "$n_1$",
    "n\u2082" = "$n_2$",
    "\u03B4" = "$\\delta$",
    "\u2265" = "$\\geq$",
    "\u2264" = "$\\leq$",
    "\u2260" = "$\\neq$",
    "\u00B1" = "$\\pm$"
  )

  for (i in seq_along(replacements))
    paragraph <- gsub(names(replacements)[i], replacements[[i]], paragraph, fixed = TRUE)

  paragraph <- gsub(" ~ ", " $\\sim$ ", paragraph, fixed = TRUE)
  paragraph <- gsub("p $\\sim$", "$p$ $\\sim$", paragraph, fixed = TRUE)
  paragraph <- gsub("p = ", "$p$ = ", paragraph, fixed = TRUE)
  paragraph <- gsub(" p $\\leq$ ", " $p$ $\\leq$ ", paragraph, fixed = TRUE)
  paragraph <- gsub(" p $\\geq$ ", " $p$ $\\geq$ ", paragraph, fixed = TRUE)
  paragraph <- gsub(" p < ", " $p$ < ", paragraph, fixed = TRUE)
  paragraph <- gsub(" p > ", " $p$ > ", paragraph, fixed = TRUE)
  paragraph <- gsub(" < ", " $<$ ", paragraph, fixed = TRUE)
  paragraph <- gsub(" > ", " $>$ ", paragraph, fixed = TRUE)

  return(paragraph)
}

.bfdReportNumber <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(.bfdFormatNumber(x[1]))
}

.bfdReportPercent <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(paste0(formatC(100 * x[1], format = "f", digits = 1), "%"))
}

.bfdReportReciprocalText <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(gettextf("1/%1$s", .bfdReportNumber(x[1])))
}

.bfdReportMomentNumber <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(formatC(x[1], format = "f", digits = 1))
}

.bfdRCode <- function(jaspResults, settings, result, validation) {
  html <- .bfdCreateHtml(
    parent       = jaspResults,
    key          = "evidenceRCode",
    title        = gettext("R Code"),
    position     = 14,
    dependencies = .bfdRCodeDependencies
  )
  if (is.null(html))
    return()

  if (jaspBase::isTryError(validation)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .bfdCleanError(validation))
    return()
  }

  code <- try(.bfdBfpwrCall(settings, result), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .bfdCleanError(code))
    return()
  }

  html[["text"]] <- .bfdCodeHtml(code)
}

.bfdBfpwrCall <- function(settings, result = NULL) {
  calls <- vapply(.bfdResultTargets(settings), function(target) {
    targetSettings <- .bfdSettingsForTargetCall(settings, target)
    prefix <- if (target == "h1") "# Plan for evidence for H1" else "# Plan for evidence for H0"

    call <- if (targetSettings[["isBinomial"]]) {
      .bfdBinomialBfpwrCall(targetSettings)
    } else if (targetSettings[["isZTest"]]) {
      .bfdZBfpwrCall(targetSettings)
    } else {
      .bfdTBfpwrCall(targetSettings)
    }

    paste(prefix, call, sep = "\n")
  }, character(1))

  return(paste(calls, collapse = "\n\n"))
}

.bfdSettingsForTargetCall <- function(settings, target) {
  targetSettings <- settings
  designPrior <- if (target == "h0") {
    settings[["designPriorUnderH0"]]
  } else {
    settings[["designPriorUnderH1"]]
  }

  targetSettings[["evidenceTarget"]] <- target
  targetSettings[["bfThreshold"]]    <- .bfdThreshold(settings, target)
  targetSettings[["eventK"]]         <- .bfdEventK(settings, target)
  targetSettings[["lowerTail"]]      <- .bfdLowerTail(target)

  if (settings[["isBinomial"]]) {
    targetSettings[["designPriorUnderH1"]] <- designPrior
  } else {
    targetSettings[["designPriorMean"]] <- designPrior[["mean"]]
    targetSettings[["designPriorSd"]]   <- designPrior[["sd"]]
  }

  return(targetSettings)
}

.bfdBinomialBfpwrCall <- function(settings) {
  if (settings[["calculation"]] == "sampleSize") {
    args <- c(
      list(
        k          = settings[["eventK"]],
        power      = .bfdTargetPower(settings, settings[["evidenceTarget"]]),
        p0         = settings[["nullProportion"]],
        type       = settings[["nullPriorDistribution"]],
        a          = settings[["analysisPriorSuccesses"]],
        b          = settings[["analysisPriorFailures"]],
        lower.tail = settings[["lowerTail"]],
        nrange     = c(.bfdMinimumSampleSize(settings), ceiling(settings[["rangeMax"]]))
      ),
      .bfdBinomialDesignArguments(settings)
    )

    return(.bfdFormatRCall("bfpwr::nbinbf01", args))
  }

  k         <- if (settings[["evidenceTarget"]] == "h1") 1 / settings[["bfThreshold"]] else settings[["bfThreshold"]]
  lowerTail <- settings[["evidenceTarget"]] == "h1"
  args <- c(
    list(
      k          = k,
      n          = settings[["sampleSize"]],
      p0         = settings[["nullProportion"]],
      type       = settings[["nullPriorDistribution"]],
      a          = settings[["analysisPriorSuccesses"]],
      b          = settings[["analysisPriorFailures"]],
      lower.tail = lowerTail
    ),
    .bfdBinomialDesignArguments(settings)
  )

  return(.bfdFormatRCall("bfpwr::pbinbf01", args))
}

.bfdZBfpwrCall <- function(settings) {
  normalPrior <- settings[["analysisPriorDistribution"]] %in% c("point", "normal")

  if (settings[["calculation"]] == "sampleSize") {
    n <- .bfdMinimumSampleSize(settings)
    if (normalPrior) {
      args <- list(
        k          = settings[["eventK"]],
        power      = .bfdTargetPower(settings, settings[["evidenceTarget"]]),
        usd        = .bfdZUnitStandardDeviation(settings, n),
        null       = settings[["nullValue"]],
        pm         = settings[["analysisPriorMean"]],
        psd        = settings[["analysisPriorSd"]],
        dpm        = settings[["designPriorMean"]],
        dpsd       = settings[["designPriorSd"]],
        nrange     = c(n, ceiling(settings[["rangeMax"]])),
        lower.tail = settings[["lowerTail"]]
      )

      return(.bfdFormatRCall("bfpwr::nbf01", args))
    }

    args <- list(
      k          = settings[["eventK"]],
      power      = .bfdTargetPower(settings, settings[["evidenceTarget"]]),
      usd        = .bfdZUnitStandardDeviation(settings, n),
      null       = settings[["nullValue"]],
      psd        = settings[["momentPriorSpread"]],
      dpm        = settings[["designPriorMean"]],
      dpsd       = settings[["designPriorSd"]],
      nrange     = c(n, ceiling(settings[["rangeMax"]])),
      lower.tail = settings[["lowerTail"]]
    )

    return(.bfdFormatRCall("bfpwr::nnmbf01", args))
  }

  k         <- if (settings[["evidenceTarget"]] == "h1") 1 / settings[["bfThreshold"]] else settings[["bfThreshold"]]
  lowerTail <- settings[["evidenceTarget"]] == "h1"
  n         <- settings[["sampleSize"]]

  if (normalPrior) {
    args <- list(
      k          = k,
      n          = n,
      usd        = .bfdZUnitStandardDeviation(settings, n),
      null       = settings[["nullValue"]],
      pm         = settings[["analysisPriorMean"]],
      psd        = settings[["analysisPriorSd"]],
      dpm        = settings[["designPriorMean"]],
      dpsd       = settings[["designPriorSd"]],
      lower.tail = lowerTail
    )

    return(.bfdFormatRCall("bfpwr::pbf01", args))
  }

  args <- list(
    k          = k,
    n          = n,
    usd        = .bfdZUnitStandardDeviation(settings, n),
    null       = settings[["nullValue"]],
    psd        = settings[["momentPriorSpread"]],
    dpm        = settings[["designPriorMean"]],
    dpsd       = settings[["designPriorSd"]],
    lower.tail = lowerTail
  )

  return(.bfdFormatRCall("bfpwr::pnmbf01", args))
}

.bfdTBfpwrCall <- function(settings) {
  if (settings[["calculation"]] == "sampleSize") {
    if (settings[["isIndependentSamples"]] && settings[["sampleSizeRatio"]] != 1)
      stop(gettext("R code generation for t-test sample-size search with unequal group sizes is not available."))

    if (settings[["drangeMode"]] == "custom")
      stop(gettext("R code generation for t-test sample-size search with a custom t search range is not available."))

    if (!isTRUE(all.equal(settings[["nullValue"]], 0)))
      stop(gettext("R code generation for t-test sample-size search with a nonzero null value is not available."))

    args <- list(
      k           = settings[["eventK"]],
      power       = .bfdTargetPower(settings, settings[["evidenceTarget"]]),
      null        = settings[["nullValue"]],
      plocation   = settings[["tPriorLocationRelative"]],
      pscale      = settings[["tPriorScale"]],
      pdf         = settings[["tPriorDf"]],
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      dpm         = settings[["designPriorMean"]],
      dpsd        = settings[["designPriorSd"]],
      lower.tail  = settings[["lowerTail"]],
      nrange      = c(.bfdMinimumSampleSize(settings), ceiling(settings[["rangeMax"]]))
    )

    return(.bfdFormatRCall("bfpwr::ntbf01", args))
  }

  n1        <- settings[["sampleSize"]]
  n2        <- .bfdSampleSizeSecondGroup(settings, n1)
  k         <- if (settings[["evidenceTarget"]] == "h1") 1 / settings[["bfThreshold"]] else settings[["bfThreshold"]]
  lowerTail <- settings[["evidenceTarget"]] == "h1"
  args <- list(
    k           = k,
    n           = n1,
    n1          = n1,
    n2          = n2,
    null        = settings[["nullValue"]],
    plocation   = settings[["tPriorLocationRelative"]],
    pscale      = settings[["tPriorScale"]],
    pdf         = settings[["tPriorDf"]],
    dpm         = settings[["designPriorMean"]],
    dpsd        = settings[["designPriorSd"]],
    type        = settings[["testType"]],
    alternative = settings[["alternative"]],
    lower.tail  = lowerTail,
    drange      = .bfdTSearchRange(settings, n1, k)
  )

  return(.bfdFormatRCall("bfpwr::ptbf01", args))
}

.bfdSampleSizePlot <- function(jaspResults, settings, result) {
  for (spec in .bfdUnderPlotSpecs(settings, "evidenceBySampleSize", gettext("Decision Probabilities by Sample Size"), 9)) {
    .bfdSampleSizeOutcomePlot(
      jaspResults = jaspResults,
      settings    = settings,
      result      = result,
      key         = spec[["key"]],
      title       = spec[["title"]],
      position    = spec[["position"]],
      under       = spec[["under"]]
    )
  }
}

.bfdSampleSizeOutcomePlot <- function(jaspResults, settings, result, key, title, position, under) {
  plot <- .bfdCreatePlot(
    parent       = jaspResults,
    key          = key,
    title        = title,
    position     = position,
    dependencies = .bfdSampleSizePlotDependencies,
    width        = .bfdPlotWidth(settings),
    height       = 350
  )
  if (is.null(plot))
    return()

  if (jaspBase::isTryError(result)) {
    .bfdSetOutcomePlotError(plot, result)
    return()
  }

  plotData <- .bfdCachedPlotData(
    jaspResults  = jaspResults,
    stateKey     = "evidenceBySampleSizePlotData",
    dependencies = .bfdSampleSizePlotDataDependencies,
    dataKey      = "data",
    compute      = function() .bfdSampleSizePlotData(settings, result)
  )
  if (jaspBase::isTryError(plotData)) {
    .bfdSetOutcomePlotError(plot, plotData)
    return()
  }

  plotResult <- try(.bfdBuildSampleSizePlot(settings, result, under, plotData), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    .bfdSetOutcomePlotError(plot, plotResult)
    return()
  }

  plot$plotObject <- plotResult
}

.bfdUnderPlotSpecs <- function(settings, keyPrefix, title, position) {
  if (isTRUE(settings[["mergeH1H0Figures"]])) {
    return(list(list(
      key      = paste0(keyPrefix, "Plot"),
      title    = title,
      position = position,
      under    = NULL
    )))
  }

  unders <- .bfdCurveUnders(settings)
  specs <- lapply(seq_along(unders), function(i) {
    under <- unders[i]
    list(
      key      = paste0(keyPrefix, .bfdUnderKeySuffix(under), "Plot"),
      title    = .bfdPlotTitleUnder(title, under),
      position = position + i - 1,
      under    = under
    )
  })

  return(specs)
}

.bfdUnderKeySuffix <- function(under) {
  if (under == "h1")
    return("UnderH1")

  return("UnderH0")
}

.bfdPlotTitleUnder <- function(title, under) {
  gettextf("%1$s: %2$s", title, .bfdDesignPriorUnderLabel(under))
}

.bfdSetOutcomePlotError <- function(plot, error) {
  plot$setError(gettextf("Unable to compute conclusive evidence and misleading evidence curves: %1$s", .bfdCleanError(error)))
}

.bfdSampleSizePlotData <- function(settings, result) {
  minimumN <- .bfdMinimumSampleSize(settings)
  maximumN <- max(result[["n1"]] * 2, settings[["sampleSize"]] * 2, minimumN + 20, 50)

  if (settings[["calculation"]] == "sampleSize")
    maximumN <- min(settings[["rangeMax"]], maximumN)

  nValues <- if (isTRUE(settings[["logSampleSize"]])) {
    unique(ceiling(exp(seq(log(minimumN), log(maximumN), length.out = settings[["plotPoints"]]))))
  } else {
    unique(ceiling(seq(minimumN, maximumN, length.out = settings[["plotPoints"]])))
  }

  return(list(
    data   = .bfdCurveBySampleSize(settings, nValues, .bfdCurveUnders(settings)),
    xLabel = if (settings[["isIndependentSamples"]]) gettext("Sample size (group 1)") else gettext("Sample size")
  ))
}

.bfdBuildSampleSizePlot <- function(settings, result, under, plotData = NULL) {
  if (is.null(plotData))
    plotData <- .bfdSampleSizePlotData(settings, result)

  unders <- .bfdPlotUnders(settings, under)
  data   <- .bfdFilterCurveData(plotData[["data"]], under)
  xLabel <- plotData[["xLabel"]]

  xScale <- if (isTRUE(settings[["logSampleSize"]])) {
    ggplot2::scale_x_log10()
  } else {
    ggplot2::scale_x_continuous()
  }

  showUnder <- length(unique(data[["under"]])) > 1
  if (showUnder) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome, linetype = under)) +
      ggplot2::geom_line(linewidth = 1.2) +
      xScale +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Outcome"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.2) +
      xScale +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Outcome"))
  }

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      .bfdTargetPowerLine(settings, unders)
  }

  linetypeValues <- if (showUnder) .bfdUnderLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdEffectSizePlot <- function(jaspResults, settings, result) {
  for (spec in .bfdUnderPlotSpecs(settings, "evidenceByEffectSize", .bfdEffectSizePlotTitle(settings), 5)) {
    .bfdEffectSizeOutcomePlot(
      jaspResults = jaspResults,
      settings    = settings,
      result      = result,
      key         = spec[["key"]],
      title       = spec[["title"]],
      position    = spec[["position"]],
      under       = spec[["under"]]
    )
  }
}

.bfdEffectSizeOutcomePlot <- function(jaspResults, settings, result, key, title, position, under) {
  plot <- .bfdCreatePlot(
    parent       = jaspResults,
    key          = key,
    title        = title,
    position     = position,
    dependencies = .bfdEffectSizePlotDependencies,
    width        = .bfdPlotWidth(settings),
    height       = 350
  )
  if (is.null(plot))
    return()

  if (jaspBase::isTryError(result)) {
    .bfdSetOutcomePlotError(plot, result)
    return()
  }

  plotData <- .bfdCachedPlotData(
    jaspResults  = jaspResults,
    stateKey     = "evidenceByEffectSizePlotData",
    dependencies = .bfdEffectSizePlotDataDependencies,
    dataKey      = "data",
    compute      = function() .bfdEffectSizePlotData(settings, result)
  )
  if (jaspBase::isTryError(plotData)) {
    .bfdSetOutcomePlotError(plot, plotData)
    return()
  }

  plotResult <- try(.bfdBuildEffectSizePlot(settings, result, under, plotData), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    .bfdSetOutcomePlotError(plot, plotResult)
    return()
  }

  plot$plotObject <- plotResult
}

.bfdEffectSizePlotTitle <- function(settings) {
  if (settings[["isBinomial"]])
    return(gettext("Decision Probabilities by Proportion"))

  return(gettext("Decision Probabilities by Effect Size"))
}

.bfdEffectSizePlotData <- function(settings, result) {
  if (settings[["isBinomial"]])
    return(.bfdBinomialProportionPlotData(settings, result))

  effectRange <- .bfdEffectRange(settings)
  effect      <- seq(effectRange[1], effectRange[2], length.out = settings[["plotPoints"]])

  return(list(
    data   = .bfdCurveByEffectSize(settings, effect, .bfdCurveUnders(settings), function(under) .bfdResultN1ForBasis(settings, result, under)),
    xLabel = .bfdDesignPriorEffectAxisLabel(settings)
  ))
}

.bfdBuildEffectSizePlot <- function(settings, result, under, plotData = NULL) {
  if (settings[["isBinomial"]])
    return(.bfdBuildBinomialProportionPlot(settings, result, under, plotData))

  if (is.null(plotData))
    plotData <- .bfdEffectSizePlotData(settings, result)

  unders <- .bfdPlotUnders(settings, under)
  data   <- .bfdFilterCurveData(plotData[["data"]], under)
  xLabel <- plotData[["xLabel"]]

  showUnder <- length(unique(data[["under"]])) > 1
  if (showUnder) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = effect, y = probability, color = outcome, linetype = under)) +
      ggplot2::geom_line(linewidth = 1.2) +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Outcome"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = effect, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.2) +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Outcome"))
  }

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      .bfdTargetPowerLine(settings, unders)
  }

  linetypeValues <- if (showUnder) .bfdUnderLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdDesignPriorEffectAxisLabel <- function(settings) {
  designPrior <- .bfdContinuousDesignPriorForUnder(settings, "h1")
  if (identical(designPrior[["distribution"]], "point"))
    return(gettext("Design prior location"))

  return(gettext("Design prior mean"))
}

.bfdBinomialProportionPlotData <- function(settings, result) {
  proportionAxis <- .bfdBinomialProportionAxis(settings)
  xRange         <- c(max(proportionAxis[["range"]][1], .Machine$double.eps), min(proportionAxis[["range"]][2], 1 - .Machine$double.eps))
  proportion     <- seq(xRange[1], xRange[2], length.out = settings[["plotPoints"]])

  return(list(
    data           = .bfdCurveByProportion(settings, proportion, .bfdCurveUnders(settings), function(under) .bfdResultN1ForBasis(settings, result, under)),
    proportionAxis = proportionAxis
  ))
}

.bfdBuildBinomialProportionPlot <- function(settings, result, under, plotData = NULL) {
  if (is.null(plotData))
    plotData <- .bfdBinomialProportionPlotData(settings, result)

  unders         <- .bfdPlotUnders(settings, under)
  data           <- .bfdFilterCurveData(plotData[["data"]], under)
  proportionAxis <- plotData[["proportionAxis"]]

  showUnder <- length(unique(data[["under"]])) > 1
  if (showUnder) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = proportion, y = probability, color = outcome, linetype = under)) +
      ggplot2::geom_line(linewidth = 1.2) +
      .bfdProbabilityYScale() +
      jaspGraphs::scale_x_continuous(
        name   = gettext("Design proportion"),
        breaks = proportionAxis[["breaks"]],
        limits = proportionAxis[["range"]]
      ) +
      ggplot2::labs(y = gettext("Probability"), color = gettext("Outcome"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = proportion, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.2) +
      .bfdProbabilityYScale() +
      jaspGraphs::scale_x_continuous(
        name   = gettext("Design proportion"),
        breaks = proportionAxis[["breaks"]],
        limits = proportionAxis[["range"]]
      ) +
      ggplot2::labs(y = gettext("Probability"), color = gettext("Outcome"))
  }

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      .bfdTargetPowerLine(settings, unders)
  }

  linetypeValues <- if (showUnder) .bfdUnderLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdPriorPlot <- function(jaspResults, settings, validation) {
  .bfdPriorPlotContainer(
    jaspResults  = jaspResults,
    settings     = settings,
    validation   = validation,
    key          = "evidencePriorPlot",
    position     = 13,
    dependencies = .bfdPriorPlotDependencies,
    stateKey     = "evidencePriorPlotData",
    dataDependencies = .bfdPriorPlotDataDependencies
  )
}

.bfdPriorPlotContainer <- function(jaspResults, settings, validation = NULL, key, position,
                                  dependencies, stateKey, dataDependencies) {
  if (!is.null(jaspResults[[key]]))
    return()

  container <- createJaspContainer(title = gettext("Prior Distribution"))
  container$dependOn(dependencies)
  container$position <- position
  jaspResults[[key]] <- container

  specs <- .bfdPriorPlotSpecs(settings)
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    plot <- createJaspPlot(title = spec[["title"]], width = .bfdPlotWidth(settings), height = 350)
    plot$position <- i
    container[[spec[["key"]]]] <- plot

    if (!is.null(validation) && jaspBase::isTryError(validation)) {
      plot$setError(gettextf("Unable to compute prior distribution plot: %1$s", .bfdCleanError(validation)))
      next
    }

    plotData <- .bfdCachedPlotData(
      jaspResults  = jaspResults,
      stateKey     = stateKey,
      dependencies = dataDependencies,
      dataKey      = .bfdPriorSetKey(spec[["priorSet"]]),
      compute      = function() .bfdPriorPlotData(settings, spec[["priorSet"]])
    )
    if (jaspBase::isTryError(plotData)) {
      plot$setError(gettextf("Unable to compute prior distribution plot: %1$s", .bfdCleanError(plotData)))
      next
    }

    plotResult <- try(.bfdBuildPriorPlot(settings, spec[["priorSet"]], plotData), silent = TRUE)
    if (jaspBase::isTryError(plotResult)) {
      plot$setError(gettextf("Unable to compute prior distribution plot: %1$s", .bfdCleanError(plotResult)))
      next
    }

    plot$plotObject <- plotResult
  }
}

.bfdPriorPlotSpecs <- function(settings) {
  priorSets <- .bfdSelectedPriorSets(settings)
  if (length(priorSets) == 0)
    return(list())

  if (isTRUE(settings[["priorPlotMerge"]])) {
    title <- if (length(priorSets) == 2) {
      gettext("Design and Analysis Priors")
    } else if (identical(priorSets, "analysis")) {
      gettext("Analysis Prior")
    } else {
      gettext("Design Prior")
    }
    return(list(list(
      key      = "merged",
      title    = title,
      priorSet = priorSets
    )))
  }

  specs <- list()
  if ("design" %in% priorSets) {
    specs[[length(specs) + 1]] <- list(
      key      = "design",
      title    = gettext("Design Prior"),
      priorSet = "design"
    )
  }
  if ("analysis" %in% priorSets) {
    specs[[length(specs) + 1]] <- list(
      key      = "analysis",
      title    = gettext("Analysis Prior"),
      priorSet = "analysis"
    )
  }

  return(specs)
}

.bfdSelectedPriorSets <- function(settings) {
  priorSets <- character(0)
  if (isTRUE(settings[["priorPlotDesign"]]))
    priorSets <- c(priorSets, "design")
  if (isTRUE(settings[["priorPlotAnalysis"]]))
    priorSets <- c(priorSets, "analysis")

  return(priorSets)
}

.bfdPriorSetIncludes <- function(priorSet, value) {
  value %in% priorSet
}

.bfdPriorSetKey <- function(priorSet) {
  paste(sort(priorSet), collapse = "_")
}

.bfdPriorLegendTitle <- function(priorSet) {
  if (identical(priorSet, "analysis"))
    return(gettext("Analysis Prior"))

  if (identical(priorSet, "design"))
    return(gettext("Design Prior"))

  return(gettext("Prior"))
}

.bfdPriorRelabelData <- function(data, priorSet) {
  if (nrow(data) == 0 || length(priorSet) != 1)
    return(data)

  data[["prior"]] <- .bfdPriorLegendLabels(data[["prior"]], priorSet)
  return(data)
}

.bfdPriorAestheticData <- function(data) {
  if (!"prior" %in% names(data)) {
    data[["hypothesis"]] <- character(nrow(data))
    data[["priorType"]]  <- character(nrow(data))
    return(data)
  }

  data[["hypothesis"]] <- vapply(data[["prior"]], .bfdPriorHypothesisAestheticLabel, character(1))
  data[["priorType"]]  <- vapply(data[["prior"]], .bfdPriorTypeAestheticLabel, character(1))

  return(data)
}

.bfdPriorHypothesisAestheticLabel <- function(priorLabel) {
  if (priorLabel %in% c(.bfdAnalysisPriorPlotLabel("h1"), .bfdDesignPriorPlotLabel("h1"), .bfdUnderLabel("h1")))
    return(gettext("H\u2081"))

  if (priorLabel %in% c(.bfdAnalysisPriorPlotLabel("h0"), .bfdDesignPriorPlotLabel("h0"), .bfdUnderLabel("h0")))
    return(gettext("H\u2080"))

  return(priorLabel)
}

.bfdPriorTypeAestheticLabel <- function(priorLabel) {
  if (priorLabel %in% c(.bfdAnalysisPriorPlotLabel("h1"), .bfdAnalysisPriorPlotLabel("h0")))
    return(gettext("Analysis Prior"))

  if (priorLabel %in% c(.bfdDesignPriorPlotLabel("h1"), .bfdDesignPriorPlotLabel("h0")))
    return(gettext("Design Prior"))

  return(priorLabel)
}

.bfdPriorUsesMergedAesthetics <- function(priorSet) {
  .bfdPriorSetIncludes(priorSet, "analysis") && .bfdPriorSetIncludes(priorSet, "design")
}

.bfdPriorLegendLabels <- function(labels, priorSet) {
  if (identical(priorSet, "analysis")) {
    labels[labels == .bfdAnalysisPriorPlotLabel("h0")] <- .bfdUnderLabel("h0")
    labels[labels == .bfdAnalysisPriorPlotLabel("h1")] <- .bfdUnderLabel("h1")
  }

  if (identical(priorSet, "design")) {
    labels[labels == .bfdDesignPriorPlotLabel("h0")] <- .bfdUnderLabel("h0")
    labels[labels == .bfdDesignPriorPlotLabel("h1")] <- .bfdUnderLabel("h1")
  }

  return(labels)
}

.bfdPriorPlotData <- function(settings, priorSet = c("analysis", "design")) {
  if (settings[["isBinomial"]])
    return(.bfdBinomialPriorPlotData(settings, priorSet))

  return(.bfdContinuousPriorPlotData(settings, priorSet))
}

.bfdBuildPriorPlot <- function(settings, priorSet = c("analysis", "design"), plotData = NULL) {
  if (settings[["isBinomial"]])
    return(.bfdBuildBinomialPriorPlot(settings, priorSet, plotData))

  return(.bfdBuildContinuousPriorPlot(settings, priorSet, plotData))
}

.bfdCurveUnders <- function(settings) {
  return(c("h1", "h0"))
}

.bfdCurveOutcomes <- function() {
  return(c("power", "misleading"))
}

.bfdPlotUnders <- function(settings, under = NULL) {
  if (!is.null(under))
    return(under)

  return(.bfdCurveUnders(settings))
}

.bfdFilterCurveData <- function(data, under = NULL) {
  if (is.null(under))
    return(data)

  data[data[["under"]] == .bfdUnderLabel(under), , drop = FALSE]
}

.bfdOutcomeLabel <- function(outcome) {
  if (outcome == "power")
    return(gettext("Conclusive"))

  return(gettext("Misleading"))
}

.bfdCurveLabel <- function(outcome, under, nUnders) {
  outcomeLabel <- .bfdOutcomeLabel(outcome)
  if (nUnders == 1)
    return(outcomeLabel)

  return(gettextf("%1$s: %2$s", outcomeLabel, .bfdUnderLabel(under)))
}

.bfdCurveBySampleSize <- function(settings, nValues, unders) {
  rows <- lapply(unders, function(under) {
    lapply(.bfdCurveOutcomes(), function(outcome) {
      target <- .bfdCurveTarget(under, outcome)
      data.frame(
        n           = nValues,
        probability = .bfdEvidenceProbability(settings, n1 = nValues, target = target, under = under),
        under       = .bfdUnderLabel(under),
        outcome     = .bfdOutcomeLabel(outcome),
        curve       = .bfdCurveLabel(outcome, under, length(unders)),
        stringsAsFactors = FALSE
      )
    })
  })

  return(do.call(rbind, unlist(rows, recursive = FALSE)))
}

.bfdCurveByEffectSize <- function(settings, effect, unders, n1) {
  rows <- lapply(unders, function(under) {
    n1Under <- .bfdCurveN1ForUnder(n1, under)
    lapply(.bfdCurveOutcomes(), function(outcome) {
      target          <- .bfdCurveTarget(under, outcome)
      designPriorMean <- effect
      data.frame(
        effect      = effect,
        probability = .bfdEvidenceProbability(settings, n1 = n1Under, target = target, under = under, designPriorMean = designPriorMean),
        under       = .bfdUnderLabel(under),
        outcome     = .bfdOutcomeLabel(outcome),
        curve       = .bfdCurveLabel(outcome, under, length(unders)),
        stringsAsFactors = FALSE
      )
    })
  })

  return(do.call(rbind, unlist(rows, recursive = FALSE)))
}

.bfdCurveByProportion <- function(settings, proportion, unders, n1) {
  rows <- lapply(unders, function(under) {
    n1Under <- .bfdCurveN1ForUnder(n1, under)
    lapply(.bfdCurveOutcomes(), function(outcome) {
      target          <- .bfdCurveTarget(under, outcome)
      designPriorMean <- proportion
      data.frame(
        proportion  = proportion,
        probability = .bfdEvidenceProbability(settings, n1 = n1Under, target = target, under = under, designPriorMean = designPriorMean),
        under       = .bfdUnderLabel(under),
        outcome     = .bfdOutcomeLabel(outcome),
        curve       = .bfdCurveLabel(outcome, under, length(unders)),
        stringsAsFactors = FALSE
      )
    })
  })

  return(do.call(rbind, unlist(rows, recursive = FALSE)))
}

.bfdCurveN1ForUnder <- function(n1, under) {
  if (is.function(n1))
    return(n1(under))

  return(n1)
}

.bfdCurveTarget <- function(under, outcome) {
  if (outcome == "power")
    return(under)

  return(.bfdOppositeTarget(under))
}

.bfdOppositeTarget <- function(target) {
  if (target == "h1")
    return("h0")

  return("h1")
}

.bfdContinuousPriorPlotData <- function(settings, priorSet) {
  priorAxis   <- .bfdPriorAxis(settings, priorSet)
  x           <- seq(priorAxis[["range"]][1], priorAxis[["range"]][2], length.out = settings[["plotPoints"]])
  densityData <- .bfdContinuousPriorDensityData(settings, x, priorSet)
  spikeData   <- .bfdContinuousPriorSpikeData(settings, priorSet, densityData)
  yAxis       <- .bfdPriorYAxis(densityData, spikeData)
  legendTitle <- .bfdPriorLegendTitle(priorSet)

  densityData <- .bfdPriorRelabelData(densityData, priorSet)
  spikeData   <- .bfdPriorRelabelData(spikeData, priorSet)
  densityData <- .bfdPriorAestheticData(densityData)
  spikeData   <- .bfdPriorAestheticData(spikeData)

  return(list(
    priorAxis   = priorAxis,
    densityData = densityData,
    spikeData   = spikeData,
    yAxis       = yAxis,
    legendTitle = legendTitle
  ))
}

.bfdBuildContinuousPriorPlot <- function(settings, priorSet, plotData = NULL) {
  if (is.null(plotData))
    plotData <- .bfdContinuousPriorPlotData(settings, priorSet)

  priorAxis   <- plotData[["priorAxis"]]
  densityData <- plotData[["densityData"]]
  spikeData   <- plotData[["spikeData"]]
  yAxis       <- plotData[["yAxis"]]
  legendTitle <- plotData[["legendTitle"]]
  mergedAesthetics <- .bfdPriorUsesMergedAesthetics(priorSet)

  plot <- ggplot2::ggplot() +
    jaspGraphs::scale_x_continuous(
      name   = gettext("Parameter value"),
      breaks = priorAxis[["breaks"]],
      limits = priorAxis[["range"]]
    )

  if (nrow(densityData) > 0) {
    plot <- if (mergedAesthetics) {
      plot + ggplot2::geom_line(
        data = densityData,
        ggplot2::aes(x = x, y = density, color = hypothesis, linetype = priorType),
        linewidth = 1.1
      )
    } else {
      plot + ggplot2::geom_line(
        data = densityData,
        ggplot2::aes(x = x, y = density, color = prior),
        linewidth = 1.1
      )
    }
  }

  if (nrow(spikeData) > 0) {
    plot <- if (mergedAesthetics) {
      plot + ggplot2::geom_segment(
        data = spikeData,
        ggplot2::aes(x = x, xend = x, y = 0, yend = height, color = hypothesis, linetype = priorType),
        arrow       = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
        linewidth   = 1.1,
        inherit.aes = FALSE
      )
    } else {
      plot + ggplot2::geom_segment(
        data = spikeData,
        ggplot2::aes(x = x, xend = x, y = 0, yend = height, color = prior),
        arrow       = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
        linewidth   = 1.1,
        inherit.aes = FALSE
      )
    }
  }

  plot <- plot +
    ggplot2::scale_y_continuous(
      breaks = yAxis[["breaks"]],
      labels = yAxis[["labels"]],
      limits = yAxis[["range"]],
      expand = .bfdPriorYAxisExpansion()
    ) +
    ggplot2::labs(
      y        = gettext("Density"),
      color    = if (mergedAesthetics) gettext("Hypothesis") else legendTitle,
      linetype = if (mergedAesthetics) gettext("Prior") else NULL
    )

  linetypeValues <- if (mergedAesthetics) .bfdPriorTypeLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdContinuousPriorDensityData <- function(settings, x, priorSet) {
  rows <- list()

  if (.bfdPriorSetIncludes(priorSet, "analysis"))
    rows <- c(rows, .bfdContinuousAnalysisPriorDensityRows(settings, x))

  if (.bfdPriorSetIncludes(priorSet, "design"))
    rows <- c(rows, .bfdContinuousDesignPriorDensityRows(settings, x))

  if (length(rows) == 0) {
    return(data.frame(
      x       = numeric(0),
      density = numeric(0),
      prior   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  return(do.call(rbind, rows))
}

.bfdContinuousAnalysisPriorDensityRows <- function(settings, x) {
  if (settings[["isZTest"]])
    return(.bfdZAnalysisPriorDensityRows(settings, x))

  return(.bfdTAnalysisPriorDensityRows(settings, x))
}

.bfdZAnalysisPriorDensityRows <- function(settings, x) {
  if (settings[["analysisPriorDistribution"]] == "point")
    return(list())

  if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    if (.bfdUsesDirectionalZTest(settings))
      return(.bfdDirectionalNormalPriorDensityRows(settings, x))

    return(list(.bfdPriorDensityRows(
      x       = x,
      density = stats::dnorm(x, mean = settings[["analysisPriorMean"]], sd = settings[["analysisPriorSd"]]),
      prior   = .bfdAnalysisPriorPlotLabel("h1")
    )))
  }

  density <- ((x - settings[["nullValue"]])^2 / settings[["momentPriorSpread"]]^2) *
    stats::dnorm(x, mean = settings[["nullValue"]], sd = settings[["momentPriorSpread"]])

  return(list(.bfdPriorDensityRows(
    x       = x,
    density = density,
    prior   = .bfdAnalysisPriorPlotLabel("h1")
  )))
}

.bfdDirectionalNormalPriorDensityRows <- function(settings, x) {
  rows <- list()
  h1   <- .bfdDirectionalNormalPriorDensity(settings, x, "h1")
  h0   <- .bfdDirectionalNormalPriorDensity(settings, x, "h0")

  rows[[length(rows) + 1]] <- .bfdPriorDensityRows(
    x            = x,
    density      = h1[["density"]],
    prior        = .bfdAnalysisPriorPlotLabel("h1"),
    lower        = h1[["lower"]],
    upper        = h1[["upper"]],
    lowerDensity = h1[["lowerDensity"]],
    upperDensity = h1[["upperDensity"]]
  )
  rows[[length(rows) + 1]] <- .bfdPriorDensityRows(
    x            = x,
    density      = h0[["density"]],
    prior        = .bfdAnalysisPriorPlotLabel("h0"),
    lower        = h0[["lower"]],
    upper        = h0[["upper"]],
    lowerDensity = h0[["lowerDensity"]],
    upperDensity = h0[["upperDensity"]]
  )

  return(rows)
}

.bfdDirectionalNormalPriorDensity <- function(settings, x, under) {
  nullValue <- settings[["nullValue"]]
  mean      <- settings[["analysisPriorMean"]]
  sd        <- settings[["analysisPriorSd"]]
  density   <- stats::dnorm(x, mean = mean, sd = sd)
  atNull    <- stats::dnorm(nullValue, mean = mean, sd = sd)
  lower     <- -Inf
  upper     <- Inf

  h1Greater <- settings[["alternative"]] == "greater"
  if (under == "h0")
    h1Greater <- !h1Greater

  if (h1Greater) {
    normalizer <- stats::pnorm(nullValue, mean = mean, sd = sd, lower.tail = FALSE)
    lower      <- nullValue
    density    <- density / normalizer
    atNull     <- atNull / normalizer
    return(list(density = density, lower = lower, upper = upper, lowerDensity = atNull, upperDensity = NULL))
  }

  normalizer <- stats::pnorm(nullValue, mean = mean, sd = sd)
  upper      <- nullValue
  density    <- density / normalizer
  atNull     <- atNull / normalizer

  return(list(density = density, lower = lower, upper = upper, lowerDensity = NULL, upperDensity = atNull))
}

.bfdTAnalysisPriorDensityRows <- function(settings, x) {
  rawDensity <- stats::dt((x - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]]) / settings[["tPriorScale"]]
  nullValue  <- settings[["nullValue"]]

  if (settings[["alternative"]] == "two.sided") {
    return(list(.bfdPriorDensityRows(
      x       = x,
      density = rawDensity,
      prior   = .bfdAnalysisPriorPlotLabel("h1")
    )))
  }

  if (settings[["alternative"]] == "greater") {
    normalizer <- 1 - stats::pt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]])
    density    <- rawDensity / normalizer
    atNull     <- stats::dt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]]) / settings[["tPriorScale"]] / normalizer
    return(list(.bfdPriorDensityRows(
      x            = x,
      density      = density,
      prior        = .bfdAnalysisPriorPlotLabel("h1"),
      lower        = nullValue,
      lowerDensity = atNull
    )))
  } else {
    normalizer <- stats::pt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]])
    density    <- rawDensity / normalizer
    atNull     <- stats::dt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]]) / settings[["tPriorScale"]] / normalizer
    return(list(.bfdPriorDensityRows(
      x            = x,
      density      = density,
      prior        = .bfdAnalysisPriorPlotLabel("h1"),
      upper        = nullValue,
      upperDensity = atNull
    )))
  }
}

.bfdContinuousDesignPriorDensityRows <- function(settings, x) {
  rows <- list()
  for (under in c("h0", "h1")) {
    designPrior <- .bfdContinuousDesignPriorForUnder(settings, under)
    if (designPrior[["distribution"]] == "normal") {
      rows[[length(rows) + 1]] <- .bfdPriorDensityRows(
        x       = x,
        density = stats::dnorm(x, mean = designPrior[["mean"]], sd = designPrior[["sd"]]),
        prior   = .bfdDesignPriorPlotLabel(under)
      )
    }
  }

  return(rows)
}

.bfdBinomialPriorPlotData <- function(settings, priorSet) {
  priorAxis   <- .bfdPriorAxis(settings, priorSet)
  xRange      <- c(max(priorAxis[["range"]][1], .Machine$double.eps), min(priorAxis[["range"]][2], 1 - .Machine$double.eps))
  x           <- seq(xRange[1], xRange[2], length.out = settings[["plotPoints"]])
  densityData <- .bfdBinomialPriorDensityData(settings, x, priorSet)
  spikeData   <- .bfdBinomialPriorSpikeData(settings, priorSet, densityData)
  yAxis       <- .bfdPriorYAxis(densityData, spikeData)
  legendTitle <- .bfdPriorLegendTitle(priorSet)

  densityData <- .bfdPriorRelabelData(densityData, priorSet)
  spikeData   <- .bfdPriorRelabelData(spikeData, priorSet)
  densityData <- .bfdPriorAestheticData(densityData)
  spikeData   <- .bfdPriorAestheticData(spikeData)

  return(list(
    priorAxis   = priorAxis,
    densityData = densityData,
    spikeData   = spikeData,
    yAxis       = yAxis,
    legendTitle = legendTitle
  ))
}

.bfdBuildBinomialPriorPlot <- function(settings, priorSet, plotData = NULL) {
  if (is.null(plotData))
    plotData <- .bfdBinomialPriorPlotData(settings, priorSet)

  priorAxis   <- plotData[["priorAxis"]]
  densityData <- plotData[["densityData"]]
  spikeData   <- plotData[["spikeData"]]
  yAxis       <- plotData[["yAxis"]]
  legendTitle <- plotData[["legendTitle"]]
  mergedAesthetics <- .bfdPriorUsesMergedAesthetics(priorSet)

  plot <- ggplot2::ggplot() +
    jaspGraphs::scale_x_continuous(
      name   = gettext("Proportion"),
      breaks = priorAxis[["breaks"]],
      limits = priorAxis[["range"]]
    )

  if (nrow(densityData) > 0) {
    plot <- if (mergedAesthetics) {
      plot + ggplot2::geom_line(
        data = densityData,
        ggplot2::aes(x = x, y = density, color = hypothesis, linetype = priorType),
        linewidth = 1.1
      )
    } else {
      plot + ggplot2::geom_line(
        data = densityData,
        ggplot2::aes(x = x, y = density, color = prior),
        linewidth = 1.1
      )
    }
  }

  if (nrow(spikeData) > 0) {
    plot <- if (mergedAesthetics) {
      plot + ggplot2::geom_segment(
        data = spikeData,
        ggplot2::aes(x = x, xend = x, y = 0, yend = height, color = hypothesis, linetype = priorType),
        arrow       = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
        linewidth   = 1.1,
        inherit.aes = FALSE
      )
    } else {
      plot + ggplot2::geom_segment(
        data = spikeData,
        ggplot2::aes(x = x, xend = x, y = 0, yend = height, color = prior),
        arrow       = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
        linewidth   = 1.1,
        inherit.aes = FALSE
      )
    }
  }

  plot <- plot +
    ggplot2::scale_y_continuous(
      breaks = yAxis[["breaks"]],
      labels = yAxis[["labels"]],
      limits = yAxis[["range"]],
      expand = .bfdPriorYAxisExpansion()
    ) +
    ggplot2::labs(
      y        = gettext("Density"),
      color    = if (mergedAesthetics) gettext("Hypothesis") else legendTitle,
      linetype = if (mergedAesthetics) gettext("Prior") else NULL
    )

  linetypeValues <- if (mergedAesthetics) .bfdPriorTypeLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdBinomialPriorDensityData <- function(settings, x, priorSet) {
  rows <- list()

  if (.bfdPriorSetIncludes(priorSet, "analysis"))
    rows <- c(rows, .bfdBinomialAnalysisPriorDensityRows(settings, x))

  if (.bfdPriorSetIncludes(priorSet, "design"))
    rows <- c(rows, .bfdBinomialDesignPriorDensityRows(settings, x))

  if (length(rows) == 0) {
    return(data.frame(
      x       = numeric(0),
      density = numeric(0),
      prior   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  return(do.call(rbind, rows))
}

.bfdBinomialAnalysisPriorDensityRows <- function(settings, x) {
  h1Density <- stats::dbeta(x, settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])

  if (settings[["nullPriorDistribution"]] == "direction") {
    h1Normalizer <- stats::pbeta(settings[["nullProportion"]], settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]], lower.tail = FALSE)
    h1Density    <- h1Density / h1Normalizer
    h1AtNull     <- stats::dbeta(settings[["nullProportion"]], settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]]) / h1Normalizer

    h0Density <- stats::dbeta(x, settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])
    h0Normalizer <- stats::pbeta(settings[["nullProportion"]], settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])
    h0Density    <- h0Density / h0Normalizer
    h0AtNull     <- stats::dbeta(settings[["nullProportion"]], settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]]) / h0Normalizer
  } else {
    h0Density <- NULL
  }

  rows <- list(.bfdPriorDensityRows(
    x       = x,
    density = h1Density,
    prior   = .bfdAnalysisPriorPlotLabel("h1"),
    lower   = if (settings[["nullPriorDistribution"]] == "direction") settings[["nullProportion"]] else -Inf,
    lowerDensity = if (settings[["nullPriorDistribution"]] == "direction") h1AtNull else NULL
  ))

  if (!is.null(h0Density)) {
    rows[[length(rows) + 1]] <- .bfdPriorDensityRows(
      x            = x,
      density      = h0Density,
      prior        = .bfdAnalysisPriorPlotLabel("h0"),
      upper        = settings[["nullProportion"]],
      upperDensity = h0AtNull
    )
  }

  return(rows)
}

.bfdBinomialDesignPriorDensityRows <- function(settings, x) {
  rows <- list()
  for (under in c("h0", "h1")) {
    designPrior <- .bfdBinomialDesignPriorForUnder(settings, under)
    if (designPrior[["distribution"]] == "beta") {
      designDensity <- stats::dbeta(x, designPrior[["a"]], designPrior[["b"]])
      designNormalizer <- diff(stats::pbeta(c(designPrior[["lower"]], designPrior[["upper"]]),
                                            designPrior[["a"]],
                                            designPrior[["b"]]))
      lowerDensity <- stats::dbeta(designPrior[["lower"]], designPrior[["a"]], designPrior[["b"]]) / designNormalizer
      upperDensity <- stats::dbeta(designPrior[["upper"]], designPrior[["a"]], designPrior[["b"]]) / designNormalizer
      rows[[length(rows) + 1]] <- .bfdPriorDensityRows(
        x            = x,
        density      = designDensity / designNormalizer,
        prior        = .bfdDesignPriorPlotLabel(under),
        lower        = designPrior[["lower"]],
        upper        = designPrior[["upper"]],
        lowerDensity = lowerDensity,
        upperDensity = upperDensity
      )
    }
  }

  return(rows)
}

.bfdContinuousPriorSpikeData <- function(settings, priorSet, densityData) {
  height <- .bfdPriorSpikeHeight(densityData)
  rows   <- list()

  if (.bfdPriorSetIncludes(priorSet, "analysis")) {
    if (!.bfdUsesDirectionalZTest(settings)) {
      rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(settings[["nullValue"]], .bfdAnalysisPriorPlotLabel("h0"), height)
    }
    if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "point") {
      rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(settings[["analysisPriorMean"]], .bfdAnalysisPriorPlotLabel("h1"), height)
    }
  }

  if (.bfdPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .bfdContinuousDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "point")
        rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(designPrior[["mean"]], .bfdDesignPriorPlotLabel(under), height)
    }
  }

  return(.bfdPriorSpikeRows(rows))
}

.bfdBinomialPriorSpikeData <- function(settings, priorSet, densityData) {
  height <- .bfdPriorSpikeHeight(densityData)
  rows   <- list()

  if (.bfdPriorSetIncludes(priorSet, "analysis") && settings[["nullPriorDistribution"]] != "direction")
    rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(settings[["nullProportion"]], .bfdAnalysisPriorPlotLabel("h0"), height)

  if (.bfdPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .bfdBinomialDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "point")
        rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(designPrior[["proportion"]], .bfdDesignPriorPlotLabel(under), height)
    }
  }

  return(.bfdPriorSpikeRows(rows))
}

.bfdPriorSpikeHeight <- function(densityData) {
  if (nrow(densityData) == 0)
    return(1)

  density <- densityData[["density"]]
  density <- density[is.finite(density)]
  if (length(density) == 0 || max(density) <= 0)
    return(1)

  return(1.2 * max(density))
}

.bfdPriorSpikeRow <- function(x, prior, height) {
  return(data.frame(
    x       = x,
    height  = height,
    prior   = prior,
    stringsAsFactors = FALSE
  ))
}

.bfdPriorSpikeRows <- function(rows) {
  if (length(rows) == 0) {
    return(data.frame(
      x       = numeric(0),
      height  = numeric(0),
      prior   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  return(do.call(rbind, rows))
}

.bfdPriorYAxis <- function(densityData, spikeData) {
  if (nrow(densityData) == 0 && nrow(spikeData) > 0) {
    return(list(
      breaks = c(0, 1),
      labels = c("0", "\u221E"),
      range  = c(0, 1)
    ))
  }

  y <- c(densityData[["density"]], spikeData[["height"]])
  y <- y[is.finite(y)]
  yMaximum <- if (length(y) == 0) 1 else max(y)
  breaks   <- .bfdPriorPrettyBreaks(c(0, yMaximum))

  if (max(breaks) < yMaximum)
    breaks <- .bfdPriorPrettyBreaks(c(0, yMaximum * 1.05))

  return(list(
    breaks = breaks,
    labels = ggplot2::waiver(),
    range  = range(breaks)
  ))
}

.bfdPriorYAxisExpansion <- function() {
  return(ggplot2::expansion(mult = c(0, 0.04)))
}

.bfdPriorPrettyBreaks <- function(x) {
  breaks <- jaspGraphs::getPrettyAxisBreaks(x)
  breaks <- breaks[is.finite(breaks)]

  if (length(breaks) < 2)
    breaks <- base::pretty(x)

  breaks <- sort(unique(c(0, breaks)))
  breaks <- breaks[breaks >= 0]

  if (length(breaks) < 2)
    breaks <- c(0, max(x, na.rm = TRUE))

  return(breaks)
}

.bfdPriorDensityRows <- function(x, density, prior, lower = -Inf, upper = Inf, lowerDensity = NULL, upperDensity = NULL) {
  if (!is.finite(lower) && !is.finite(upper))
    return(.bfdPriorDensityFrame(x, density, prior))

  parts <- list()
  if (is.finite(lower)) {
    parts[[length(parts) + 1]] <- .bfdPriorDensityFrame(x[x < lower], rep(0, sum(x < lower)), prior)
    parts[[length(parts) + 1]] <- .bfdPriorDensityFrame(lower, 0, prior)
    if (!is.null(lowerDensity) && is.finite(lowerDensity))
      parts[[length(parts) + 1]] <- .bfdPriorDensityFrame(lower, lowerDensity, prior)
  }

  support <- x > lower & x < upper
  parts[[length(parts) + 1]] <- .bfdPriorDensityFrame(x[support], density[support], prior)

  if (is.finite(upper)) {
    if (!is.null(upperDensity) && is.finite(upperDensity))
      parts[[length(parts) + 1]] <- .bfdPriorDensityFrame(upper, upperDensity, prior)
    parts[[length(parts) + 1]] <- .bfdPriorDensityFrame(upper, 0, prior)
    parts[[length(parts) + 1]] <- .bfdPriorDensityFrame(x[x > upper], rep(0, sum(x > upper)), prior)
  }

  return(do.call(rbind, parts))
}

.bfdPriorDensityFrame <- function(x, density, prior) {
  if (length(x) == 0) {
    return(data.frame(
      x       = numeric(0),
      density = numeric(0),
      prior   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data <- data.frame(
    x       = x,
    density = density,
    prior   = rep(prior, length(x)),
    stringsAsFactors = FALSE
  )
  data <- data[is.finite(data[["x"]]) & is.finite(data[["density"]]), , drop = FALSE]

  return(data)
}

.bfdZUnitStandardDeviation <- function(settings, n1) {
  if (isTRUE(settings[["isGeneralZ"]])) {
    return(.bfdGeneralZUnitInformationSd(settings))
  }

  if (!settings[["isIndependentSamples"]])
    return(settings[["standardDeviation"]])

  n2 <- .bfdSampleSizeSecondGroup(settings, n1)
  return(settings[["standardDeviation"]] * sqrt(1 + n1 / n2))
}

.bfdGeneralZUnitInformationSd <- function(settings) {
  parameterization <- settings[["generalZParameterization"]]

  if (parameterization == "unitInformationSd")
    return(settings[["unitInformationSd"]])

  switch(
    parameterization,
    standardizedMeanDifference = sqrt(2),
    fisherZCorrelation        = 1,
    logRiskRatio              = sqrt(4),
    logOddsRatio              = sqrt(4),
    logHazardRatio            = sqrt(4),
    logIncidenceRateRatio     = sqrt(4),
    effectSize                = 1,
    1
  )
}

.bfdGeneralZKnownUisdFootnote <- function(settings) {
  if (!isTRUE(settings[["isGeneralZ"]]))
    return(NULL)

  if (!.bfdGeneralZUsesKnownUisd(settings))
    return(NULL)

  gettextf(
    "The %1$s parameterization assumes a unit information standard deviation (UISD) of %2$s.",
    .bfdGeneralZParameterizationLabel(settings[["generalZParameterization"]]),
    .bfdFormatNumber(.bfdGeneralZUnitInformationSd(settings))
  )
}

.bfdGeneralZUsesKnownUisd <- function(settings) {
  settings[["generalZParameterization"]] %in% c(
    "standardizedMeanDifference",
    "fisherZCorrelation",
    "logRiskRatio",
    "logOddsRatio",
    "logHazardRatio",
    "logIncidenceRateRatio",
    "effectSize"
  )
}

.bfdGeneralZParameterizationLabel <- function(parameterization) {
  switch(
    parameterization,
    standardizedMeanDifference = gettext("SMD"),
    fisherZCorrelation        = gettext("Fisher's z"),
    logRiskRatio              = gettext("logRR"),
    logOddsRatio              = gettext("logOR"),
    logHazardRatio            = gettext("logHR"),
    logIncidenceRateRatio     = gettext("logIRR"),
    effectSize                = gettext("effect size"),
    gettext("effect size")
  )
}

.bfdSampleSizeSecondGroup <- function(settings, n1) {
  if (!settings[["isIndependentSamples"]])
    return(n1)

  return(ceiling(n1 * settings[["sampleSizeRatio"]]))
}

.bfdValidateSampleSize <- function(settings, n1) {
  if (settings[["isIndependentSamples"]] && settings[["isTTest"]] && .bfdSampleSizeSecondGroup(settings, n1) <= 1)
    stop(gettext("The sample-size ratio leads to a second-group sample size smaller than 2."))
}

.bfdMinimumSampleSize <- function(settings) {
  minimumN <- max(ceiling(settings[["rangeMin"]]), if (settings[["isBinomial"]]) 1 else 2)

  if (settings[["isIndependentSamples"]] && settings[["isTTest"]]) {
    while (.bfdSampleSizeSecondGroup(settings, minimumN) <= 1)
      minimumN <- minimumN + 1
  }

  return(minimumN)
}

.bfdBinomialDesignPriorForUnder <- function(settings, under) {
  if (under == "h0")
    return(settings[["designPriorUnderH0"]])

  return(settings[["designPriorUnderH1"]])
}

.bfdBinomialDesignArguments <- function(settings, under = "h1") {
  designPrior <- .bfdBinomialDesignPriorForUnder(settings, under)
  if (designPrior[["distribution"]] == "point")
    return(list(dp = designPrior[["proportion"]]))

  return(list(
    dp = NA_real_,
    da = designPrior[["a"]],
    db = designPrior[["b"]],
    dl = designPrior[["lower"]],
    du = designPrior[["upper"]]
  ))
}

.bfdBinomialNullDesignArguments <- function(settings) {
  if (settings[["nullPriorDistribution"]] == "direction") {
    return(list(
      dp = NA_real_,
      da = settings[["analysisPriorSuccesses"]],
      db = settings[["analysisPriorFailures"]],
      dl = 0,
      du = settings[["nullProportion"]]
    ))
  }

  return(list(dp = settings[["nullProportion"]]))
}

.bfdValidateSettings <- function(settings) {
  .bfdValidateTargetPowers(settings)

  if (identical(settings[["calculation"]], "sampleSize") && length(settings[["planningTargets"]]) == 0)
    stop(gettext("Select at least one Bayes factor planning target."))

  if (settings[["isBinomial"]]) {
    for (under in c("h0", "h1")) {
      designPrior <- .bfdBinomialDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "beta" && designPrior[["lower"]] >= designPrior[["upper"]])
        stop(gettext("The lower beta design-prior truncation must be smaller than the upper truncation."))
    }
  }
}

.bfdDrange <- function(settings) {
  if (settings[["drangeMode"]] != "custom")
    return("adaptive")

  if (settings[["drangeLower"]] >= settings[["drangeUpper"]])
    stop(gettext("The lower t search bound must be smaller than the upper bound."))

  return(c(settings[["drangeLower"]], settings[["drangeUpper"]]))
}

.bfdTSearchRange <- function(settings, n1, k) {
  if (settings[["drangeMode"]] == "custom")
    return(.bfdDrange(settings))

  if (isTRUE(all.equal(settings[["nullValue"]], 0)))
    return("adaptive")

  if (settings[["alternative"]] != "two.sided")
    return("adaptive")

  n2    <- .bfdSampleSizeSecondGroup(settings, n1)
  neff  <- if (settings[["testType"]] == "two.sample") 1 / (1 / n1 + 1 / n2) else n1
  pscale <- settings[["tPriorScale"]]
  ploc   <- settings[["tPriorLocationRelative"]]

  suppressWarnings({
    x <- (log(1 + neff * pscale^2) + ploc^2 / pscale^2 - log(k^2)) *
      (1 + 1 / neff / pscale^2) / neff
    sqrtX <- sqrt(x)
  })

  if (is.nan(sqrtX))
    sqrtX <- 0.3

  m     <- ploc / neff / pscale^2
  lower <- -sqrtX - m
  upper <-  sqrtX - m

  searchRange <- settings[["nullValue"]] + c(min(lower, upper) - 0.01, max(lower, upper) + 0.01)

  return(range(c(searchRange, .bfdEffectRange(settings))))
}

.bfdPriorAxis <- function(settings, priorSet = c("analysis", "design")) {
  requestedRange <- if (settings[["isBinomial"]]) .bfdBinomialPriorRange(settings, priorSet) else .bfdContinuousPriorRange(settings, priorSet)
  breaks         <- .bfdPrettyPriorBreaks(requestedRange, isBinomial = settings[["isBinomial"]])

  return(list(
    breaks = breaks,
    range  = range(breaks)
  ))
}

.bfdContinuousPriorRange <- function(settings, priorSet = c("analysis", "design")) {
  rangeValues <- numeric(0)

  if (.bfdPriorSetIncludes(priorSet, "analysis")) {
    rangeValues <- c(rangeValues, settings[["nullValue"]])

    if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "point") {
      rangeValues <- c(rangeValues, settings[["analysisPriorMean"]])
    } else if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
      rangeValues <- c(rangeValues, .bfdPriorInterval(settings[["analysisPriorMean"]], settings[["analysisPriorSd"]]))
    } else if (settings[["isZTest"]]) {
      rangeValues <- c(rangeValues, .bfdPriorInterval(settings[["nullValue"]], sqrt(3) * settings[["momentPriorSpread"]]))
    } else {
      rangeValues <- c(rangeValues, .bfdPriorInterval(settings[["tPriorLocation"]], .bfdStudentTPriorSpread(settings)))

      if (settings[["alternative"]] != "two.sided")
        rangeValues <- c(rangeValues, settings[["nullValue"]])
    }
  }

  if (.bfdPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .bfdContinuousDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "normal") {
        rangeValues <- c(rangeValues, .bfdPriorInterval(designPrior[["mean"]], designPrior[["sd"]]))
      } else {
        rangeValues <- c(rangeValues, designPrior[["mean"]])
      }
    }
  }

  return(.bfdFiniteRange(rangeValues))
}

.bfdBinomialPriorRange <- function(settings, priorSet = c("analysis", "design")) {
  rangeValues <- numeric(0)

  if (.bfdPriorSetIncludes(priorSet, "analysis")) {
    rangeValues <- c(
      rangeValues,
      settings[["nullProportion"]],
      .bfdBetaPriorInterval(settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])
    )
  }

  if (.bfdPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .bfdBinomialDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "point") {
        rangeValues <- c(rangeValues, designPrior[["proportion"]])
      } else {
        rangeValues <- c(rangeValues, .bfdBetaPriorInterval(
          designPrior[["a"]],
          designPrior[["b"]],
          lower = designPrior[["lower"]],
          upper = designPrior[["upper"]]
        ))
      }
    }
  }

  rangeValues <- pmin(1, pmax(0, rangeValues))

  return(.bfdFiniteRange(rangeValues, fallback = c(0, 1)))
}

.bfdBinomialProportionAxis <- function(settings) {
  requestedRange <- .bfdBinomialPriorRange(settings)
  breaks         <- .bfdPrettyPriorBreaks(requestedRange, isBinomial = TRUE)

  return(list(
    breaks = breaks,
    range  = range(breaks)
  ))
}

.bfdPriorInterval <- function(center, sd) {
  if (!is.finite(sd) || sd <= 0)
    return(c(center, center))

  return(center + c(-1, 1) * 2.5 * sd)
}

.bfdBetaPriorInterval <- function(a, b, lower = 0, upper = 1) {
  mean <- a / (a + b)
  sd   <- sqrt(a * b / ((a + b)^2 * (a + b + 1)))

  interval <- .bfdPriorInterval(mean, sd)
  interval <- c(max(lower, interval[1]), min(upper, interval[2]))

  if (interval[1] >= interval[2])
    return(c(lower, upper))

  return(interval)
}

.bfdStudentTPriorSpread <- function(settings) {
  if (settings[["tPriorDf"]] > 2)
    return(settings[["tPriorScale"]] * sqrt(settings[["tPriorDf"]] / (settings[["tPriorDf"]] - 2)))

  return(settings[["tPriorScale"]])
}

.bfdPrettyPriorBreaks <- function(x, isBinomial) {
  x <- .bfdFiniteRange(x, fallback = if (isBinomial) c(0, 1) else c(-1, 1))

  if (identical(x[1], x[2]))
    x <- x + c(-0.5, 0.5)

  breaks <- jaspGraphs::getPrettyAxisBreaks(x)
  breaks <- breaks[is.finite(breaks)]

  if (isBinomial) {
    breaks <- breaks[breaks >= 0 & breaks <= 1]

    if (length(breaks) < 2)
      breaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  }

  if (length(breaks) < 2)
    breaks <- base::pretty(x)

  return(sort(unique(breaks)))
}

.bfdFiniteRange <- function(x, fallback = c(-1, 1)) {
  x <- x[is.finite(x)]

  if (length(x) == 0)
    return(fallback)

  return(range(x))
}

.bfdEffectRange <- function(settings) {
  designH0 <- .bfdContinuousDesignPriorForUnder(settings, "h0")
  designH1 <- .bfdContinuousDesignPriorForUnder(settings, "h1")
  anchors <- c(settings[["nullValue"]], designH0[["mean"]], designH1[["mean"]])

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "point") {
    anchors <- c(anchors, settings[["analysisPriorMean"]])
    spread  <- max(designH0[["sd"]], designH1[["sd"]], abs(diff(range(anchors))), 0.25)
  } else if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    anchors <- c(anchors, settings[["analysisPriorMean"]])
    spread  <- max(settings[["analysisPriorSd"]], designH0[["sd"]], designH1[["sd"]], abs(diff(range(anchors))), 0.25)
  } else if (settings[["isZTest"]]) {
    anchors <- c(anchors, settings[["nullValue"]] + c(-1, 1) * settings[["momentPriorMode"]])
    spread  <- max(settings[["momentPriorSpread"]], designH0[["sd"]], designH1[["sd"]], abs(diff(range(anchors))), 0.25)
  } else {
    anchors <- c(anchors, settings[["tPriorLocation"]])
    spread  <- max(settings[["tPriorScale"]], designH0[["sd"]], designH1[["sd"]], abs(diff(range(anchors))), 0.25)
  }

  lower <- min(anchors) - 1.5 * spread
  upper <- max(anchors) + 1.5 * spread

  return(c(lower, upper))
}

.bfdTestType <- function(test) {
  switch(test,
    independentSamplesTTest = "two.sample",
    independentSamplesZTest = "two.sample",
    pairedSamplesTTest      = "paired",
    pairedSamplesZTest      = "paired",
    oneSampleTTest          = "one.sample",
    oneSampleZTest          = "one.sample",
    oneSampleProportion     = "binomial",
    generalZApproximation   = "general"
  )
}

.bfdTestLabel <- function(test) {
  switch(test,
    independentSamplesTTest = gettext("independent samples t-test"),
    pairedSamplesTTest      = gettext("paired samples t-test"),
    oneSampleTTest          = gettext("one sample t-test"),
    independentSamplesZTest = gettext("independent samples z-test"),
    pairedSamplesZTest      = gettext("paired samples z-test"),
    oneSampleZTest          = gettext("one sample z-test"),
    oneSampleProportion     = gettext("one sample proportion test"),
    generalZApproximation   = gettext("general z-approximation")
  )
}

.bfdTargetLabel <- function(target) {
  if (target == "h1")
    return(gettext("H\u2081 (BF\u2081\u2080)"))

  return(gettext("H\u2080 (BF\u2080\u2081)"))
}

.bfdUnderLabel <- function(under) {
  if (under == "h1")
    return(gettext("Under H\u2081"))

  return(gettext("Under H\u2080"))
}

.bfdDesignPriorUnderLabel <- function(under) {
  if (under == "h1")
    return(gettext("Design Prior Under H\u2081"))

  return(gettext("Design Prior Under H\u2080"))
}

.bfdProbabilityColumnTitle <- function(settings) {
  if (settings[["evidenceTarget"]] == "h1")
    return("Pr(BF\u2081\u2080 \u2265 k)")

  return("Pr(BF\u2080\u2081 \u2265 k)")
}

.bfdThresholdColumnTitle <- function(settings) {
  if (settings[["evidenceTarget"]] == "h1")
    return("BF\u2081\u2080")

  return("BF\u2080\u2081")
}

.bfdThresholdText <- function(settings) {
  if (settings[["evidenceTarget"]] == "h1")
    return(gettextf("BF\u2081\u2080 \u2265 %1$s", .bfdFormatNumber(settings[["bfThreshold"]])))

  return(gettextf("BF\u2080\u2081 \u2265 %1$s", .bfdFormatNumber(settings[["bfThreshold"]])))
}

.bfdNullPriorLabel <- function(settings) {
  if (.bfdUsesDirectionalZTest(settings))
    return(gettext("Directional"))

  if (!settings[["isBinomial"]])
    return(gettext("Point null"))

  if (settings[["nullPriorDistribution"]] == "direction")
    return(gettext("Directional"))

  return(gettext("Point null"))
}

.bfdNullPriorParameters <- function(settings) {
  if (.bfdUsesDirectionalZTest(settings)) {
    if (settings[["alternative"]] == "less")
      return(gettextf("\u03B8 >= \u03B8\u2080, \u03B8\u2080 = %1$s", .bfdFormatNumber(settings[["nullValue"]])))

    return(gettextf("\u03B8 <= \u03B8\u2080, \u03B8\u2080 = %1$s", .bfdFormatNumber(settings[["nullValue"]])))
  }

  if (!settings[["isBinomial"]])
    return(gettextf("\u03B8\u2080 = %1$s", .bfdFormatNumber(settings[["nullValue"]])))

  if (settings[["nullPriorDistribution"]] == "direction")
    return(gettextf("p <= p\u2080, p\u2080 = %1$s", .bfdFormatNumber(settings[["nullProportion"]])))

  return(gettextf("p\u2080 = %1$s", .bfdFormatNumber(settings[["nullProportion"]])))
}

.bfdUsesDirectionalZTest <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return(FALSE)

  return(isTRUE(settings[["isDirectionalZTest"]]) || identical(settings[["analysisPriorDistribution"]], "directional"))
}

.bfdAnalysisPriorLabel <- function(settings) {
  if (settings[["isBinomial"]])
    return(gettext("Beta"))

  if (settings[["isTTest"]] && isTRUE(settings[["analysisPriorIsCauchy"]]))
    return(gettext("Cauchy"))

  if (settings[["isTTest"]])
    return(gettext("Student-t"))

  if (settings[["analysisPriorDistribution"]] == "point")
    return(gettext("Point"))

  if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional"))
    return(gettext("Normal"))

  return(gettext("Normal-moment"))
}

.bfdAnalysisPriorParameters <- function(settings) {
  if (settings[["isBinomial"]]) {
    return(gettextf(
      "a = %1$s, b = %2$s",
      .bfdFormatNumber(settings[["analysisPriorSuccesses"]]),
      .bfdFormatNumber(settings[["analysisPriorFailures"]])
    ))
  }

  if (settings[["isTTest"]]) {
    if (isTRUE(settings[["analysisPriorIsCauchy"]])) {
      return(gettextf(
        "location = %1$s, scale = %2$s",
        .bfdFormatNumber(settings[["tPriorLocation"]]),
        .bfdFormatNumber(settings[["tPriorScale"]])
      ))
    }

    return(gettextf(
      "location = %1$s, scale = %2$s, df = %3$s",
      .bfdFormatNumber(settings[["tPriorLocation"]]),
      .bfdFormatNumber(settings[["tPriorScale"]]),
      .bfdFormatNumber(settings[["tPriorDf"]])
    ))
  }

  if (settings[["analysisPriorDistribution"]] == "point") {
    return(gettextf(
      "location = %1$s",
      .bfdFormatNumber(settings[["analysisPriorMean"]])
    ))
  }

  if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    return(gettextf(
      "mean = %1$s, sd = %2$s",
      .bfdFormatNumber(settings[["analysisPriorMean"]]),
      .bfdFormatNumber(settings[["analysisPriorSd"]])
    ))
  }

  return(gettextf(
    "spread = %1$s, modes = +/- %2$s",
    .bfdFormatNumber(settings[["momentPriorSpread"]]),
    .bfdFormatNumber(settings[["momentPriorMode"]])
  ))
}

.bfdDesignPriorLabel <- function(settings, under = "h1") {
  if (settings[["isBinomial"]])
    return(.bfdBinomialDesignPriorForUnder(settings, under)[["label"]])

  return(.bfdContinuousDesignPriorForUnder(settings, under)[["label"]])
}

.bfdDesignPriorParameters <- function(settings, under = "h1") {
  if (settings[["isBinomial"]])
    return(.bfdBinomialDesignPriorForUnder(settings, under)[["parameters"]])

  return(.bfdContinuousDesignPriorForUnder(settings, under)[["parameters"]])
}

.bfdNullPriorString <- function(settings) {
  if (!settings[["isBinomial"]] && !.bfdUsesDirectionalZTest(settings)) {
    return(.bfdPriorString(
      gettext("Point"),
      gettextf("location = %1$s", .bfdFormatNumber(settings[["nullValue"]]))
    ))
  }

  if (settings[["isBinomial"]] && settings[["nullPriorDistribution"]] != "direction") {
    return(.bfdPriorString(
      gettext("Point"),
      gettextf("p = %1$s", .bfdFormatNumber(settings[["nullProportion"]]))
    ))
  }

  return(.bfdPriorString(.bfdNullPriorLabel(settings), .bfdNullPriorParameters(settings)))
}

.bfdAnalysisPriorString <- function(settings) {
  return(.bfdPriorString(.bfdAnalysisPriorTableLabel(settings), .bfdAnalysisPriorParameters(settings)))
}

.bfdAnalysisPriorTableLabel <- function(settings) {
  label <- .bfdAnalysisPriorLabel(settings)

  if (identical(settings[["alternative"]], "greater"))
    return(paste0(label, "\u208A"))

  if (identical(settings[["alternative"]], "less"))
    return(paste0(label, "\u208B"))

  return(label)
}

.bfdDesignPriorString <- function(settings, under = "h1") {
  return(.bfdPriorString(.bfdDesignPriorLabel(settings, under), .bfdDesignPriorParameters(settings, under)))
}

.bfdPriorString <- function(distribution, parameters) {
  return(paste0(distribution, "(", parameters, ")"))
}

.bfdDesignPriorPlotLabel <- function(under) {
  return(.bfdDesignPriorUnderLabel(under))
}

.bfdAnalysisPriorPlotLabel <- function(under) {
  if (under == "h0")
    return(gettext("Analysis Prior Under H\u2080"))

  return(gettext("Analysis Prior Under H\u2081"))
}
