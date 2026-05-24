BayesFactorSequentialDesign <- function(jaspResults, dataset, options) {
  settings    <- .bfsdPrepareSettings(options)
  computation <- .bfsdCachedComputation(jaspResults, settings)
  settings    <- computation[["settings"]]
  result      <- computation[["result"]]

  if (options[["designSummary"]])
    .bfsdResultsTable(jaspResults, settings, result)

  if (options[["sampleSizeSummary"]])
    .bfsdSampleSizeSummaryTable(jaspResults, settings, result)

  if (options[["decisionProbabilities"]])
    .bfsdDesignOutcomeTable(jaspResults, settings, result)

  if (options[["cumulativeDecisionProbabilities"]])
    .bfsdStagewiseTotalTable(jaspResults, settings, result)

  if (options[["incrementalDecisionProbabilities"]])
    .bfsdStagewiseIncrementalTable(jaspResults, settings, result)

  if (options[["stoppingBoundariesTable"]])
    .bfsdBoundariesTable(jaspResults, settings, result)

  if (options[["designSpecification"]])
    .bfsdPriorsTable(jaspResults, settings)

  if (.bfdObservedAnalysisReady(dataset, options, settings)) {
    .bfdObservedAnalysisTable(
      jaspResults  = jaspResults,
      dataset      = dataset,
      options      = options,
      settings     = settings,
      key          = "sequentialEvidenceObservedAnalysis",
      position     = 16,
      sequential   = TRUE,
      dependencies = .bfsdComputationDependencies
    )
  }

  if (options[["explanatoryText"]])
    .bfsdText(jaspResults, settings, result)

  if (options[["generateReport"]])
    .bfsdReport(jaspResults, settings, result)

  if (options[["generateRCode"]])
    .bfsdRCode(jaspResults, settings, result)

  if (options[["cumulativeDecisionProbabilitiesPlot"]])
    .bfsdStoppingProbabilitiesPlot(jaspResults, settings, result)

  if (options[["stoppingBoundariesPlot"]])
    .bfsdStoppingBoundariesPlot(jaspResults, settings, result)

  if (.bfdPriorPlotRequested(options))
    .bfsdPriorPlot(jaspResults, settings)

  return()
}

.bfsdComputationDependencies <- c(
  "statisticalTest", "calculationTarget", "conclusiveEvidenceThresholdH1", "conclusiveEvidenceThresholdH0",
  "probabilityOfConclusiveEvidenceUnderH1", "probabilityOfConclusiveEvidenceUnderH0", "designSampleSizeBasis", "analysisPriorDirection", "lookScheduleType",
  "numberOfLooks", "initialSampleSize", "sampleSizeIncreasePerLook", "maximumSampleSize", "sampleSizeSchedule",
  "sampleSizeScheduleGroup2", "sampleSizeAllocationRatio", "firstInformationFraction",
  "informationFractionSchedule", "lowerSearchBoundForMaximumSampleSize", "upperSearchBoundForMaximumSampleSize",
  "knownStandardDeviation",
  "generalZParameterization", "unitInformationSd", "standardErrorSchedule",
  "nullPriorDistribution", "nullValue", "analysisPriorDistribution",
  "analysisPriorLocation", "analysisPriorMean", "analysisPriorScale",
  "analysisPriorSpread", "analysisPriorMode", "tPriorLocation", "tPriorScale",
  "tPriorDegreesOfFreedom", "designNullPriorDistribution", "designNullPriorMean", "designNullPriorStandardDeviation",
  "designPriorDistribution", "designPriorMean", "designPriorStandardDeviation",
  "exactIntegrationOverAllRegions", "integrationMethod", "integrationAbsoluteTolerance",
  "integrationRelativeTolerance", "integrationMaximumPoints", "tSearchRangeMode", "tSearchRangeLower",
  "tSearchRangeUpper"
)

.bfsdTextDependencies   <- c(.bfsdComputationDependencies, "explanatoryText")
.bfsdReportDependencies <- c(.bfsdComputationDependencies, "generateReport", "generateReportLatexFormattedOutput")
.bfsdRCodeDependencies  <- c(.bfsdComputationDependencies, "generateRCode")
.bfsdSummaryDesignDependencies                 <- c(.bfsdComputationDependencies, "designSummary")
.bfsdSummarySampleSizeDependencies             <- c(.bfsdComputationDependencies, "sampleSizeSummary")
.bfsdSummaryEvidenceDependencies               <- c(.bfsdComputationDependencies, "decisionProbabilities")
.bfsdSummarySpecificationDependencies          <- c(.bfsdComputationDependencies, "designSpecification")
.bfsdStagewiseEvidenceDependencies             <- c(.bfsdComputationDependencies, "cumulativeDecisionProbabilities")
.bfsdStagewiseIncrementalEvidenceDependencies  <- c(.bfsdComputationDependencies, "incrementalDecisionProbabilities")
.bfsdStagewiseStoppingBoundariesDependencies   <- c(.bfsdComputationDependencies, "stoppingBoundariesTable")

.bfsdStoppingProbabilitiesPlotDependencies <- c(
  .bfsdComputationDependencies, "cumulativeDecisionProbabilitiesPlot",
  "combineH1H0Figures", "legendPosition", "colorPalette"
)
.bfsdStoppingProbabilitiesPlotDataDependencies <- .bfsdComputationDependencies
.bfsdStoppingBoundariesPlotDependencies <- c(
  .bfsdComputationDependencies, "stoppingBoundariesPlot", "legendPosition", "colorPalette"
)
.bfsdStoppingBoundariesPlotDataDependencies <- .bfsdComputationDependencies
.bfsdPriorPlotDependencies <- c(
  .bfsdComputationDependencies, "designPriorDistributionFigure", "analysisPriorDistributionFigure", "combineDesignAnalysisPriorFigures",
  "curvePoints", "legendPosition", "colorPalette"
)
.bfsdPriorPlotDataDependencies <- c(
  "statisticalTest", "analysisPriorDirection", "nullPriorDistribution", "nullValue",
  "analysisPriorDistribution", "analysisPriorLocation", "analysisPriorMean", "analysisPriorScale",
  "analysisPriorSpread", "analysisPriorMode", "tPriorLocation", "tPriorScale", "tPriorDegreesOfFreedom",
  "designNullPriorDistribution", "designNullPriorMean", "designNullPriorStandardDeviation",
  "designPriorDistribution", "designPriorMean", "designPriorStandardDeviation", "curvePoints"
)

.bfsdDisplaySettingNames <- c(
  "plotPoints", "mergeH1H0Figures",
  "priorPlotDesign", "priorPlotAnalysis", "priorPlotMerge",
  "reportLatex", "legendPosition", "colorPalette"
)

.bfsdCachedComputation <- function(jaspResults, settings) {
  state <- jaspResults[["sequentialEvidenceComputation"]]
  if (!is.null(state) && !is.null(state$object))
    return(.bfdApplyCurrentSettings(state$object, settings, .bfsdDisplaySettingNames))

  state <- createJaspState()
  state$dependOn(.bfsdComputationDependencies)
  jaspResults[["sequentialEvidenceComputation"]] <- state

  result           <- try(.bfsdComputeResult(settings), silent = TRUE)
  computedSettings <- settings
  if (!jaspBase::isTryError(result) && !is.null(result[["settings"]]))
    computedSettings <- result[["settings"]]

  state$object <- list(
    settings = computedSettings,
    result   = result
  )

  return(.bfdApplyCurrentSettings(state$object, settings, .bfsdDisplaySettingNames))
}

.bfsdPrepareSettings <- function(options) {
  test <- options[["statisticalTest"]]

  settings <- list(
    test                 = test,
    testLabel            = .bfdTestLabel(test),
    testType             = .bfdTestType(test),
    isIndependentSamples = grepl("independentSamples", test, fixed = TRUE),
    isTTest              = grepl("TTest", test, fixed = TRUE),
    isGeneralZ           = identical(test, "generalZApproximation"),
    isZTest              = grepl("ZTest", test, fixed = TRUE) || identical(test, "generalZApproximation"),
    isBinomial           = FALSE,
    calculation          = options[["calculationTarget"]],
    bf10Threshold        = options[["conclusiveEvidenceThresholdH1"]],
    bf01Threshold        = options[["conclusiveEvidenceThresholdH0"]],
    targetPowerH1        = options[["probabilityOfConclusiveEvidenceUnderH1"]],
    targetPowerH0        = options[["probabilityOfConclusiveEvidenceUnderH0"]],
    designSampleSizeBasis = options[["designSampleSizeBasis"]],
    planningTargets      = .bfdPlanningTargets(),
    k1                   = 1 / options[["conclusiveEvidenceThresholdH1"]],
    k0                   = options[["conclusiveEvidenceThresholdH0"]],
    lookScheduleMode     = options[["lookScheduleType"]],
    numberOfLooks        = options[["numberOfLooks"]],
    sampleSizeFirstLook  = options[["initialSampleSize"]],
    sampleSizeIncrease   = options[["sampleSizeIncreasePerLook"]],
    sampleSize           = options[["maximumSampleSize"]],
    sampleSizeSchedule   = options[["sampleSizeSchedule"]],
    sampleSizeSecondGroupSchedule = options[["sampleSizeScheduleGroup2"]],
    sampleSizeRatio      = options[["sampleSizeAllocationRatio"]],
    informationFractionFirstLook = options[["firstInformationFraction"]],
    informationFractionSchedule  = options[["informationFractionSchedule"]],
    rangeMin             = options[["lowerSearchBoundForMaximumSampleSize"]],
    rangeMax             = options[["upperSearchBoundForMaximumSampleSize"]],
    nullPriorDistribution = options[["nullPriorDistribution"]],
    nullValue            = options[["nullValue"]],
    strictIntegration    = options[["exactIntegrationOverAllRegions"]],
    integrationMethod    = options[["integrationMethod"]],
    integrationAbsEps    = options[["integrationAbsoluteTolerance"]],
    integrationRelEps    = options[["integrationRelativeTolerance"]],
    integrationMaxPts    = options[["integrationMaximumPoints"]],
    drangeMode           = options[["tSearchRangeMode"]],
    drangeLower          = options[["tSearchRangeLower"]],
    drangeUpper          = options[["tSearchRangeUpper"]],
    plotPoints           = options[["curvePoints"]],
    mergeH1H0Figures     = options[["combineH1H0Figures"]],
    priorPlotDesign      = options[["designPriorDistributionFigure"]],
    priorPlotAnalysis    = options[["analysisPriorDistributionFigure"]],
    priorPlotMerge       = options[["combineDesignAnalysisPriorFigures"]],
    reportLatex          = options[["generateReportLatexFormattedOutput"]],
    legendPosition       = options[["legendPosition"]],
    colorPalette         = options[["colorPalette"]]
  )

  settings <- .bfsdAddContinuousSettings(settings, options)

  return(settings)
}

.bfsdAddContinuousSettings <- function(settings, options) {
  settings[["standardDeviation"]] <- options[["knownStandardDeviation"]]
  settings[["generalZParameterization"]] <- options[["generalZParameterization"]]
  settings[["unitInformationSd"]]        <- options[["unitInformationSd"]]
  settings[["standardErrorSchedule"]]    <- options[["standardErrorSchedule"]]
  settings[["alternative"]]       <- switch(options[["analysisPriorDirection"]], twoSided = "two.sided", options[["analysisPriorDirection"]])

  if (settings[["isZTest"]] && settings[["alternative"]] != "two.sided" &&
      options[["analysisPriorDistribution"]] != "normal") {
    stop(gettext("Sequential one-sided z designs require a normal analysis prior."))
  }

  if (settings[["isZTest"]]) {
    settings <- .bfdAddContinuousZAnalysisPrior(
      settings              = settings,
      options               = options,
      includeRelativeMean   = TRUE,
      includeDirectional    = TRUE
    )
  } else {
    settings <- .bfdAddContinuousTAnalysisPrior(
      settings           = settings,
      options            = options,
      includeDirectional = TRUE
    )
  }

  settings <- .bfdAddContinuousDesignPriors(settings, options, includeRelativeToNull = TRUE)

  return(settings)
}

.bfsdUsesSampleSizeSearch <- function(settings) {
  return(identical(settings[["calculation"]], "sampleSize"))
}

.bfsdUsesStandardErrorOnly <- function(settings) {
  return(
    isTRUE(settings[["isGeneralZ"]]) &&
      identical(settings[["generalZParameterization"]], "standardErrorSchedule") &&
      !.bfsdUsesSampleSizeSearch(settings)
  )
}

.bfsdSearchLowerBound <- function(settings) {
  minimumN <- ceiling(settings[["rangeMin"]])

  if (identical(settings[["lookScheduleMode"]], "increase"))
    minimumN <- max(minimumN, ceiling(settings[["sampleSizeFirstLook"]]))

  return(minimumN)
}

.bfsdCompleteSettings <- function(settings) {
  if (.bfsdUsesStandardErrorOnly(settings))
    return(.bfsdApplyStandardErrorSchedule(settings))

  if (!.bfsdUsesSampleSizeSearch(settings))
    return(.bfsdApplyFixedSchedule(settings))

  return(settings)
}

.bfsdApplyFixedSchedule <- function(settings) {
  settings[["n1Seq"]] <- .bfsdSampleSizeSchedule(settings)
  settings[["n2Seq"]] <- .bfsdSampleSizeSecondGroup(settings, settings[["n1Seq"]])
  settings[["n1"]]    <- max(settings[["n1Seq"]])
  settings[["n2"]]    <- max(settings[["n2Seq"]])

  return(settings)
}

.bfsdApplyStandardErrorSchedule <- function(settings) {
  se <- .bfsdParseNumericSchedule(settings[["standardErrorSchedule"]], gettext("standard error schedule"))

  if (length(se) < 1)
    stop(gettext("The standard error schedule must contain at least one look."))

  if (any(diff(se) >= 0))
    stop(gettext("The standard error schedule must be strictly decreasing."))

  settings[["standardErrors"]] <- se
  settings[["n1Seq"]]          <- seq_along(se)
  settings[["n2Seq"]]          <- settings[["n1Seq"]]
  settings[["n1"]]             <- NA_integer_
  settings[["n2"]]             <- NA_integer_

  return(settings)
}

.bfsdSampleSizeSchedule <- function(settings) {
  if (settings[["lookScheduleMode"]] == "custom") {
    n <- .bfsdParseNumericSchedule(settings[["sampleSizeSchedule"]], gettext("sample size schedule"), integer = TRUE)

    if (length(n) < 1)
      stop(gettext("The custom sample size schedule must contain at least one look."))

    if (any(n < 2))
      stop(gettext("All sample sizes in the custom schedule must be at least 2."))

    if (any(diff(n) <= 0))
      stop(gettext("The custom sample size schedule must be strictly increasing."))

    return(n)
  }

  if (settings[["lookScheduleMode"]] == "increase")
    return(.bfsdSampleSizeIncreaseSchedule(settings))

  firstLook <- ceiling(settings[["sampleSizeFirstLook"]])
  lastLook  <- ceiling(settings[["sampleSize"]])
  looks     <- settings[["numberOfLooks"]]

  if (firstLook > lastLook)
    stop(gettext("The sample size at the first look must be smaller than or equal to the maximum sample size."))

  if (looks == 1)
    return(lastLook)

  if (firstLook == lastLook)
    stop(gettext("More than one look requires a maximum sample size larger than the first-look sample size."))

  n <- unique(as.integer(round(seq(firstLook, lastLook, length.out = looks))))

  if (length(n) != looks)
    stop(gettext("The number of looks is too large for the selected first-look and maximum sample sizes."))

  return(n)
}

.bfsdSampleSizeIncreaseSchedule <- function(settings, maximumN = settings[["sampleSize"]]) {
  firstLook <- ceiling(settings[["sampleSizeFirstLook"]])
  lastLook  <- ceiling(maximumN)
  increase  <- ceiling(settings[["sampleSizeIncrease"]])

  if (firstLook > lastLook)
    stop(gettext("The sample size at the first look must be smaller than or equal to the maximum sample size."))

  if (increase < 1)
    stop(gettext("The sample size increase must be at least 1."))

  n <- seq(firstLook, lastLook, by = increase)
  n <- unique(as.integer(c(n[n < lastLook], lastLook)))

  if (any(n < 2))
    stop(gettext("The generated sample-size schedule contains sample sizes smaller than 2."))

  if (any(diff(n) <= 0))
    stop(gettext("The generated sample-size schedule is not strictly increasing. Increase the maximum sample size or adjust the sample size increase."))

  return(n)
}

.bfsdSampleSizeSecondGroup <- function(settings, n1) {
  if (!settings[["isIndependentSamples"]])
    return(n1)

  if (settings[["lookScheduleMode"]] == "custom") {
    n2 <- .bfsdParseNumericSchedule(settings[["sampleSizeSecondGroupSchedule"]], gettext("group 2 sample size schedule"), integer = TRUE)

    if (length(n2) != length(n1))
      stop(gettext("The group 1 and group 2 custom sample size schedules must have the same number of looks."))

    if (any(n2 < 2))
      stop(gettext("All group 2 sample sizes in the custom schedule must be at least 2."))

    if (any(diff(n2) <= 0))
      stop(gettext("The custom group 2 sample size schedule must be strictly increasing."))

    return(n2)
  }

  n2 <- ceiling(n1 * settings[["sampleSizeRatio"]])
  .bfsdValidateSecondGroupSampleSize(n2)

  return(n2)
}

.bfsdValidateSecondGroupSampleSize <- function(n2) {
  if (any(n2 < 2))
    stop(gettext("The sample-size ratio leads to a second-group sample size smaller than 2."))

  return(invisible(TRUE))
}

.bfsdParseNumericSchedule <- function(text, label, integer = FALSE) {
  values <- as.character(text)
  values <- unlist(strsplit(values, "[,;[:space:]]+"))
  values <- values[nzchar(values)]
  parsed <- suppressWarnings(as.numeric(values))

  if (length(parsed) == 0 || any(!is.finite(parsed)))
    stop(gettextf("The %1$s must contain only finite numeric values.", label))

  if (any(parsed <= 0))
    stop(gettextf("The %1$s must contain only positive values.", label))

  if (integer)
    parsed <- as.integer(ceiling(parsed))

  return(parsed)
}

.bfsdComputeResult <- function(settings) {
  settings <- .bfsdCompleteSettings(settings)

  if (.bfsdUsesSampleSizeSearch(settings))
    return(.bfsdFindMaximumSampleSize(settings))

  result <- .bfsdComputeDesignResult(settings)
  result[["settings"]] <- settings

  return(result)
}

.bfsdComputeDesignResult <- function(settings) {
  design <- .bfsdRunDesign(
    settings,
    designPriorMean = settings[["designPriorMeanRelative"]],
    designPriorSd   = settings[["designPriorSd"]]
  )

  null <- .bfsdRunDesign(
    settings,
    designPriorMean = settings[["designNullPriorMeanRelative"]],
    designPriorSd   = settings[["designPriorUnderH0"]][["sd"]]
  )

  return(list(
    design = design,
    null   = null
  ))
}

.bfsdFindMaximumSampleSize <- function(settings) {
  .bfdValidateTargetPowers(settings)

  if (length(settings[["planningTargets"]]) == 0)
    stop(gettext("Select at least one Bayes factor planning target."))

  if (isTRUE(settings[["isGeneralZ"]]) && settings[["generalZParameterization"]] == "standardErrorSchedule")
    stop(gettext("Maximum sample size search is not available with a fixed standard error schedule."))

  minimumN <- .bfsdSearchLowerBound(settings)
  maximumN <- ceiling(settings[["rangeMax"]])

  if (minimumN >= maximumN)
    stop(gettext("The lower search bound must be smaller than the upper search bound."))

  cache <- new.env(parent = emptyenv())
  targetComputations <- lapply(settings[["planningTargets"]], function(target) {
    .bfsdFindMaximumSampleSizeForTarget(settings, target, minimumN, maximumN, cache)
  })
  names(targetComputations) <- settings[["planningTargets"]]

  displayMaximumN <- max(vapply(targetComputations, function(x) x[["maximumN"]], numeric(1)), na.rm = TRUE)
  displaySettings <- .bfsdSettingsForMaximumN(settings, displayMaximumN)
  result          <- .bfsdComputeDesignResult(displaySettings)
  targetResults   <- do.call(rbind, lapply(targetComputations, `[[`, "targetResult"))

  result[["settings"]] <- displaySettings
  result[["solver"]] <- list(
    maximumN           = displayMaximumN,
    targetResults      = targetResults,
    targetComputations = targetComputations,
    limitReached       = any(vapply(targetComputations, function(x) isTRUE(x[["limitReached"]]), logical(1)))
  )

  return(result)
}

.bfsdFindMaximumSampleSizeForTarget <- function(settings, target, minimumN, maximumN, cache) {
  lower <- .bfsdEvaluateTargetMaximumN(settings, target, minimumN, cache)
  if (is.finite(lower[["criterion"]]) && lower[["criterion"]] >= 0)
    return(.bfsdSolvedTargetMaximumSampleSize(settings, target, minimumN, cache))

  bracket <- .bfsdFindTargetMaximumNSearchBracket(settings, target, minimumN, maximumN, lower, cache)
  if (!is.null(bracket[["upperN"]]))
    return(.bfsdSearchTargetMaximumSampleSize(settings, target, minimumN, bracket[["upperN"]], cache))

  if (!is.null(bracket[["limit"]]))
    return(.bfsdTargetMaximumSampleSizeLimit(bracket[["limit"]]))

  if (!is.null(bracket[["error"]])) {
    stop(gettextf(
      "No valid sequential design could be computed within the selected search bounds: %1$s",
      bracket[["error"]]
    ))
  }

  stop(gettext("No valid sequential design could be computed within the selected search bounds."))
}

.bfsdFindTargetMaximumNSearchBracket <- function(settings, target, minimumN, maximumN, lower, cache) {
  lastFinite <- if (is.finite(lower[["criterion"]])) lower else NULL
  currentN   <- minimumN

  while (currentN < maximumN) {
    candidateN <- .bfsdNextSearchCandidate(currentN, maximumN)
    candidate  <- .bfsdEvaluateTargetMaximumN(settings, target, candidateN, cache)

    if (is.finite(candidate[["criterion"]])) {
      lastFinite <- candidate

      if (candidate[["criterion"]] >= 0)
        return(list(upperN = candidateN))

      if (candidateN == maximumN)
        return(list(limit = candidate))

      currentN <- candidateN
      next
    }

    boundary <- .bfsdSearchBeforeInvalidMaximumN(settings, target, currentN, candidateN, cache)
    if (!is.null(boundary[["upperN"]]) || !is.null(boundary[["limit"]]))
      return(boundary)

    if (!is.null(lastFinite))
      return(list(limit = lastFinite))

    return(list(error = candidate[["error"]]))
  }

  if (!is.null(lastFinite))
    return(list(limit = lastFinite))

  return(list(error = lower[["error"]]))
}

.bfsdNextSearchCandidate <- function(currentN, maximumN) {
  return(min(maximumN, max(currentN + 1L, as.integer(ceiling(2 * currentN)))))
}

.bfsdSearchBeforeInvalidMaximumN <- function(settings, target, validN, invalidN, cache) {
  lastFinite <- .bfsdEvaluateTargetMaximumN(settings, target, validN, cache)
  solvedN    <- if (is.finite(lastFinite[["criterion"]]) && lastFinite[["criterion"]] >= 0) validN else NULL

  while ((invalidN - validN) > 1) {
    midpoint <- floor((validN + invalidN) / 2)
    current  <- .bfsdEvaluateTargetMaximumN(settings, target, midpoint, cache)

    if (is.finite(current[["criterion"]])) {
      validN     <- midpoint
      lastFinite <- current

      if (current[["criterion"]] >= 0)
        solvedN <- midpoint
    } else {
      invalidN <- midpoint
    }
  }

  if (!is.null(solvedN))
    return(list(upperN = solvedN))

  if (is.finite(lastFinite[["criterion"]]))
    return(list(limit = lastFinite))

  return(list())
}

.bfsdSearchTargetMaximumSampleSize <- function(settings, target, minimumN, maximumN, cache) {
  lowerN <- minimumN
  upperN <- maximumN

  while ((upperN - lowerN) > 1) {
    midpoint <- floor((lowerN + upperN) / 2)
    current  <- .bfsdEvaluateTargetMaximumN(settings, target, midpoint, cache)

    if (is.finite(current[["criterion"]]) && current[["criterion"]] >= 0) {
      upperN <- midpoint
    } else {
      lowerN <- midpoint
    }
  }

  foundN <- upperN

  while (foundN > minimumN) {
    previous <- .bfsdEvaluateTargetMaximumN(settings, target, foundN - 1, cache)
    if (!is.finite(previous[["criterion"]]) || previous[["criterion"]] < 0)
      break

    foundN <- foundN - 1
  }

  while (foundN <= maximumN) {
    current <- .bfsdEvaluateTargetMaximumN(settings, target, foundN, cache)
    if (is.finite(current[["criterion"]]) && current[["criterion"]] >= 0)
      return(.bfsdSolvedTargetMaximumSampleSize(settings, target, foundN, cache))

    foundN <- foundN + 1
  }

  return(.bfsdTargetMaximumSampleSizeLimit(.bfsdEvaluateTargetMaximumN(settings, target, maximumN, cache)))
}

.bfsdEvaluateTargetMaximumN <- function(settings, target, maximumN, cache) {
  value <- .bfsdEvaluateMaximumN(settings, maximumN, cache)
  if (!is.null(value[["error"]]))
    return(list(criterion = NA_real_, error = value[["error"]]))

  targetResult <- .bfsdTargetResults(settings, value[["designResult"]], target)

  return(list(
    criterion    = targetResult[["actualProbability"]] - targetResult[["targetProbability"]],
    targetResult = targetResult,
    settings     = value[["settings"]],
    designResult = value[["designResult"]]
  ))
}

.bfsdEvaluateMaximumN <- function(settings, maximumN, cache) {
  key <- as.character(maximumN)
  if (exists(key, envir = cache, inherits = FALSE))
    return(get(key, envir = cache, inherits = FALSE))

  value <- try({
    candidateSettings <- .bfsdSettingsForMaximumN(settings, maximumN)
    designResult      <- .bfsdComputeDesignResult(candidateSettings)

    list(
      settings     = candidateSettings,
      designResult = designResult
    )
  }, silent = TRUE)

  if (jaspBase::isTryError(value))
    value <- list(error = .bfdCleanError(value))

  assign(key, value, envir = cache)
  return(value)
}

.bfsdSolvedTargetMaximumSampleSize <- function(settings, target, maximumN, cache) {
  evaluated <- .bfsdEvaluateTargetMaximumN(settings, target, maximumN, cache)

  return(list(
    maximumN     = maximumN,
    targetResult = .bfsdTargetResultWithMaximumN(evaluated[["targetResult"]], maximumN),
    settings     = evaluated[["settings"]],
    designResult = evaluated[["designResult"]]
  ))
}

.bfsdTargetMaximumSampleSizeLimit <- function(evaluated) {
  targetResult <- evaluated[["targetResult"]]
  targetResult[["actualProbability"]][!targetResult[["reached"]]] <- NA_real_

  return(list(
    maximumN     = max(evaluated[["settings"]][["n1Seq"]], na.rm = TRUE),
    targetResult = .bfsdTargetResultWithMaximumN(targetResult, NA_integer_),
    settings     = evaluated[["settings"]],
    designResult = evaluated[["designResult"]],
    limitReached = TRUE
  ))
}

.bfsdTargetResultWithMaximumN <- function(targetResult, maximumN) {
  targetResult[["maximumN"]] <- maximumN

  return(targetResult)
}

.bfsdTargetComputation <- function(result, target) {
  targetComputations <- result[["solver"]][["targetComputations"]]
  if (is.null(targetComputations) || is.null(targetComputations[[target]]))
    return(NULL)

  return(targetComputations[[target]])
}

.bfsdTargetSettings <- function(settings, result, target) {
  computation <- .bfsdTargetComputation(result, target)
  if (is.null(computation) || is.null(computation[["settings"]]))
    return(settings)

  return(computation[["settings"]])
}

.bfsdTargetDesignResult <- function(result, target) {
  computation <- .bfsdTargetComputation(result, target)
  if (is.null(computation) || is.null(computation[["designResult"]]))
    return(result)

  return(computation[["designResult"]])
}

.bfsdTargetDesign <- function(result, target) {
  targetResult <- .bfsdTargetDesignResult(result, target)
  if (target == "h0")
    return(targetResult[["null"]])

  return(targetResult[["design"]])
}

.bfsdTargetSampleSizeSchedule <- function(settings, result, target) {
  targetSettings <- .bfsdTargetSettings(settings, result, target)

  return(list(
    n1Seq = targetSettings[["n1Seq"]],
    n2Seq = targetSettings[["n2Seq"]]
  ))
}

.bfsdSampleSizeBasisTarget <- function(settings, under) {
  if (!.bfsdUsesSampleSizeSearch(settings))
    return(NULL)

  switch(settings[["designSampleSizeBasis"]],
    eachDesignHypothesis = under,
    bothDesignHypotheses = NULL,
    alternativeHypothesis = "h1",
    nullHypothesis        = "h0",
    under
  )
}

.bfsdBasisSettings <- function(settings, result, under) {
  target <- .bfsdSampleSizeBasisTarget(settings, under)
  if (is.null(target))
    return(settings)

  return(.bfsdTargetSettings(settings, result, target))
}

.bfsdBasisDesignResult <- function(result, target) {
  if (is.null(target))
    return(result)

  return(.bfsdTargetDesignResult(result, target))
}

.bfsdBasisDesignForUnder <- function(settings, result, under) {
  target       <- .bfsdSampleSizeBasisTarget(settings, under)
  designResult <- .bfsdBasisDesignResult(result, target)

  if (under == "h0")
    return(designResult[["null"]])

  return(designResult[["design"]])
}

.bfsdBasisSampleSizeSchedule <- function(settings, result, under) {
  basisSettings <- .bfsdBasisSettings(settings, result, under)

  return(list(
    n1Seq = basisSettings[["n1Seq"]],
    n2Seq = basisSettings[["n2Seq"]]
  ))
}

.bfsdCommonSampleSizeBasisTarget <- function(settings) {
  if (!.bfsdUsesSampleSizeSearch(settings))
    return(NULL)

  switch(settings[["designSampleSizeBasis"]],
    alternativeHypothesis = "h1",
    nullHypothesis        = "h0",
    NULL
  )
}

.bfsdCommonBasisSettings <- function(settings, result) {
  target <- .bfsdCommonSampleSizeBasisTarget(settings)
  if (is.null(target))
    return(settings)

  return(.bfsdTargetSettings(settings, result, target))
}

.bfsdCommonBasisDesignResult <- function(result, settings) {
  target <- .bfsdCommonSampleSizeBasisTarget(settings)
  if (is.null(target))
    return(result)

  return(.bfsdTargetDesignResult(result, target))
}

.bfsdBasisTargetReached <- function(settings, result, under) {
  if (!.bfsdUsesSampleSizeSearch(settings))
    return(TRUE)

  target <- .bfsdSampleSizeBasisTarget(settings, under)
  if (is.null(target))
    target <- under

  targetRows <- result[["solver"]][["targetResults"]]
  if (is.null(targetRows) || is.null(targetRows[["target"]]) || is.null(targetRows[["reached"]]))
    return(TRUE)

  index <- which(targetRows[["target"]] == target)
  if (length(index) == 0)
    return(FALSE)

  return(isTRUE(targetRows[["reached"]][index[1]]))
}

.bfsdTargetResults <- function(settings, result, targets = settings[["planningTargets"]]) {
  rows <- lapply(targets, function(target) {
    design <- if (target == "h0") result[["null"]] else result[["design"]]
    targetPower <- .bfdTargetPower(settings, target)
    actualPower <- .bfsdFinalTargetProbability(design, target)
    data.frame(
      target            = target,
      targetProbability = targetPower,
      actualProbability = actualPower,
      reached           = is.finite(actualPower) && actualPower >= targetPower,
      stringsAsFactors  = FALSE
    )
  })

  return(do.call(rbind, rows))
}

.bfsdDisplayMaximumFootnote <- function(settings, result) {
  if (.bfsdUsesSeparateLookSchedules(settings, result))
    return(NULL)

  targetRows <- result[["solver"]][["targetResults"]]
  if (is.null(targetRows) || is.null(targetRows[["maximumN"]]))
    return(NULL)

  reached <- !is.na(targetRows[["maximumN"]])
  if (sum(reached) < 2 || length(unique(targetRows[["maximumN"]][reached])) < 2)
    return(NULL)

  return(gettext("Stagewise evidence tables and figures are evaluated through the largest computed maximum sample size."))
}

.bfsdSearchDesignMean <- function(settings, target) {
  if (target == "h0")
    return(settings[["designNullPriorMeanRelative"]])

  return(settings[["designPriorMeanRelative"]])
}

.bfsdSearchDesignSd <- function(settings, target) {
  if (target == "h0")
    return(settings[["designPriorUnderH0"]][["sd"]])

  return(settings[["designPriorSd"]])
}

.bfsdFinalTargetProbability <- function(design, target) {
  if (target == "h1")
    return(.bfdClampProbability(utils::tail(design[["cumpH1"]], 1)))

  return(.bfdClampProbability(utils::tail(design[["cumpH0"]], 1)))
}

.bfsdSettingsForMaximumN <- function(settings, maximumN) {
  maximumN <- as.integer(ceiling(maximumN))
  if (settings[["lookScheduleMode"]] == "increase") {
    n1Seq <- .bfsdSampleSizeIncreaseSchedule(settings, maximumN)
  } else {
    fractions <- .bfsdInformationFractions(settings)
    n1Seq <- as.integer(ceiling(maximumN * fractions - sqrt(.Machine$double.eps)))
    n1Seq[length(n1Seq)] <- maximumN
  }

  if (any(n1Seq < 2))
    stop(gettext("The generated sample-size schedule contains sample sizes smaller than 2."))

  if (any(diff(n1Seq) <= 0))
    stop(gettext("The generated sample-size schedule is not strictly increasing. Increase the maximum sample size or adjust the information fractions."))

  candidate <- settings
  candidate[["n1Seq"]] <- n1Seq
  candidate[["n2Seq"]] <- if (candidate[["isIndependentSamples"]]) as.integer(ceiling(n1Seq * candidate[["sampleSizeRatio"]])) else n1Seq
  if (candidate[["isIndependentSamples"]])
    .bfsdValidateSecondGroupSampleSize(candidate[["n2Seq"]])
  candidate[["n1"]]    <- max(candidate[["n1Seq"]])
  candidate[["n2"]]    <- max(candidate[["n2Seq"]])

  return(candidate)
}

.bfsdInformationFractions <- function(settings) {
  if (settings[["lookScheduleMode"]] != "custom") {
    if (settings[["numberOfLooks"]] == 1)
      return(1)

    return(seq(settings[["informationFractionFirstLook"]], 1, length.out = settings[["numberOfLooks"]]))
  }

  fractions <- .bfsdParseNumericSchedule(settings[["informationFractionSchedule"]], gettext("information fraction schedule"))

  if (length(fractions) < 1)
    stop(gettext("The information fraction schedule must contain at least one look."))

  if (any(fractions > 1))
    stop(gettext("Information fractions must be less than or equal to 1."))

  if (any(diff(fractions) <= 0))
    stop(gettext("The information fraction schedule must be strictly increasing."))

  if (!isTRUE(all.equal(utils::tail(fractions, 1), 1, tolerance = sqrt(.Machine$double.eps))))
    stop(gettext("The information fraction schedule must end at 1."))

  return(fractions)
}

.bfsdRunDesign <- function(settings, designPriorMean, designPriorSd) {
  if (settings[["isZTest"]])
    return(.bfsdRunZDesign(settings, designPriorMean, designPriorSd))

  return(.bfsdRunTDesign(settings, designPriorMean, designPriorSd))
}

.bfsdRunZDesign <- function(settings, designPriorMean, designPriorSd) {
  se              <- .bfsdStandardErrors(settings)
  analysisPrior   <- .bfsdZAnalysisPrior(settings)
  designPriorMean <- .bfsdDirectionalMean(settings, designPriorMean)

  args <- list(
    k1     = settings[["k1"]],
    k0     = settings[["k0"]],
    se     = se,
    pm     = analysisPrior[["pm"]],
    psd    = analysisPrior[["psd"]],
    dpm    = designPriorMean,
    dpsd   = designPriorSd,
    type   = analysisPrior[["type"]],
    strict = settings[["strictIntegration"]]
  )

  if (!.bfsdUsesStandardErrorOnly(settings))
    args[["n"]] <- settings[["n1Seq"]]

  do.call(
    what = bfpwr::pbf01seq,
    args = c(args, .bfsdIntegrationArguments(settings))
  )
}

.bfsdRunTDesign <- function(settings, designPriorMean, designPriorSd) {
  do.call(
    what = bfpwr::ptbf01seq,
    args = c(
      list(
        k1          = settings[["k1"]],
        k0          = settings[["k0"]],
        n           = settings[["n1Seq"]],
        n1          = settings[["n1Seq"]],
        n2          = settings[["n2Seq"]],
        plocation   = settings[["tPriorLocationRelative"]],
        pscale      = settings[["tPriorScale"]],
        pdf         = settings[["tPriorDf"]],
        dpm         = designPriorMean,
        dpsd        = designPriorSd,
        type        = settings[["testType"]],
        alternative = settings[["alternative"]],
        strict      = settings[["strictIntegration"]],
        drange      = .bfsdDrange(settings)
      ),
      .bfsdIntegrationArguments(settings)
    )
  )
}

.bfsdStandardErrors <- function(settings) {
  if (isTRUE(settings[["isGeneralZ"]]))
    return(.bfsdGeneralZStandardErrors(settings))

  if (settings[["isIndependentSamples"]])
    return(settings[["standardDeviation"]] * sqrt(1 / settings[["n1Seq"]] + 1 / settings[["n2Seq"]]))

  return(settings[["standardDeviation"]] / sqrt(settings[["n1Seq"]]))
}

.bfsdZAnalysisPrior <- function(settings) {
  if (isTRUE(settings[["isDirectionalZTest"]])) {
    return(list(
      type = "directional",
      pm   = .bfsdDirectionalMean(settings, settings[["analysisPriorMeanRelative"]]),
      psd  = settings[["analysisPriorSd"]]
    ))
  }

  if (settings[["analysisPriorDistribution"]] == "point") {
    return(list(
      type = "normal",
      pm   = settings[["analysisPriorMeanRelative"]],
      psd  = 0
    ))
  }

  if (settings[["analysisPriorDistribution"]] == "normal") {
    return(list(
      type = "normal",
      pm   = settings[["analysisPriorMeanRelative"]],
      psd  = settings[["analysisPriorSd"]]
    ))
  }
  return(list(
    type = "moment",
    pm   = NULL,
    psd  = settings[["momentPriorSpread"]]
  ))
}

.bfsdDirectionalMean <- function(settings, mean) {
  if (!isTRUE(settings[["isDirectionalZTest"]]) || settings[["alternative"]] != "less")
    return(mean)

  return(-mean)
}

.bfsdGeneralZStandardErrors <- function(settings) {
  if (!is.null(settings[["standardErrors"]]))
    return(settings[["standardErrors"]])

  if (settings[["generalZParameterization"]] == "standardErrorSchedule") {
    se <- .bfsdParseNumericSchedule(settings[["standardErrorSchedule"]], gettext("standard error schedule"))

    if (length(se) != length(settings[["n1Seq"]]))
      stop(gettext("The standard error schedule must have the same number of entries as the look schedule."))

    if (any(diff(se) >= 0))
      stop(gettext("The standard error schedule must be strictly decreasing."))

    return(se)
  }

  unitSd <- .bfdGeneralZUnitInformationSd(settings)

  return(unitSd / sqrt(settings[["n1Seq"]]))
}

.bfsdIntegrationArguments <- function(settings) {
  args <- list(method = settings[["integrationMethod"]])

  if (settings[["integrationMethod"]] == "pmvnorm") {
    args[["abseps"]] <- settings[["integrationAbsEps"]]
    args[["releps"]] <- settings[["integrationRelEps"]]
    args[["maxpts"]] <- settings[["integrationMaxPts"]]
  }

  return(args)
}

.bfsdDrange <- function(settings) {
  if (settings[["drangeMode"]] != "custom")
    return("adaptive")

  if (settings[["drangeLower"]] >= settings[["drangeUpper"]])
    stop(gettext("The lower t search bound must be smaller than the upper bound."))

  return(c(settings[["drangeLower"]], settings[["drangeUpper"]]))
}

.bfsdResultsTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceResults",
    title        = gettext("Bayes Factor Sequential Design"),
    position     = 1,
    dependencies = .bfsdSummaryDesignDependencies
  )
  if (is.null(table))
    return()

  .bfsdAddResultsColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute Bayes factor sequential design: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.bfsdResultsRow(settings, result))

  generalZUisdFootnote <- .bfdGeneralZKnownUisdFootnote(settings)
  if (!is.null(generalZUisdFootnote))
    table$addFootnote(generalZUisdFootnote)

  if (.bfsdUsesSampleSizeSearch(settings))
    table$addFootnote(gettext("Due to integer sample-size schedules, Pr(Conclusive Evidence) can exceed the target probability."))

  combinedSampleSizeFootnote <- .bfsdCombinedMaximumSampleSizeFootnote(settings, result)
  if (!is.null(combinedSampleSizeFootnote))
    table$addFootnote(combinedSampleSizeFootnote)

  displayMaximumFootnote <- .bfsdDisplayMaximumFootnote(settings, result)
  if (!is.null(displayMaximumFootnote))
    table$addFootnote(displayMaximumFootnote)

  unreachedTargetsFootnote <- .bfsdUnreachedTargetsFootnote(result)
  if (!is.null(unreachedTargetsFootnote))
    table$addFootnote(unreachedTargetsFootnote)

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested N\u2082/N\u2081 sample-size ratio, unless a custom group 2 schedule is supplied."))

  if (isTRUE(settings[["strictIntegration"]]) && length(settings[["n1Seq"]]) > 10)
    table$addFootnote(gettext("Exact integration over all regions can be slow for designs with many looks."))

  if (.bfsdUsesStandardErrorOnly(settings))
    table$addFootnote(gettext("This design uses the supplied standard error schedule, so sample-size summaries are omitted."))
}

.bfsdAddResultsColumns <- function(table, settings) {
  computed    <- gettext("Computed")
  userDefined <- gettext("User Defined")

  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "decisionRule", title = gettext("Decision Rule"), type = "string", overtitle = userDefined)

  if (.bfsdUsesSampleSizeSearch(settings)) {
    table$addColumnInfo(name = "targetProbability", title = gettext("Target Probability"), type = "number", overtitle = userDefined)
  }

  table$addColumnInfo(name = "actualProbability", title = gettext("Achieved Probability"), type = "number", overtitle = computed)

  if (.bfsdUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "finalSE", title = gettext("Final SE"), type = "number", overtitle = computed)
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "maximumN1", title = gettext("Maximum N\u2081"), type = "integer", overtitle = computed)
    table$addColumnInfo(name = "maximumN2", title = gettext("Maximum N\u2082"), type = "integer", overtitle = computed)
  } else {
    table$addColumnInfo(name = "maximumN", title = gettext("Maximum N"), type = "integer", overtitle = computed)
  }

}

.bfsdResultsRow <- function(settings, result) {
  targetRows <- .bfsdMainTargetRows(settings, result)
  row <- data.frame(
    designPrior       = vapply(targetRows[["target"]], .bfdUnderLabel, character(1)),
    decisionRule      = vapply(targetRows[["target"]], function(target) .bfdDecisionRuleLabel(settings, target), character(1)),
    actualProbability = targetRows[["actualProbability"]],
    stringsAsFactors = FALSE
  )

  if (.bfsdUsesSampleSizeSearch(settings))
    row[["targetProbability"]] <- targetRows[["targetProbability"]]

  if (.bfsdUsesStandardErrorOnly(settings)) {
    row[["finalSE"]] <- utils::tail(.bfsdStandardErrors(settings), 1)
  } else {
    for (i in seq_len(nrow(targetRows))) {
      maximum <- .bfsdMaximumSampleSizes(settings, result, targetRows[["target"]][i])
      if (settings[["isIndependentSamples"]]) {
        row[["maximumN1"]][i] <- maximum[["n1"]]
        row[["maximumN2"]][i] <- maximum[["n2"]]
      } else {
        row[["maximumN"]][i] <- maximum[["n1"]]
      }
    }
  }

  if (.bfsdUsesSampleSizeSearch(settings))
    row <- .bfsdBlankUnreachedRows(row, targetRows)

  columnOrder <- c(
    "designPrior", "decisionRule",
    if (.bfsdUsesSampleSizeSearch(settings)) "targetProbability",
    "actualProbability",
    if (.bfsdUsesStandardErrorOnly(settings)) "finalSE",
    if (!.bfsdUsesStandardErrorOnly(settings) && settings[["isIndependentSamples"]]) c("maximumN1", "maximumN2"),
    if (!.bfsdUsesStandardErrorOnly(settings) && !settings[["isIndependentSamples"]]) "maximumN"
  )
  row <- row[, columnOrder, drop = FALSE]
  row.names(row) <- NULL

  return(row)
}

.bfsdSampleSizeSummaryTable <- function(jaspResults, settings, result) {
  if (.bfsdUsesStandardErrorOnly(settings))
    return()

  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceSampleSizeSummary",
    title        = gettext("Sample Size Operating Characteristics"),
    position     = 2,
    dependencies = .bfsdSummarySampleSizeDependencies
  )
  if (is.null(table))
    return()

  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  if (.bfsdUsesSampleSizeSearch(settings))
    table$addColumnInfo(name = "decisionRule", title = gettext("Decision Rule"), type = "string", overtitle = gettext("User Defined"))
  table$addColumnInfo(name = "looks", title = gettext("Looks"), type = "integer")

  if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "firstN1", title = "N\u2081", type = "integer", overtitle = gettext("First Look"))
    table$addColumnInfo(name = "firstN2", title = "N\u2082", type = "integer", overtitle = gettext("First Look"))
    table$addColumnInfo(name = "maximumN1", title = "N\u2081", type = "integer", overtitle = gettext("Maximum Sample Size"))
    table$addColumnInfo(name = "maximumN2", title = "N\u2082", type = "integer", overtitle = gettext("Maximum Sample Size"))
    table$addColumnInfo(name = "meanN1", title = "N\u2081", type = "number", overtitle = gettext("Mean Sample Size"))
    table$addColumnInfo(name = "meanN2", title = "N\u2082", type = "number", overtitle = gettext("Mean Sample Size"))
    table$addColumnInfo(name = "sdN1", title = "N\u2081", type = "number", overtitle = gettext("Standard Deviation"))
    table$addColumnInfo(name = "sdN2", title = "N\u2082", type = "number", overtitle = gettext("Standard Deviation"))
  } else {
    table$addColumnInfo(name = "firstN", title = "N", type = "integer", overtitle = gettext("First Look"))
    table$addColumnInfo(name = "maximumN", title = "N", type = "integer", overtitle = gettext("Maximum Sample Size"))
    table$addColumnInfo(name = "meanN", title = "N", type = "number", overtitle = gettext("Mean Sample Size"))
    table$addColumnInfo(name = "sdN", title = "N", type = "number", overtitle = gettext("Standard Deviation"))
  }

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute sample size summary: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.bfsdSampleSizeSummaryRows(settings, result))

  displayMaximumFootnote <- .bfsdDisplayMaximumFootnote(settings, result)
  if (!is.null(displayMaximumFootnote))
    table$addFootnote(displayMaximumFootnote)

  unreachedTargetsFootnote <- .bfsdUnreachedTargetsFootnote(result)
  if (!is.null(unreachedTargetsFootnote))
    table$addFootnote(unreachedTargetsFootnote)
}

.bfsdSampleSizeSummaryRows <- function(settings, result) {
  targetRows <- .bfsdMainTargetRows(settings, result)
  if (.bfsdUsesSampleSizeSearch(settings)) {
    rows <- data.frame(
      designPrior  = vapply(targetRows[["target"]], .bfdUnderLabel, character(1)),
      decisionRule = vapply(targetRows[["target"]], function(target) .bfdDecisionRuleLabel(settings, target), character(1)),
      looks        = NA_integer_,
      stringsAsFactors = FALSE
    )
  } else {
    rows <- data.frame(
      designPrior = vapply(targetRows[["target"]], .bfdUnderLabel, character(1)),
      looks       = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(targetRows))) {
    target   <- targetRows[["target"]][i]
    expected <- .bfsdExpectedSampleSizes(settings, result, target)
    schedule <- .bfsdBasisSampleSizeSchedule(settings, result, target)
    rows[["looks"]][i] <- length(schedule[["n1Seq"]])
    if (settings[["isIndependentSamples"]]) {
      rows[["firstN1"]][i]   <- schedule[["n1Seq"]][1]
      rows[["firstN2"]][i]   <- schedule[["n2Seq"]][1]
      rows[["maximumN1"]][i] <- max(schedule[["n1Seq"]])
      rows[["maximumN2"]][i] <- max(schedule[["n2Seq"]])
      rows[["meanN1"]][i]    <- expected[["n1"]][["mean"]]
      rows[["meanN2"]][i]    <- expected[["n2"]][["mean"]]
      rows[["sdN1"]][i]      <- expected[["n1"]][["sd"]]
      rows[["sdN2"]][i]      <- expected[["n2"]][["sd"]]
    } else {
      rows[["firstN"]][i]   <- schedule[["n1Seq"]][1]
      rows[["maximumN"]][i] <- max(schedule[["n1Seq"]])
      rows[["meanN"]][i]    <- expected[["n1"]][["mean"]]
      rows[["sdN"]][i]      <- expected[["n1"]][["sd"]]
    }
  }

  if (.bfsdUsesSampleSizeSearch(settings))
    rows <- .bfsdBlankUnreachedRows(rows, targetRows)

  row.names(rows) <- NULL

  return(rows)
}

.bfsdMainTargetRows <- function(settings, result) {
  targets <- c("h1", "h0")
  rows <- data.frame(
    target            = targets,
    actualProbability = vapply(targets, function(target) {
      if (!.bfsdBasisRowAvailable(settings, result, target))
        return(NA_real_)

      .bfsdFinalTargetProbability(.bfsdBasisDesignForUnder(settings, result, target), target)
    }, numeric(1)),
    stringsAsFactors = FALSE
  )

  if (.bfsdUsesSampleSizeSearch(settings)) {
    rows[["targetProbability"]] <- vapply(targets, function(target) .bfdTargetPower(settings, target), numeric(1))
    rows[["reached"]]           <- vapply(targets, function(target) .bfsdBasisRowAvailable(settings, result, target), logical(1))
  }

  return(rows)
}

.bfsdBasisRowAvailable <- function(settings, result, under) {
  if (!.bfsdUsesSampleSizeSearch(settings))
    return(TRUE)

  basisTarget <- .bfsdSampleSizeBasisTarget(settings, under)
  if (!is.null(basisTarget) && basisTarget != under)
    return(.bfsdBasisTargetReached(settings, result, basisTarget))

  return(.bfsdBasisTargetReached(settings, result, under))
}

.bfsdExpectedSampleSizes <- function(settings, result, target) {
  schedule <- .bfsdBasisSampleSizeSchedule(settings, result, target)
  design   <- .bfsdBasisDesignForUnder(settings, result, target)

  return(list(
    n1 = .bfsdExpectedSampleSize(design, schedule[["n1Seq"]]),
    n2 = .bfsdExpectedSampleSize(design, schedule[["n2Seq"]])
  ))
}

.bfsdMaximumSampleSizes <- function(settings, result, target) {
  schedule <- .bfsdBasisSampleSizeSchedule(settings, result, target)

  return(list(
    n1 = max(schedule[["n1Seq"]], na.rm = TRUE),
    n2 = max(schedule[["n2Seq"]], na.rm = TRUE)
  ))
}

.bfsdUnreachedTargetLabels <- function(result) {
  targetRows <- result[["solver"]][["targetResults"]]
  if (is.null(targetRows) || is.null(targetRows[["reached"]]))
    return(character(0))

  unreached <- targetRows[["target"]][!targetRows[["reached"]]]
  return(vapply(unreached, .bfdTargetLabel, character(1)))
}

.bfsdUnreachedTargetsFootnote <- function(result) {
  unreachedTargets <- .bfsdUnreachedTargetLabels(result)
  if (length(unreachedTargets) == 0)
    return(NULL)

  return(gettextf(
    "The requested BF target could not be reached within the selected maximum sample size for %1$s; entries that depend on reaching the target are left empty.",
    paste(unreachedTargets, collapse = ", ")
  ))
}

.bfsdBlankUnreachedRows <- function(row, targetRows) {
  reached <- targetRows[["reached"]]
  if (is.null(reached))
    return(row)

  reached <- !is.na(reached) & reached
  if (all(reached))
    return(row)

  blankColumns <- intersect(
    c(
      "actualProbability", "maximumN", "maximumN1", "maximumN2",
      "expectedN", "meanN", "sdN", "expectedN1", "expectedN2", "meanN1", "meanN2", "sdN1", "sdN2",
      "expectedNNull", "sdNNull", "expectedN1Null", "expectedN2Null", "sdN1Null", "sdN2Null",
      "firstN", "firstN1", "firstN2"
    ),
    names(row)
  )

  row[!reached, blankColumns] <- NA
  return(row)
}

.bfsdExpectedSampleSize <- function(design, n) {
  stageStop <- diff(c(0, design[["cumpH1"]] + design[["cumpH0"]]))
  expected  <- sum(stageStop * n) + (1 - sum(stageStop)) * max(n)
  expected2 <- sum(stageStop * n^2) + (1 - sum(stageStop)) * max(n)^2
  variance  <- max(0, expected2 - expected^2)

  return(c(mean = expected, sd = sqrt(variance)))
}

.bfsdCombinedMaximumSampleSizeFootnote <- function(settings, result) {
  if (!.bfsdUsesSampleSizeSearch(settings) || jaspBase::isTryError(result))
    return(NULL)

  targetRows <- result[["solver"]][["targetResults"]]
  if (is.null(targetRows) || is.null(targetRows[["reached"]]) || !all(targetRows[["reached"]]))
    return(NULL)

  n1 <- max(settings[["n1Seq"]], na.rm = TRUE)
  if (length(n1) != 1 || !is.finite(n1))
    return(NULL)

  if (settings[["isIndependentSamples"]]) {
    n2 <- max(settings[["n2Seq"]], na.rm = TRUE)
    if (length(n2) != 1 || !is.finite(n2))
      return(NULL)

    return(gettextf(
      "To satisfy both design priors in a single sequential design, use a final look of N\u2081 = %1$s and N\u2082 = %2$s (total N = %3$s).",
      n1,
      n2,
      n1 + n2
    ))
  }

  return(gettextf("To satisfy both design priors in a single sequential design, use a final look of N = %1$s.", n1))
}

.bfsdDesignOutcomeTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceDesignOutcome",
    title        = gettext("Bayes Factor Decision Probabilities"),
    position     = 3,
    dependencies = .bfsdSummaryEvidenceDependencies
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

  rows <- try(.bfsdDesignOutcomeRows(settings, result), silent = TRUE)
  if (jaspBase::isTryError(rows)) {
    table$setError(gettextf("Unable to compute Bayes factor decision probabilities: %1$s", .bfdCleanError(rows)))
    return()
  }

  table$setData(rows)
  table$addFootnote(.bfsdDesignOutcomeSampleSizeFootnote(settings, result))
}

.bfsdDesignOutcomeRows <- function(settings, result) {
  h1Outcome <- .bfsdFinalOutcome(.bfsdBasisDesignForUnder(settings, result, "h1"))
  h0Outcome <- .bfsdFinalOutcome(.bfsdBasisDesignForUnder(settings, result, "h0"))

  return(.bfdDesignOutcomeRowsFromOutcomes(
    h1Outcome,
    h0Outcome,
    underLabels = c(.bfdUnderLabel("h1"), .bfdUnderLabel("h0"))
  ))
}

.bfsdFinalOutcome <- function(design) {
  return(c(
    null        = .bfdClampProbability(utils::tail(design[["cumpH0"]], 1)),
    undecided   = .bfdClampProbability(utils::tail(design[["cumpInc"]], 1)),
    alternative = .bfdClampProbability(utils::tail(design[["cumpH1"]], 1))
  ))
}

.bfsdDesignOutcomeSampleSizeFootnote <- function(settings, result) {
  if (.bfsdUsesStandardErrorOnly(settings)) {
    finalSE <- utils::tail(.bfsdStandardErrors(settings), 1)
    return(gettextf(
      "Probabilities are cumulative through the selected final look: SE = %1$s. Rows use the corresponding design prior under H\u2081 or H\u2080.",
      .bfdFormatNumber(finalSE)
    ))
  }

  if (.bfsdUsesSampleSizeSearch(settings) && identical(settings[["designSampleSizeBasis"]], "eachDesignHypothesis")) {
    h1Schedule <- .bfsdBasisSampleSizeSchedule(settings, result, "h1")
    h0Schedule <- .bfsdBasisSampleSizeSchedule(settings, result, "h0")
    h1N1       <- max(h1Schedule[["n1Seq"]], na.rm = TRUE)
    h0N1       <- max(h0Schedule[["n1Seq"]], na.rm = TRUE)

    if (settings[["isIndependentSamples"]]) {
      h1N2 <- max(h1Schedule[["n2Seq"]], na.rm = TRUE)
      h0N2 <- max(h0Schedule[["n2Seq"]], na.rm = TRUE)
      return(gettextf(
        "Probabilities are cumulative through the final look required for each design prior: under H\u2081, N\u2081 = %1$s and N\u2082 = %2$s; under H\u2080, N\u2081 = %3$s and N\u2082 = %4$s.",
        h1N1,
        h1N2,
        h0N1,
        h0N2
      ))
    }

    return(gettextf(
      "Probabilities are cumulative through the final look required for each design prior: under H\u2081, N = %1$s; under H\u2080, N = %2$s.",
      h1N1,
      h0N1
    ))
  }

  basisSettings <- .bfsdBasisSettings(settings, result, "h1")
  n1 <- max(basisSettings[["n1Seq"]], na.rm = TRUE)
  if (length(n1) != 1 || !is.finite(n1))
    return(gettext("Probabilities are cumulative through the selected final look. Rows use the corresponding design prior under H\u2081 or H\u2080."))

  if (settings[["isIndependentSamples"]]) {
    n2 <- max(basisSettings[["n2Seq"]], na.rm = TRUE)
    if (length(n2) != 1 || !is.finite(n2))
      return(gettext("Probabilities are cumulative through the selected final look. Rows use the corresponding design prior under H\u2081 or H\u2080."))

    return(gettextf(
      "Probabilities are cumulative through the selected final look: N\u2081 = %1$s and N\u2082 = %2$s (total N = %3$s). Rows use the corresponding design prior under H\u2081 or H\u2080.",
      n1,
      n2,
      n1 + n2
    ))
  }

  return(gettextf(
    "Probabilities are cumulative through the selected final look: N = %1$s. Rows use the corresponding design prior under H\u2081 or H\u2080.",
    n1
  ))
}

.bfsdStagewiseTotalTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceStagewiseTotal",
    title        = gettext("Cumulative Decision Probabilities by Look"),
    position     = 5,
    dependencies = .bfsdStagewiseEvidenceDependencies
  )
  if (is.null(table))
    return()

  if (jaspBase::isTryError(result)) {
    .bfsdAddStagewiseTotalColumns(table, settings)
    table$setError(gettextf("Unable to compute cumulative decision probabilities: %1$s", .bfdCleanError(result)))
    return()
  }

  .bfsdAddStagewiseTotalColumns(table, settings, result)
  table$setData(.bfsdStagewiseTotalRows(settings, result))
}

.bfsdStagewiseIncrementalTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceStagewiseIncremental",
    title        = gettext("New Stop Probabilities by Look"),
    position     = 6,
    dependencies = .bfsdStagewiseIncrementalEvidenceDependencies
  )
  if (is.null(table))
    return()

  if (jaspBase::isTryError(result)) {
    .bfsdAddStagewiseIncrementalColumns(table, settings)
    table$setError(gettextf("Unable to compute incremental stagewise stops: %1$s", .bfdCleanError(result)))
    return()
  }

  .bfsdAddStagewiseIncrementalColumns(table, settings, result)
  table$setData(.bfsdStagewiseIncrementalRows(settings, result))
}

.bfsdDesignPriorOvertitle <- function(under) {
  gettextf("Design Prior: %1$s", .bfdUnderLabel(under))
}

.bfsdAddLookColumns <- function(table, settings, separate = TRUE) {
  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  if (.bfsdUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "standardError", title = gettext("SE"), type = "number")
  } else if (isTRUE(separate) && .bfsdUsesSeparateLookSchedules(settings)) {
    h1Overtitle <- .bfsdDesignPriorOvertitle("h1")
    h0Overtitle <- .bfsdDesignPriorOvertitle("h0")
    if (settings[["isIndependentSamples"]]) {
      table$addColumnInfo(name = "h1N1", title = "N\u2081", type = "integer", overtitle = h1Overtitle)
      table$addColumnInfo(name = "h1N2", title = "N\u2082", type = "integer", overtitle = h1Overtitle)
      table$addColumnInfo(name = "h0N1", title = "N\u2081", type = "integer", overtitle = h0Overtitle)
      table$addColumnInfo(name = "h0N2", title = "N\u2082", type = "integer", overtitle = h0Overtitle)
    } else {
      table$addColumnInfo(name = "h1N", title = "N", type = "integer", overtitle = h1Overtitle)
      table$addColumnInfo(name = "h0N", title = "N", type = "integer", overtitle = h0Overtitle)
    }
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer")
    table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer")
  } else {
    table$addColumnInfo(name = "n", title = "N", type = "integer")
  }
}

.bfsdAddStagewiseTotalColumns <- function(table, settings, result = NULL) {
  separateSchedules <- .bfsdUsesSeparateLookSchedules(settings, result)

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")
  if (separateSchedules) {
    .bfsdAddStagewiseSampleSizeColumns(table, settings, "h1")
    .bfsdAddStagewiseTotalEvidenceColumns(table, "h1")
    .bfsdAddStagewiseSampleSizeColumns(table, settings, "h0")
    .bfsdAddStagewiseTotalEvidenceColumns(table, "h0")
  } else {
    .bfsdAddStagewiseSampleSizeColumns(table, settings)
    .bfsdAddStagewiseTotalEvidenceColumns(table, "h1")
    .bfsdAddStagewiseTotalEvidenceColumns(table, "h0")
  }
}

.bfsdAddStagewiseIncrementalColumns <- function(table, settings, result = NULL) {
  separateSchedules <- .bfsdUsesSeparateLookSchedules(settings, result)

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")
  if (separateSchedules) {
    .bfsdAddStagewiseSampleSizeColumns(table, settings, "h1")
    .bfsdAddStagewiseIncrementalEvidenceColumns(table, "h1")
    .bfsdAddStagewiseSampleSizeColumns(table, settings, "h0")
    .bfsdAddStagewiseIncrementalEvidenceColumns(table, "h0")
  } else {
    .bfsdAddStagewiseSampleSizeColumns(table, settings)
    .bfsdAddStagewiseIncrementalEvidenceColumns(table, "h1")
    .bfsdAddStagewiseIncrementalEvidenceColumns(table, "h0")
  }
}

.bfsdAddStagewiseSampleSizeColumns <- function(table, settings, under = NULL) {
  overtitle <- if (is.null(under)) NULL else .bfsdDesignPriorOvertitle(under)
  prefix    <- if (is.null(under)) "n" else under

  if (.bfsdUsesStandardErrorOnly(settings)) {
    .bfsdAddColumnInfo(table, name = "standardError", title = gettext("SE"), type = "number", overtitle = overtitle)
  } else if (settings[["isIndependentSamples"]]) {
    .bfsdAddColumnInfo(table, name = paste0(prefix, if (is.null(under)) "1" else "N1"), title = "N\u2081", type = "integer", overtitle = overtitle)
    .bfsdAddColumnInfo(table, name = paste0(prefix, if (is.null(under)) "2" else "N2"), title = "N\u2082", type = "integer", overtitle = overtitle)
  } else {
    .bfsdAddColumnInfo(table, name = if (is.null(under)) "n" else paste0(prefix, "N"), title = "N", type = "integer", overtitle = overtitle)
  }
}

.bfsdAddColumnInfo <- function(table, name, title, type, overtitle = NULL) {
  if (is.null(overtitle)) {
    table$addColumnInfo(name = name, title = title, type = type)
  } else {
    table$addColumnInfo(name = name, title = title, type = type, overtitle = overtitle)
  }
}

.bfsdAddStagewiseTotalEvidenceColumns <- function(table, under) {
  overtitle <- .bfsdDesignPriorOvertitle(under)
  prefix    <- under

  table$addColumnInfo(name = paste0(prefix, "Alternative"), title = gettext("Evidence for H\u2081"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = paste0(prefix, "Undecided"),   title = gettext("Inconclusive"),     type = "number", overtitle = overtitle)
  table$addColumnInfo(name = paste0(prefix, "Null"),        title = gettext("Evidence for H\u2080"), type = "number", overtitle = overtitle)
}

.bfsdAddStagewiseIncrementalEvidenceColumns <- function(table, under) {
  overtitle <- .bfsdDesignPriorOvertitle(under)
  prefix    <- under

  table$addColumnInfo(name = paste0(prefix, "AlternativeStop"), title = gettext("Stop for H\u2081"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = paste0(prefix, "NullStop"),        title = gettext("Stop for H\u2080"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = paste0(prefix, "AnyStop"),         title = gettext("Any Stop"),      type = "number", overtitle = overtitle)
}

.bfsdUsesSeparateLookSchedules <- function(settings, result = NULL) {
  if (!.bfsdUsesSampleSizeSearch(settings) || !identical(settings[["designSampleSizeBasis"]], "eachDesignHypothesis"))
    return(FALSE)

  if (is.null(result) || jaspBase::isTryError(result))
    return(TRUE)

  return(!.bfsdLookSchedulesEqual(settings, result))
}

.bfsdLookSchedulesEqual <- function(settings, result) {
  h1Schedule <- .bfsdBasisSampleSizeSchedule(settings, result, "h1")
  h0Schedule <- .bfsdBasisSampleSizeSchedule(settings, result, "h0")

  identical(as.integer(h1Schedule[["n1Seq"]]), as.integer(h0Schedule[["n1Seq"]])) &&
    identical(as.integer(h1Schedule[["n2Seq"]]), as.integer(h0Schedule[["n2Seq"]]))
}

.bfsdStagewiseTotalRows <- function(settings, result) {
  if (.bfsdUsesSeparateLookSchedules(settings, result)) {
    rows     <- .bfsdSeparateLookRows(settings, result)
    h1Design <- .bfsdBasisDesignForUnder(settings, result, "h1")
    h0Design <- .bfsdBasisDesignForUnder(settings, result, "h0")
    rows[["h1Alternative"]] <- .bfsdPadByLooks(h1Design[["cumpH1"]], nrow(rows))
    rows[["h1Undecided"]]   <- .bfsdPadByLooks(h1Design[["cumpInc"]], nrow(rows))
    rows[["h1Null"]]        <- .bfsdPadByLooks(h1Design[["cumpH0"]], nrow(rows))
    rows[["h0Alternative"]] <- .bfsdPadByLooks(h0Design[["cumpH1"]], nrow(rows))
    rows[["h0Undecided"]]   <- .bfsdPadByLooks(h0Design[["cumpInc"]], nrow(rows))
    rows[["h0Null"]]        <- .bfsdPadByLooks(h0Design[["cumpH0"]], nrow(rows))

    return(.bfsdOrderStagewiseRows(rows, settings, result, "total"))
  }

  basisSettings <- .bfsdBasisSettings(settings, result, "h1")
  rows          <- .bfsdLookRows(basisSettings)
  h1Design      <- .bfsdBasisDesignForUnder(settings, result, "h1")
  h0Design      <- .bfsdBasisDesignForUnder(settings, result, "h0")
  rows[["h1Alternative"]] <- h1Design[["cumpH1"]]
  rows[["h1Undecided"]]   <- h1Design[["cumpInc"]]
  rows[["h1Null"]]        <- h1Design[["cumpH0"]]
  rows[["h0Alternative"]] <- h0Design[["cumpH1"]]
  rows[["h0Undecided"]]   <- h0Design[["cumpInc"]]
  rows[["h0Null"]]        <- h0Design[["cumpH0"]]

  return(.bfsdOrderStagewiseRows(rows, settings, result, "total"))
}

.bfsdStagewiseIncrementalRows <- function(settings, result) {
  h1Design <- .bfsdBasisDesignForUnder(settings, result, "h1")
  h0Design <- .bfsdBasisDesignForUnder(settings, result, "h0")

  h1AlternativeStop <- diff(c(0, h1Design[["cumpH1"]]))
  h1NullStop        <- diff(c(0, h1Design[["cumpH0"]]))
  h0AlternativeStop <- diff(c(0, h0Design[["cumpH1"]]))
  h0NullStop        <- diff(c(0, h0Design[["cumpH0"]]))

  rows <- if (.bfsdUsesSeparateLookSchedules(settings, result)) .bfsdSeparateLookRows(settings, result) else .bfsdLookRows(.bfsdBasisSettings(settings, result, "h1"))
  rows[["h1AlternativeStop"]] <- .bfsdPadByLooks(h1AlternativeStop, nrow(rows))
  rows[["h1NullStop"]]        <- .bfsdPadByLooks(h1NullStop, nrow(rows))
  rows[["h1AnyStop"]]         <- .bfsdPadByLooks(h1AlternativeStop + h1NullStop, nrow(rows))
  rows[["h0AlternativeStop"]] <- .bfsdPadByLooks(h0AlternativeStop, nrow(rows))
  rows[["h0NullStop"]]        <- .bfsdPadByLooks(h0NullStop, nrow(rows))
  rows[["h0AnyStop"]]         <- .bfsdPadByLooks(h0AlternativeStop + h0NullStop, nrow(rows))

  return(.bfsdOrderStagewiseRows(rows, settings, result, "incremental"))
}

.bfsdOrderStagewiseRows <- function(rows, settings, result, type) {
  if (.bfsdUsesSeparateLookSchedules(settings, result)) {
    columnNames <- c(
      "look",
      .bfsdStagewiseSampleSizeColumnNames(settings, "h1"),
      .bfsdStagewiseEvidenceColumnNames("h1", type),
      .bfsdStagewiseSampleSizeColumnNames(settings, "h0"),
      .bfsdStagewiseEvidenceColumnNames("h0", type)
    )
  } else {
    columnNames <- c(
      "look",
      .bfsdStagewiseSampleSizeColumnNames(settings),
      .bfsdStagewiseEvidenceColumnNames("h1", type),
      .bfsdStagewiseEvidenceColumnNames("h0", type)
    )
  }

  return(rows[intersect(columnNames, names(rows))])
}

.bfsdStagewiseSampleSizeColumnNames <- function(settings, under = NULL) {
  if (.bfsdUsesStandardErrorOnly(settings))
    return("standardError")

  if (settings[["isIndependentSamples"]]) {
    if (is.null(under))
      return(c("n1", "n2"))

    return(paste0(under, c("N1", "N2")))
  }

  if (is.null(under))
    return("n")

  return(paste0(under, "N"))
}

.bfsdStagewiseEvidenceColumnNames <- function(under, type) {
  if (type == "incremental")
    return(paste0(under, c("AlternativeStop", "NullStop", "AnyStop")))

  return(paste0(under, c("Alternative", "Undecided", "Null")))
}

.bfsdSeparateLookRows <- function(settings, result) {
  h1Settings <- .bfsdBasisSettings(settings, result, "h1")
  h0Settings <- .bfsdBasisSettings(settings, result, "h0")
  looks      <- max(length(h1Settings[["n1Seq"]]), length(h0Settings[["n1Seq"]]))
  rows       <- data.frame(look = seq_len(looks), stringsAsFactors = FALSE)

  if (settings[["isIndependentSamples"]]) {
    rows[["h1N1"]] <- .bfsdPadByLooks(h1Settings[["n1Seq"]], looks)
    rows[["h1N2"]] <- .bfsdPadByLooks(h1Settings[["n2Seq"]], looks)
    rows[["h0N1"]] <- .bfsdPadByLooks(h0Settings[["n1Seq"]], looks)
    rows[["h0N2"]] <- .bfsdPadByLooks(h0Settings[["n2Seq"]], looks)
  } else {
    rows[["h1N"]] <- .bfsdPadByLooks(h1Settings[["n1Seq"]], looks)
    rows[["h0N"]] <- .bfsdPadByLooks(h0Settings[["n1Seq"]], looks)
  }

  return(rows)
}

.bfsdPadByLooks <- function(x, lengthOut) {
  out <- rep(NA_real_, lengthOut)
  out[seq_along(x)] <- x
  return(out)
}

.bfsdLookRows <- function(settings) {
  rows <- data.frame(
    look = seq_along(settings[["n1Seq"]]),
    stringsAsFactors = FALSE
  )

  if (.bfsdUsesStandardErrorOnly(settings)) {
    rows[["standardError"]] <- .bfsdStandardErrors(settings)
  } else if (settings[["isIndependentSamples"]]) {
    rows[["n1"]] <- settings[["n1Seq"]]
    rows[["n2"]] <- settings[["n2Seq"]]
  } else {
    rows[["n"]] <- settings[["n1Seq"]]
  }

  return(rows)
}

.bfsdBoundariesTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceBoundaries",
    title        = gettext("Stopping Boundaries"),
    position     = 7,
    dependencies = .bfsdStagewiseStoppingBoundariesDependencies
  )
  if (is.null(table))
    return()

  .bfsdAddLookColumns(table, settings, separate = FALSE)

  h1Overtitle <- .bfsdBoundaryDecisionRuleOvertitle(settings, "h1")
  h0Overtitle <- .bfsdBoundaryDecisionRuleOvertitle(settings, "h0")
  table$addColumnInfo(name = "h1Lower", title = gettext("Lower Statistic"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1Upper", title = gettext("Upper Statistic"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h0Lower", title = gettext("Lower Statistic"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0Upper", title = gettext("Upper Statistic"), type = "number", overtitle = h0Overtitle)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stopping boundaries: %1$s", .bfdCleanError(result)))
    return()
  }

  boundarySettings <- .bfsdCommonBasisSettings(settings, result)
  boundaryResult   <- .bfsdCommonBasisDesignResult(result, settings)
  rows <- .bfsdBoundaryRows(boundarySettings, boundaryResult[["design"]])
  table$setData(rows)

  table$addFootnote(gettext("Boundaries are shown on the test-statistic scale. Empty cells mean no finite statistic reaches the decision rule at that look."))
}

.bfsdBoundaryDecisionRuleOvertitle <- function(settings, target) {
  if (target == "h1")
    return(gettextf("Stop for H\u2081: %1$s", .bfdDecisionRuleLabel(settings, target)))

  return(gettextf("Stop for H\u2080: %1$s", .bfdDecisionRuleLabel(settings, target)))
}

.bfsdBoundaryRows <- function(settings, design) {
  h1 <- .bfsdBoundaryMatrix(design[["zk1"]], design[["zk0"]])
  h0 <- .bfsdBoundaryMatrix(design[["zk0"]], design[["zk1"]])

  rows <- .bfsdLookRows(settings)
  rows[["h1Lower"]] <- h1[1, ]
  rows[["h1Upper"]] <- h1[2, ]
  rows[["h0Lower"]] <- h0[1, ]
  rows[["h0Upper"]] <- h0[2, ]

  return(rows)
}

.bfsdBoundaryMatrix <- function(boundary, otherBoundary) {
  boundary <- replace(boundary, !is.finite(boundary), NA_real_)

  if (is.matrix(boundary))
    return(boundary)

  direction <- if (all(boundary >= otherBoundary, na.rm = TRUE)) "positive" else "negative"
  out       <- matrix(NA_real_, nrow = 2, ncol = length(boundary))

  if (direction == "positive") {
    out[2, ] <- boundary
  } else {
    out[1, ] <- boundary
  }

  return(out)
}

.bfsdPriorsTable <- function(jaspResults, settings) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidencePriors",
    title        = gettext("Design Specification"),
    position     = 4,
    dependencies = .bfsdSummarySpecificationDependencies
  )
  if (is.null(table))
    return()

  .bfdAddPriorsTableColumns(table)
  table$setData(.bfdPriorsRows(settings))

  if (!identical(settings[["nullValue"]], 0))
    table$addFootnote(gettext("Sequential bfpwr calculations are performed on the parameter scale centered at the null value."))
}

.bfsdText <- function(jaspResults, settings, result) {
  html <- .bfdCreateHtml(
    parent       = jaspResults,
    key          = "sequentialEvidenceText",
    title        = gettext("Explanation"),
    position     = 9,
    dependencies = .bfsdTextDependencies
  )
  if (is.null(html))
    return()

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested Bayes factor sequential design could not be completed with the current settings.")
    return()
  }

  outcome <- .bfsdFinalOutcome(.bfsdBasisDesignForUnder(settings, result, "h1"))
  firstSentence <- if (.bfsdUsesSampleSizeSearch(settings)) {
    if (isTRUE(result[["solver"]][["limitReached"]])) {
      gettextf(
        "This group-sequential design is evaluated at the selected maximum sample size for %1$s at %2$s planned looks.",
        .bfdPlanningTargetText(settings),
        length(settings[["n1Seq"]])
      )
    } else {
      gettextf(
        "This group-sequential design finds separate maximum sample sizes for %1$s at %2$s planned looks.",
        .bfdPlanningTargetText(settings),
        length(settings[["n1Seq"]])
      )
    }
  } else {
    gettextf(
      "This group-sequential design computes cumulative stopping probabilities for %1$s at %2$s planned looks.",
      settings[["testLabel"]],
      length(settings[["n1Seq"]])
    )
  }

  html[["text"]] <- paste0(
    "<p>",
    firstSentence,
    "</p><p>",
    gettextf(
      "At the final look under the H\u2081 design prior, the conclusive evidence probability for H\u2081 is %1$s, the conclusive evidence probability for H\u2080 is %2$s, and the probability of remaining inconclusive is %3$s.",
      .bfdFormatNumber(outcome[["alternative"]]),
      .bfdFormatNumber(outcome[["null"]]),
      .bfdFormatNumber(outcome[["undecided"]])
    ),
    "</p>"
  )
}

.bfsdReport <- function(jaspResults, settings, result) {
  html <- .bfdCreateHtml(
    parent       = jaspResults,
    key          = "sequentialEvidenceReport",
    title        = gettext("Report"),
    position     = 0,
    dependencies = .bfsdReportDependencies
  )
  if (is.null(html))
    return()

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The report could not be generated because the Bayes factor sequential design could not be completed with the current settings.")
    return()
  }

  paragraph <- try(
    paste(
      .bfdReportOpeningSentence(settings, designType = gettext("sequential")),
      .bfdReportThresholdSentence(settings),
      .bfsdReportSampleSizeSentence(settings, result),
      .bfdReportPriorSentence(settings),
      .bfdReportProbabilitySentence(
        settings,
        h1Outcome = .bfsdFinalOutcome(.bfsdBasisDesignForUnder(settings, result, "h1")),
        h0Outcome = .bfsdFinalOutcome(.bfsdBasisDesignForUnder(settings, result, "h0"))
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

.bfsdReportSampleSizeSentence <- function(settings, result) {
  if (.bfsdUsesStandardErrorOnly(settings)) {
    return(gettext(
      "The design used a fixed standard-error schedule, so n_min, n_max, batch size, expected sample size, and sample-size SD are not defined."
    ))
  }

  pieces <- c(
    if (.bfsdUsesSampleSizeSearch(settings)) .bfsdReportTargetSentence(settings),
    .bfsdReportScheduleSentence(settings, result),
    .bfsdReportExpectedSampleSizeText(settings, result)
  )

  paste(pieces, collapse = " ")
}

.bfsdReportTargetSentence <- function(settings) {
  gettextf(
    "The design targeted conclusive evidence probabilities of %1$s under H\u2081 and %2$s under H\u2080.",
    .bfdReportPercent(.bfdTargetPower(settings, "h1")),
    .bfdReportPercent(.bfdTargetPower(settings, "h0"))
  )
}

.bfsdReportScheduleSentence <- function(settings, result = NULL) {
  if (.bfsdUsesSampleSizeSearch(settings) && !is.null(result))
    return(.bfsdReportSearchedScheduleSentence(settings, result))

  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "The design started at n_min,1 = %1$s and n_min,2 = %2$s, used %3$s, and stopped at n_max,1 = %4$s and n_max,2 = %5$s.",
      .bfdReportNumber(settings[["n1Seq"]][1]),
      .bfdReportNumber(settings[["n2Seq"]][1]),
      .bfsdReportBatchText(settings),
      .bfdReportNumber(max(settings[["n1Seq"]], na.rm = TRUE)),
      .bfdReportNumber(max(settings[["n2Seq"]], na.rm = TRUE))
    ))
  }

  return(gettextf(
    "The design started at n_min = %1$s, used %2$s, and stopped at n_max = %3$s.",
    .bfdReportNumber(settings[["n1Seq"]][1]),
    .bfsdReportBatchText(settings),
    .bfdReportNumber(max(settings[["n1Seq"]], na.rm = TRUE))
  ))
}

.bfsdReportSearchedScheduleSentence <- function(settings, result) {
  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "The design started at n_min,1 = %1$s and n_min,2 = %2$s, used %3$s, and found maximum sample sizes n_max,1 = %4$s and n_max,2 = %5$s under H\u2081 and n_max,1 = %6$s and n_max,2 = %7$s under H\u2080.",
      .bfdReportNumber(settings[["n1Seq"]][1]),
      .bfdReportNumber(settings[["n2Seq"]][1]),
      .bfsdReportBatchText(settings),
      .bfsdReportTargetMaximumText(settings, result, "h1", "n1"),
      .bfsdReportTargetMaximumText(settings, result, "h1", "n2"),
      .bfsdReportTargetMaximumText(settings, result, "h0", "n1"),
      .bfsdReportTargetMaximumText(settings, result, "h0", "n2")
    ))
  }

  return(gettextf(
    "The design started at n_min = %1$s, used %2$s, and found maximum sample sizes n_max = %3$s under H\u2081 and n_max = %4$s under H\u2080.",
    .bfdReportNumber(settings[["n1Seq"]][1]),
    .bfsdReportBatchText(settings),
    .bfsdReportTargetMaximumText(settings, result, "h1", "n1"),
    .bfsdReportTargetMaximumText(settings, result, "h0", "n1")
  ))
}

.bfsdReportTargetMaximumText <- function(settings, result, target, sampleSize = "n1") {
  targetRows <- result[["solver"]][["targetResults"]]
  index <- which(targetRows[["target"]] == target)
  if (length(index) == 0 || is.na(targetRows[["maximumN"]][index[1]]))
    return(gettext("not reached"))

  schedule <- .bfsdTargetSampleSizeSchedule(settings, result, target)
  n <- if (sampleSize == "n2") max(schedule[["n2Seq"]], na.rm = TRUE) else max(schedule[["n1Seq"]], na.rm = TRUE)

  return(.bfdReportNumber(n))
}

.bfsdReportRangeText <- function(settings) {
  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "minimum sample sizes n_min,1 = %1$s and n_min,2 = %2$s and maximum sample sizes n_max,1 = %3$s and n_max,2 = %4$s",
      .bfdReportNumber(settings[["n1Seq"]][1]),
      .bfdReportNumber(settings[["n2Seq"]][1]),
      .bfdReportNumber(max(settings[["n1Seq"]], na.rm = TRUE)),
      .bfdReportNumber(max(settings[["n2Seq"]], na.rm = TRUE))
    ))
  }

  return(gettextf(
    "minimum sample size n_min = %1$s and maximum sample size n_max = %2$s",
    .bfdReportNumber(settings[["n1Seq"]][1]),
    .bfdReportNumber(max(settings[["n1Seq"]], na.rm = TRUE))
  ))
}

.bfsdReportBatchText <- function(settings) {
  n1Increments <- diff(settings[["n1Seq"]])

  if (length(n1Increments) == 0)
    return(gettext("a single planned look"))

  if (settings[["isIndependentSamples"]]) {
    n2Increments <- diff(settings[["n2Seq"]])
    if (.bfsdReportConstantIncrements(n1Increments) && .bfsdReportConstantIncrements(n2Increments)) {
      return(gettextf(
        "batches of %1$s for n\u2081 and %2$s for n\u2082",
        .bfdReportNumber(n1Increments[1]),
        .bfdReportNumber(n2Increments[1])
      ))
    }

    return(gettextf(
      "batch-size increments of %1$s for n\u2081 and %2$s for n\u2082",
      .bfsdReportVectorText(n1Increments),
      .bfsdReportVectorText(n2Increments)
    ))
  }

  if (.bfsdReportConstantIncrements(n1Increments))
    return(gettextf("batches of %1$s", .bfdReportNumber(n1Increments[1])))

  return(gettextf("batch-size increments of %1$s", .bfsdReportVectorText(n1Increments)))
}

.bfsdReportExpectedSampleSizeText <- function(settings, result) {
  h1 <- .bfsdExpectedSampleSizes(settings, result, "h1")
  h0 <- .bfsdExpectedSampleSizes(settings, result, "h0")

  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "Under the H\u2081 design prior, the expected sample sizes were n\u2081 = %1$s (SD = %2$s) and n\u2082 = %3$s (SD = %4$s); under the H\u2080 design prior, the expected sample sizes were n\u2081 = %5$s (SD = %6$s) and n\u2082 = %7$s (SD = %8$s).",
      .bfdReportMomentNumber(h1[["n1"]][["mean"]]),
      .bfdReportMomentNumber(h1[["n1"]][["sd"]]),
      .bfdReportMomentNumber(h1[["n2"]][["mean"]]),
      .bfdReportMomentNumber(h1[["n2"]][["sd"]]),
      .bfdReportMomentNumber(h0[["n1"]][["mean"]]),
      .bfdReportMomentNumber(h0[["n1"]][["sd"]]),
      .bfdReportMomentNumber(h0[["n2"]][["mean"]]),
      .bfdReportMomentNumber(h0[["n2"]][["sd"]])
    ))
  }

  return(gettextf(
    "Under the H\u2081 design prior, the expected sample size was %1$s (SD = %2$s); under the H\u2080 design prior, the expected sample size was %3$s (SD = %4$s).",
    .bfdReportMomentNumber(h1[["n1"]][["mean"]]),
    .bfdReportMomentNumber(h1[["n1"]][["sd"]]),
    .bfdReportMomentNumber(h0[["n1"]][["mean"]]),
    .bfdReportMomentNumber(h0[["n1"]][["sd"]])
  ))
}

.bfsdReportConstantIncrements <- function(increments) {
  length(unique(increments)) == 1
}

.bfsdReportVectorText <- function(x) {
  if (length(x) <= 6)
    return(paste(vapply(x, .bfdReportNumber, character(1)), collapse = ", "))

  return(gettextf(
    "%1$s to %2$s",
    .bfdReportNumber(min(x, na.rm = TRUE)),
    .bfdReportNumber(max(x, na.rm = TRUE))
  ))
}

.bfsdRCode <- function(jaspResults, settings, result) {
  html <- .bfdCreateHtml(
    parent       = jaspResults,
    key          = "sequentialEvidenceRCode",
    title        = gettext("R Code"),
    position     = 15,
    dependencies = .bfsdRCodeDependencies
  )
  if (is.null(html))
    return()

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("R code could not be generated because the sequential design could not be computed.")
    return()
  }

  code <- try(.bfsdBfpwrCall(settings, result), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .bfdCleanError(code))
    return()
  }

  html[["text"]] <- .bfdCodeHtml(code)
}

.bfsdBfpwrCall <- function(settings, result = NULL) {
  calls <- vapply(settings[["planningTargets"]], function(target) {
    targetSettings <- .bfsdRCodeTargetSettings(settings, result, target)
    prefix <- if (target == "h1") "# Plan for evidence for H1" else "# Plan for evidence for H0"
    call <- if (targetSettings[["isZTest"]]) .bfsdZBfpwrCall(targetSettings, target) else .bfsdTBfpwrCall(targetSettings, target)
    paste(prefix, call, sep = "\n")
  }, character(1))

  return(paste(calls, collapse = "\n\n"))
}

.bfsdRCodeTargetSettings <- function(settings, result, target) {
  if (is.null(result))
    return(settings)

  return(.bfsdTargetSettings(settings, result, target))
}

.bfsdZBfpwrCall <- function(settings, target) {
  analysisPrior <- .bfsdZAnalysisPrior(settings)
  dpm           <- .bfsdDirectionalMean(settings, .bfsdSearchDesignMean(settings, target))

  args <- list(
    k1 = settings[["k1"]],
    k0 = settings[["k0"]],
    se = .bfsdStandardErrors(settings)
  )

  if (!.bfsdUsesStandardErrorOnly(settings))
    args[["n"]] <- settings[["n1Seq"]]

  args <- c(
    args,
    list(
      pm     = analysisPrior[["pm"]],
      psd    = analysisPrior[["psd"]],
      dpm    = dpm,
      dpsd   = .bfsdSearchDesignSd(settings, target),
      type   = analysisPrior[["type"]],
      strict = settings[["strictIntegration"]]
    ),
    .bfsdIntegrationArguments(settings)
  )

  return(.bfdFormatRCall("bfpwr::pbf01seq", args))
}

.bfsdTBfpwrCall <- function(settings, target) {
  args <- c(
    list(
      k1          = settings[["k1"]],
      k0          = settings[["k0"]],
      n           = settings[["n1Seq"]],
      n1          = settings[["n1Seq"]],
      n2          = settings[["n2Seq"]],
      plocation   = settings[["tPriorLocationRelative"]],
      pscale      = settings[["tPriorScale"]],
      pdf         = settings[["tPriorDf"]],
      dpm         = .bfsdSearchDesignMean(settings, target),
      dpsd        = .bfsdSearchDesignSd(settings, target),
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      strict      = settings[["strictIntegration"]],
      drange      = .bfsdDrange(settings)
    ),
    .bfsdIntegrationArguments(settings)
  )

  return(.bfdFormatRCall("bfpwr::ptbf01seq", args))
}

.bfsdStoppingProbabilitiesPlot <- function(jaspResults, settings, result) {
  for (spec in .bfsdUnderPlotSpecs(settings, "sequentialEvidenceStoppingProbabilities", gettext("Cumulative Decision Probabilities by Look"), 10)) {
    .bfsdStoppingProbabilitiesOutcomePlot(
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

.bfsdStoppingProbabilitiesOutcomePlot <- function(jaspResults, settings, result, key, title, position, under) {
  plot <- .bfdCreatePlot(
    parent       = jaspResults,
    key          = key,
    title        = title,
    position     = position,
    dependencies = .bfsdStoppingProbabilitiesPlotDependencies,
    width        = .bfdPlotWidth(settings),
    height       = 350
  )
  if (is.null(plot))
    return()

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute stopping probabilities: %1$s", .bfdCleanError(result)))
    return()
  }

  plotData <- .bfdCachedPlotData(
    jaspResults  = jaspResults,
    stateKey     = "sequentialEvidenceStoppingProbabilitiesPlotData",
    dependencies = .bfsdStoppingProbabilitiesPlotDataDependencies,
    dataKey      = "data",
    compute      = function() list(data = .bfsdStoppingProbabilityPlotData(settings, result))
  )
  if (jaspBase::isTryError(plotData)) {
    plot$setError(gettextf("Unable to compute stopping probabilities: %1$s", .bfdCleanError(plotData)))
    return()
  }

  plotResult <- try(.bfsdBuildStoppingProbabilitiesPlot(settings, result, under, plotData), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute stopping probabilities: %1$s", .bfdCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.bfsdUnderPlotSpecs <- function(settings, keyPrefix, title, position) {
  if (isTRUE(settings[["mergeH1H0Figures"]])) {
    return(list(list(
      key      = paste0(keyPrefix, "Plot"),
      title    = title,
      position = position,
      under    = NULL
    )))
  }

  unders <- .bfsdPlotUnders(settings)
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

.bfsdPlotUnders <- function(settings, under = NULL) {
  if (!is.null(under))
    return(under)

  return(c("h1", "h0"))
}

.bfsdDesignForUnder <- function(result, under) {
  if (under == "h0")
    return(result[["null"]])

  return(result[["design"]])
}

.bfsdBuildStoppingProbabilitiesPlot <- function(settings, result, under = NULL, plotData = NULL) {
  if (is.null(plotData))
    plotData <- list(data = .bfsdStoppingProbabilityPlotData(settings, result))

  data      <- .bfdFilterCurveData(plotData[["data"]], under)
  xLabel    <- .bfsdLookAxisLabel(settings)
  showUnder <- length(unique(data[["under"]])) > 1

  if (showUnder) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome, linetype = under)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::geom_point(size = 2) +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Outcome"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::geom_point(size = 2) +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Outcome"))
  }

  if (.bfsdUsesSampleSizeSearch(settings)) {
    targets <- .bfsdPlotTargets(settings, under)
    if (length(targets) > 0) {
      plot <- plot + .bfdTargetPowerLine(settings, targets)
    }
  }

  linetypeValues <- if (showUnder) .bfdUnderLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfsdPlotTargets <- function(settings, under = NULL) {
  if (is.null(under))
    return(settings[["planningTargets"]])

  return(intersect(settings[["planningTargets"]], under))
}

.bfsdStoppingProbabilityPlotData <- function(settings, result, under = NULL) {
  unders <- .bfsdPlotUnders(settings, under)
  rows   <- lapply(unders, function(under) {
    .bfsdStoppingProbabilityRows(
      settings = .bfsdBasisSettings(settings, result, under),
      design   = .bfsdBasisDesignForUnder(settings, result, under),
      under    = under
    )
  })

  return(do.call(rbind, rows))
}

.bfsdStoppingProbabilityRows <- function(settings, design, under) {
  h1Outcome <- if (under == "h1") gettext("Conclusive") else gettext("Misleading")
  h0Outcome <- if (under == "h0") gettext("Conclusive") else gettext("Misleading")

  data.frame(
    n           = rep(.bfsdLookAxisValues(settings), 3),
    probability = c(design[["cumpH1"]], design[["cumpInc"]], design[["cumpH0"]]),
    outcome     = rep(c(h1Outcome, gettext("Inconclusive"), h0Outcome), each = length(settings[["n1Seq"]])),
    under       = .bfdUnderLabel(under),
    stringsAsFactors = FALSE
  )
}

.bfsdStoppingBoundariesPlot <- function(jaspResults, settings, result) {
  .bfsdClearSplitStoppingBoundaryPlots(jaspResults)

  .bfsdStoppingBoundariesOutcomePlot(
    jaspResults = jaspResults,
    settings    = settings,
    result      = result,
    key         = "sequentialEvidenceStoppingBoundariesPlot",
    title       = gettext("Stopping Boundaries"),
    position    = 12
  )
}

.bfsdClearSplitStoppingBoundaryPlots <- function(jaspResults) {
  for (key in c("sequentialEvidenceStoppingBoundariesH1Plot", "sequentialEvidenceStoppingBoundariesH0Plot")) {
    if (!is.null(jaspResults[[key]]))
      jaspResults[[key]] <- NULL
  }
}

.bfsdStoppingBoundariesOutcomePlot <- function(jaspResults, settings, result, key, title, position) {
  plot <- .bfdCreatePlot(
    parent       = jaspResults,
    key          = key,
    title        = title,
    position     = position,
    dependencies = .bfsdStoppingBoundariesPlotDependencies,
    width        = .bfdPlotWidth(settings),
    height       = 350
  )
  if (is.null(plot))
    return()

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute stopping boundaries: %1$s", .bfdCleanError(result)))
    return()
  }

  plotData <- .bfdCachedPlotData(
    jaspResults  = jaspResults,
    stateKey     = "sequentialEvidenceStoppingBoundariesPlotData",
    dependencies = .bfsdStoppingBoundariesPlotDataDependencies,
    dataKey      = "data",
    compute      = function() list(data = .bfsdBoundaryPlotData(.bfsdCommonBasisSettings(settings, result), .bfsdCommonBasisDesignResult(result, settings)[["design"]]))
  )
  if (jaspBase::isTryError(plotData)) {
    plot$setError(gettextf("Unable to compute stopping boundaries: %1$s", .bfdCleanError(plotData)))
    return()
  }

  plotResult <- try(.bfsdBuildStoppingBoundariesPlot(settings, result, plotData), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute stopping boundaries: %1$s", .bfdCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.bfsdBuildStoppingBoundariesPlot <- function(settings, result, plotData = NULL) {
  data <- if (is.null(plotData)) {
    .bfsdBoundaryPlotData(.bfsdCommonBasisSettings(settings, result), .bfsdCommonBasisDesignResult(result, settings)[["design"]])
  } else {
    plotData[["data"]]
  }

  if (nrow(data) == 0)
    stop(gettext("No finite stopping boundaries are available for the current settings."))

  xLabel <- .bfsdLookAxisLabel(settings)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = criticalValue, color = target, linetype = boundary)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = xLabel, y = gettext("Critical value"), color = gettext("Target"), linetype = gettext("Boundary")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "#666666")

  return(.bfdApplyPlotTheme(plot, settings))
}

.bfsdBoundaryPlotData <- function(settings, design) {
  rows <- .bfsdBoundaryRows(settings, design)
  n    <- .bfsdLookAxisValues(settings)

  data <- rbind(
    data.frame(n = n, criticalValue = rows[["h1Lower"]], target = gettext("H\u2081 (BF\u2081\u2080)"), boundary = gettext("Lower"), stringsAsFactors = FALSE),
    data.frame(n = n, criticalValue = rows[["h1Upper"]], target = gettext("H\u2081 (BF\u2081\u2080)"), boundary = gettext("Upper"), stringsAsFactors = FALSE),
    data.frame(n = n, criticalValue = rows[["h0Lower"]], target = gettext("H\u2080 (BF\u2080\u2081)"), boundary = gettext("Lower"), stringsAsFactors = FALSE),
    data.frame(n = n, criticalValue = rows[["h0Upper"]], target = gettext("H\u2080 (BF\u2080\u2081)"), boundary = gettext("Upper"), stringsAsFactors = FALSE)
  )

  data <- data[is.finite(data[["criticalValue"]]), , drop = FALSE]

  return(data)
}

.bfsdLookAxisValues <- function(settings) {
  if (.bfsdUsesStandardErrorOnly(settings))
    return(seq_along(settings[["n1Seq"]]))

  if (settings[["isIndependentSamples"]])
    return(settings[["n1Seq"]])

  return(settings[["n1Seq"]])
}

.bfsdLookAxisLabel <- function(settings) {
  if (.bfsdUsesStandardErrorOnly(settings))
    return(gettext("Look"))

  if (settings[["isIndependentSamples"]])
    return(gettext("Sample size (group 1)"))

  return(gettext("Sample size"))
}

.bfsdPriorPlot <- function(jaspResults, settings) {
  .bfdPriorPlotContainer(
    jaspResults  = jaspResults,
    settings     = settings,
    key          = "sequentialEvidencePriorPlot",
    position     = 14,
    dependencies = .bfsdPriorPlotDependencies,
    stateKey     = "sequentialEvidencePriorPlotData",
    dataDependencies = .bfsdPriorPlotDataDependencies
  )
}
