BayesFactorSequentialDesign <- function(jaspResults, dataset, options) {
  settings    <- .bfsdPrepareSettings(options)
  computation <- .bfsdCachedComputation(jaspResults, settings)
  settings    <- computation[["settings"]]
  result      <- computation[["result"]]

  if (options[["summaryDesign"]])
    .bfsdResultsTable(jaspResults, settings, result)

  if (options[["summarySampleSize"]])
    .bfsdSampleSizeSummaryTable(jaspResults, settings, result)

  if (options[["summaryEvidence"]])
    .bfsdDesignOutcomeTable(jaspResults, settings, result)

  if (options[["stagewiseEvidence"]])
    .bfsdStagewiseTotalTable(jaspResults, settings, result)

  if (options[["stagewiseIncrementalEvidence"]])
    .bfsdStagewiseIncrementalTable(jaspResults, settings, result)

  if (options[["stagewiseStoppingBoundaries"]])
    .bfsdBoundariesTable(jaspResults, settings, result)

  if (options[["summarySpecification"]])
    .bfsdPriorsTable(jaspResults, settings)

  if (.bfdObservedAnalysisReady(dataset, options, settings)) {
    .bfdObservedAnalysisTable(
      jaspResults  = jaspResults,
      dataset      = dataset,
      options      = options,
      settings     = settings,
      key          = "sequentialEvidenceObservedAnalysis",
      position     = 8,
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

  if (options[["stoppingProbabilities"]])
    .bfsdStoppingProbabilitiesPlot(jaspResults, settings, result)

  if (options[["stoppingBoundaries"]])
    .bfsdStoppingBoundariesPlot(jaspResults, settings, result)

  if (.bfdPriorPlotRequested(options))
    .bfsdPriorPlot(jaspResults, settings)

  return()
}

.bfsdComputationDependencies <- c(
  "statisticalTest", "calculationTarget", "conclusiveEvidenceThresholdH1", "conclusiveEvidenceThresholdH0",
  "probabilityOfConclusiveEvidenceUnderH1", "probabilityOfConclusiveEvidenceUnderH0", "analysisPriorDirection", "lookScheduleType",
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
.bfsdSummaryDesignDependencies                 <- c(.bfsdComputationDependencies, "summaryDesign")
.bfsdSummarySampleSizeDependencies             <- c(.bfsdComputationDependencies, "summarySampleSize")
.bfsdSummaryEvidenceDependencies               <- c(.bfsdComputationDependencies, "summaryEvidence")
.bfsdSummarySpecificationDependencies          <- c(.bfsdComputationDependencies, "summarySpecification")
.bfsdStagewiseEvidenceDependencies             <- c(.bfsdComputationDependencies, "stagewiseEvidence")
.bfsdStagewiseIncrementalEvidenceDependencies  <- c(.bfsdComputationDependencies, "stagewiseIncrementalEvidence")
.bfsdStagewiseStoppingBoundariesDependencies   <- c(.bfsdComputationDependencies, "stagewiseStoppingBoundaries")

.bfsdStoppingProbabilitiesPlotDependencies <- c(
  .bfsdComputationDependencies, "stoppingProbabilities",
  "mergeH1AndH0Figures"
)
.bfsdStoppingProbabilitiesPlotDataDependencies <- .bfsdComputationDependencies
.bfsdStoppingBoundariesPlotDependencies <- c(
  .bfsdComputationDependencies, "stoppingBoundaries"
)
.bfsdStoppingBoundariesPlotDataDependencies <- .bfsdComputationDependencies
.bfsdPriorPlotDependencies <- c(
  .bfsdComputationDependencies, "priorDistributionDesign", "priorDistributionAnalysis", "priorDistributionMergeFigures",
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
    mergeH1H0Figures     = options[["mergeH1AndH0Figures"]],
    priorPlotDesign      = options[["priorDistributionDesign"]],
    priorPlotAnalysis    = options[["priorDistributionAnalysis"]],
    priorPlotMerge       = options[["priorDistributionMergeFigures"]],
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

  upper <- .bfsdEvaluateTargetMaximumN(settings, target, maximumN, cache)
  if (!is.finite(upper[["criterion"]])) {
    stop(gettextf(
      "The upper search bound does not produce a valid sequential design: %1$s",
      upper[["error"]]
    ))
  }

  if (upper[["criterion"]] < 0)
    return(.bfsdTargetMaximumSampleSizeLimit(upper))

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

.bfsdDisplayMaximumFootnote <- function(result) {
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

  displayMaximumFootnote <- .bfsdDisplayMaximumFootnote(result)
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
  computed <- gettext("Computed")
  userDefined <- gettext("User Defined")

  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "target", title = gettext("Planned Target"), type = "string")

  if (.bfsdUsesSampleSizeSearch(settings)) {
    table$addColumnInfo(name = "targetProbability", title = gettext("Target Pr(Conclusive Evidence)"), type = "number", overtitle = userDefined)
  }

  table$addColumnInfo(name = "actualProbability", title = gettext("Pr(Conclusive Evidence)"), type = "number", overtitle = computed)

  if (.bfsdUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "finalSE", title = gettext("Final SE"), type = "number", overtitle = computed)
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "expectedN1", title = "N\u2081", type = "number", overtitle = gettext("Expected Sample Size"))
    table$addColumnInfo(name = "expectedN2", title = "N\u2082", type = "number", overtitle = gettext("Expected Sample Size"))
  } else {
    table$addColumnInfo(name = "expectedN", title = "N", type = "number", overtitle = gettext("Expected Sample Size"))
  }

  table$addColumnInfo(name = "threshold", title = gettext("BF threshold"), type = "number", overtitle = userDefined)
}

.bfsdResultsRow <- function(settings, result) {
  targetRows <- .bfsdMainTargetRows(settings, result)
  row <- data.frame(
    designPrior      = vapply(targetRows[["target"]], .bfsdTargetDesignPriorLabel, character(1)),
    target           = vapply(targetRows[["target"]], .bfdTargetLabel, character(1)),
    actualProbability = targetRows[["actualProbability"]],
    threshold        = vapply(targetRows[["target"]], function(target) .bfdThreshold(settings, target), numeric(1)),
    stringsAsFactors = FALSE
  )

  if (.bfsdUsesSampleSizeSearch(settings))
    row[["targetProbability"]] <- targetRows[["targetProbability"]]

  if (.bfsdUsesStandardErrorOnly(settings)) {
    row[["finalSE"]] <- utils::tail(.bfsdStandardErrors(settings), 1)
  } else {
    for (i in seq_len(nrow(targetRows))) {
      expected <- .bfsdExpectedSampleSizes(settings, result, targetRows[["target"]][i])
      if (settings[["isIndependentSamples"]]) {
        row[["expectedN1"]][i] <- expected[["n1"]][["mean"]]
        row[["expectedN2"]][i] <- expected[["n2"]][["mean"]]
      } else {
        row[["expectedN"]][i] <- expected[["n1"]][["mean"]]
      }
    }
  }

  if (.bfsdUsesSampleSizeSearch(settings))
    row <- .bfsdBlankUnreachedRows(row, result[["solver"]][["targetResults"]])

  columnOrder <- c(
    "designPrior", "target",
    if (.bfsdUsesSampleSizeSearch(settings)) "targetProbability",
    "actualProbability",
    if (.bfsdUsesStandardErrorOnly(settings)) "finalSE",
    if (!.bfsdUsesStandardErrorOnly(settings) && settings[["isIndependentSamples"]]) c("expectedN1", "expectedN2"),
    if (!.bfsdUsesStandardErrorOnly(settings) && !settings[["isIndependentSamples"]]) "expectedN",
    "threshold"
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
    title        = gettext("Sample Size Summary"),
    position     = 2,
    dependencies = .bfsdSummarySampleSizeDependencies
  )
  if (is.null(table))
    return()

  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  if (.bfsdUsesSampleSizeSearch(settings))
    table$addColumnInfo(name = "target", title = gettext("Planned Target"), type = "string")
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

  displayMaximumFootnote <- .bfsdDisplayMaximumFootnote(result)
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
      designPrior = vapply(targetRows[["target"]], .bfsdTargetDesignPriorLabel, character(1)),
      target      = vapply(targetRows[["target"]], .bfdTargetLabel, character(1)),
      looks       = length(settings[["n1Seq"]]),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- data.frame(
      designPrior = vapply(targetRows[["target"]], .bfsdTargetDesignPriorLabel, character(1)),
      looks       = length(settings[["n1Seq"]]),
      stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(targetRows))) {
    target   <- targetRows[["target"]][i]
    expected <- .bfsdExpectedSampleSizes(settings, result, target)
    schedule <- .bfsdTargetSampleSizeSchedule(settings, result, target)
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
    rows <- .bfsdBlankUnreachedRows(rows, result[["solver"]][["targetResults"]])

  row.names(rows) <- NULL

  return(rows)
}

.bfsdMainTargetRows <- function(settings, result) {
  if (.bfsdUsesSampleSizeSearch(settings))
    return(result[["solver"]][["targetResults"]])

  data.frame(
    target            = c("h1", "h0"),
    actualProbability = c(
      .bfsdFinalTargetProbability(result[["design"]], "h1"),
      .bfsdFinalTargetProbability(result[["null"]], "h0")
    ),
    stringsAsFactors = FALSE
  )
}

.bfsdExpectedSampleSizes <- function(settings, result, target) {
  schedule <- .bfsdTargetSampleSizeSchedule(settings, result, target)
  design   <- .bfsdTargetDesign(result, target)

  return(list(
    n1 = .bfsdExpectedSampleSize(design, schedule[["n1Seq"]]),
    n2 = .bfsdExpectedSampleSize(design, schedule[["n2Seq"]])
  ))
}

.bfsdTargetDesignPriorLabel <- function(target) {
  if (target == "h1")
    return(gettext("Under H\u2081 Design Prior"))

  return(gettext("Under H\u2080 Design Prior"))
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
  if (is.null(targetRows[["reached"]]) || all(targetRows[["reached"]]))
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

  row[!targetRows[["reached"]], blankColumns] <- NA
  return(row)
}

.bfsdExpectedSampleSize <- function(design, n) {
  stageStop <- diff(c(0, design[["cumpH1"]] + design[["cumpH0"]]))
  expected  <- sum(stageStop * n) + (1 - sum(stageStop)) * max(n)
  expected2 <- sum(stageStop * n^2) + (1 - sum(stageStop)) * max(n)^2
  variance  <- max(0, expected2 - expected^2)

  return(c(mean = expected, sd = sqrt(variance)))
}

.bfsdDesignOutcomeTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceDesignOutcome",
    title        = gettext("Design Evidence"),
    position     = 3,
    dependencies = .bfsdSummaryEvidenceDependencies
  )
  if (is.null(table))
    return()

  .bfdAddDesignOutcomeColumns(table)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute design evidence: %1$s", .bfdCleanError(result)))
    return()
  }

  rows <- try(.bfsdDesignOutcomeRows(result), silent = TRUE)
  if (jaspBase::isTryError(rows)) {
    table$setError(gettextf("Unable to compute design evidence: %1$s", .bfdCleanError(rows)))
    return()
  }

  table$setData(rows)
  table$addFootnote(gettext("Probabilities are cumulative through the final look. Rows use the corresponding design prior under H\u2081 or H\u2080."))
}

.bfsdDesignOutcomeRows <- function(result) {
  h1Outcome <- .bfsdFinalOutcome(result[["design"]])
  h0Outcome <- .bfsdFinalOutcome(result[["null"]])

  return(.bfdDesignOutcomeRowsFromOutcomes(h1Outcome, h0Outcome))
}

.bfsdFinalOutcome <- function(design) {
  return(c(
    null        = .bfdClampProbability(utils::tail(design[["cumpH0"]], 1)),
    undecided   = .bfdClampProbability(utils::tail(design[["cumpInc"]], 1)),
    alternative = .bfdClampProbability(utils::tail(design[["cumpH1"]], 1))
  ))
}

.bfsdStagewiseTotalTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceStagewiseTotal",
    title        = gettext("Total Stagewise Evidence"),
    position     = 4,
    dependencies = .bfsdStagewiseEvidenceDependencies
  )
  if (is.null(table))
    return()

  .bfsdAddLookColumns(table, settings)

  h1Overtitle <- gettext("Under H\u2081 Design Prior")
  h0Overtitle <- gettext("Under H\u2080 Design Prior")
  table$addColumnInfo(name = "h1Alternative", title = gettext("Alternative"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1Undecided",   title = gettext("Inconclusive"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1Null",        title = gettext("Null"),        type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h0Alternative", title = gettext("Alternative"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0Undecided",   title = gettext("Inconclusive"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0Null",        title = gettext("Null"),        type = "number", overtitle = h0Overtitle)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute total stagewise evidence: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.bfsdStagewiseTotalRows(settings, result))
}

.bfsdStagewiseIncrementalTable <- function(jaspResults, settings, result) {
  table <- .bfdCreateTable(
    parent       = jaspResults,
    key          = "sequentialEvidenceStagewiseIncremental",
    title        = gettext("Incremental Stagewise Stops"),
    position     = 5,
    dependencies = .bfsdStagewiseIncrementalEvidenceDependencies
  )
  if (is.null(table))
    return()

  .bfsdAddLookColumns(table, settings)

  h1Overtitle <- gettext("Under H\u2081 Design Prior")
  h0Overtitle <- gettext("Under H\u2080 Design Prior")
  table$addColumnInfo(name = "h1AlternativeStop", title = gettext("Alternative"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1NullStop",        title = gettext("Null"),        type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1AnyStop",         title = gettext("Any"),         type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h0AlternativeStop", title = gettext("Alternative"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0NullStop",        title = gettext("Null"),        type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0AnyStop",         title = gettext("Any"),         type = "number", overtitle = h0Overtitle)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute incremental stagewise stops: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.bfsdStagewiseIncrementalRows(settings, result))
}

.bfsdAddLookColumns <- function(table, settings) {
  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  if (.bfsdUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "standardError", title = gettext("SE"), type = "number")
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer")
    table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer")
  } else {
    table$addColumnInfo(name = "n", title = "N", type = "integer")
  }
}

.bfsdStagewiseTotalRows <- function(settings, result) {
  rows <- .bfsdLookRows(settings)
  rows[["h1Alternative"]] <- result[["design"]][["cumpH1"]]
  rows[["h1Undecided"]]   <- result[["design"]][["cumpInc"]]
  rows[["h1Null"]]        <- result[["design"]][["cumpH0"]]
  rows[["h0Alternative"]] <- result[["null"]][["cumpH1"]]
  rows[["h0Undecided"]]   <- result[["null"]][["cumpInc"]]
  rows[["h0Null"]]        <- result[["null"]][["cumpH0"]]

  return(rows)
}

.bfsdStagewiseIncrementalRows <- function(settings, result) {
  h1AlternativeStop <- diff(c(0, result[["design"]][["cumpH1"]]))
  h1NullStop        <- diff(c(0, result[["design"]][["cumpH0"]]))
  h0AlternativeStop <- diff(c(0, result[["null"]][["cumpH1"]]))
  h0NullStop        <- diff(c(0, result[["null"]][["cumpH0"]]))

  rows <- .bfsdLookRows(settings)
  rows[["h1AlternativeStop"]] <- h1AlternativeStop
  rows[["h1NullStop"]]        <- h1NullStop
  rows[["h1AnyStop"]]         <- h1AlternativeStop + h1NullStop
  rows[["h0AlternativeStop"]] <- h0AlternativeStop
  rows[["h0NullStop"]]        <- h0NullStop
  rows[["h0AnyStop"]]         <- h0AlternativeStop + h0NullStop

  return(rows)
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
    position     = 6,
    dependencies = .bfsdStagewiseStoppingBoundariesDependencies
  )
  if (is.null(table))
    return()

  .bfsdAddLookColumns(table, settings)

  table$addColumnInfo(name = "h1Lower", title = gettext("Lower"), type = "number", overtitle = gettext("BF\u2081\u2080 target"))
  table$addColumnInfo(name = "h1Upper", title = gettext("Upper"), type = "number", overtitle = gettext("BF\u2081\u2080 target"))
  table$addColumnInfo(name = "h0Lower", title = gettext("Lower"), type = "number", overtitle = gettext("BF\u2080\u2081 target"))
  table$addColumnInfo(name = "h0Upper", title = gettext("Upper"), type = "number", overtitle = gettext("BF\u2080\u2081 target"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stopping boundaries: %1$s", .bfdCleanError(result)))
    return()
  }

  rows <- .bfsdBoundaryRows(settings, result[["design"]])
  table$setData(rows)

  if (.bfsdHasEmptyBoundaryCells(rows))
    table$addFootnote(gettext("Empty boundary cells indicate that no finite test statistic value reaches the corresponding BF target for that look and tail under the current design settings."))
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

.bfsdHasEmptyBoundaryCells <- function(rows) {
  boundaryColumns <- intersect(c("h1Lower", "h1Upper", "h0Lower", "h0Upper"), names(rows))
  if (length(boundaryColumns) == 0)
    return(FALSE)

  return(any(is.na(rows[, boundaryColumns, drop = FALSE])))
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
    position     = 7,
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

  outcome <- .bfsdFinalOutcome(result[["design"]])
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
    position     = 13,
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
        h1Outcome = .bfsdFinalOutcome(result[["design"]]),
        h0Outcome = .bfsdFinalOutcome(result[["null"]])
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
  for (spec in .bfsdUnderPlotSpecs(settings, "sequentialEvidenceStoppingProbabilities", gettext("Stopping Probabilities"), 10)) {
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
    width        = 735,
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
      ggplot2::scale_linetype_discrete(labels = .bfdPlotmathLabels) +
      ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Evidence"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::geom_point(size = 2) +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Evidence"))
  }

  if (.bfsdUsesSampleSizeSearch(settings)) {
    targets <- .bfsdPlotTargets(settings, under)
    if (length(targets) > 0) {
      plot <- plot + .bfdTargetPowerLine(settings, targets)
    }
  }

  return(.pwrApplyPlotTheme(plot))
}

.bfsdPlotTargets <- function(settings, under = NULL) {
  if (is.null(under))
    return(settings[["planningTargets"]])

  return(intersect(settings[["planningTargets"]], under))
}

.bfsdStoppingProbabilityPlotData <- function(settings, result, under = NULL) {
  unders <- .bfsdPlotUnders(settings, under)
  rows   <- lapply(unders, function(under) {
    .bfsdStoppingProbabilityRows(settings, .bfsdDesignForUnder(result, under), under)
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
    width        = 735,
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
    compute      = function() list(data = .bfsdBoundaryPlotData(settings, result[["design"]]))
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
  data <- if (is.null(plotData)) .bfsdBoundaryPlotData(settings, result[["design"]]) else plotData[["data"]]

  if (nrow(data) == 0)
    stop(gettext("No finite stopping boundaries are available for the current settings."))

  xLabel <- .bfsdLookAxisLabel(settings)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = criticalValue, color = target, linetype = boundary)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_discrete(labels = .bfdPlotmathLabels) +
    ggplot2::labs(x = xLabel, y = gettext("Critical value"), color = gettext("Target"), linetype = gettext("Boundary")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "#666666")

  return(.pwrApplyPlotTheme(plot))
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
