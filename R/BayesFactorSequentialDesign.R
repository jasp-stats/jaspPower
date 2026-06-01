BayesFactorSequentialDesign <- function(jaspResults, dataset, options) {
  settings    <- .bfsdPrepareSettings(options)
  tables      <- .bfsdInitializeOutputTables(jaspResults, options, settings)
  if (.bfdPriorPlotRequested(options))
    .bfsdPriorPlot(jaspResults, settings)

  computation <- .bfsdCachedComputation(jaspResults, settings)
  settings    <- computation[["settings"]]
  result      <- computation[["result"]]

  .bfsdPopulateInitializedOutputTables(tables, settings, result)

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

  if (options[["generateReport"]])
    .bfsdReport(jaspResults, settings, result)

  if (options[["generateRCode"]])
    .bfsdRCode(jaspResults, settings, result)

  if (options[["cumulativeDecisionProbabilitiesPlot"]])
    .bfsdStoppingProbabilitiesPlot(jaspResults, settings, result)

  if (options[["stoppingBoundariesPlot"]])
    .bfsdStoppingBoundariesPlot(jaspResults, settings, result)

  return()
}

.bfsdComputationDependencies <- c(
  "statisticalTest", "calculationTarget", "conclusiveEvidenceThresholdH1", "conclusiveEvidenceThresholdH0",
  "probabilityOfConclusiveEvidenceUnderH1", "probabilityOfConclusiveEvidenceUnderH0", "designSampleSizeBasis", "analysisPriorDirection", "lookScheduleType",
  "numberOfLooks", "initialSampleSize", "sampleSizeIncreasePerLook", "maximumSampleSize", "sampleSizeSchedule",
  "sampleSizeScheduleGroup2", "sampleSizeAllocationRatio", "firstInformationFraction",
  "informationFractionSchedule", "lowerSearchBoundForMaximumSampleSize", "upperSearchBoundForMaximumSampleSize",
  "sampleSizeSearchStrategy",
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

.bfsdReportDependencies <- c(.bfsdComputationDependencies, "generateReport", "generateReportLatexFormattedOutput")
.bfsdRCodeDependencies  <- c(.bfsdComputationDependencies, "generateRCode")
.bfsdSummaryDesignDependencies                 <- c(.bfsdComputationDependencies, "designSummary", "explanatoryText")
.bfsdSummarySampleSizeDependencies             <- c(.bfsdComputationDependencies, "sampleSizeSummary", "explanatoryText")
.bfsdSummaryEvidenceDependencies               <- c(.bfsdComputationDependencies, "decisionProbabilities", "explanatoryText")
.bfsdSummarySpecificationDependencies          <- c(.bfsdComputationDependencies, "designSpecification", "explanatoryText")
.bfsdStagewiseEvidenceDependencies             <- c(.bfsdComputationDependencies, "cumulativeDecisionProbabilities", "explanatoryText")
.bfsdStagewiseIncrementalEvidenceDependencies  <- c(.bfsdComputationDependencies, "incrementalDecisionProbabilities", "explanatoryText")
.bfsdStagewiseStoppingBoundariesDependencies   <- c(.bfsdComputationDependencies, "stoppingBoundariesTable", "explanatoryText")

.bfsdStoppingProbabilitiesPlotDependencies <- c(
  .bfsdComputationDependencies, "cumulativeDecisionProbabilitiesPlot",
  "combineH1H0Figures", "legendPosition", "colorPalette", "explanatoryText"
)
.bfsdStoppingProbabilitiesPlotDataDependencies <- .bfsdComputationDependencies
.bfsdStoppingBoundariesPlotDependencies <- c(
  .bfsdComputationDependencies, "stoppingBoundariesPlot", "legendPosition", "colorPalette", "explanatoryText"
)
.bfsdStoppingBoundariesPlotDataDependencies <- .bfsdComputationDependencies
.bfsdPriorPlotDependencies <- c(
  .bfsdComputationDependencies, "designPriorDistributionFigure", "analysisPriorDistributionFigure", "combineDesignAnalysisPriorFigures",
  "curvePoints", "legendPosition", "colorPalette", "explanatoryText"
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
  "reportLatex", "legendPosition", "colorPalette", "explanatoryText"
)

.bfsdInitializeOutputTables <- function(jaspResults, options, settings) {
  tables <- list(
    results = if (options[["designSummary"]]) {
      .bfdCreateTable(
        parent       = jaspResults,
        key          = "sequentialEvidenceResults",
        title        = gettext("Bayes Factor Sequential Design"),
        position     = 1,
        dependencies = .bfsdSummaryDesignDependencies
      )
    },
    sampleSizeSummary = if (options[["sampleSizeSummary"]] && !.bfsdUsesStandardErrorOnly(settings)) {
      .bfdCreateTable(
        parent       = jaspResults,
        key          = "sequentialEvidenceSampleSizeSummary",
        title        = gettext("Sample Size Operating Characteristics"),
        position     = 2,
        dependencies = .bfsdSummarySampleSizeDependencies
      )
    },
    designOutcome = if (options[["decisionProbabilities"]]) {
      .bfdCreateTable(
        parent       = jaspResults,
        key          = "sequentialEvidenceDesignOutcome",
        title        = gettext("Bayes Factor Decision Probabilities"),
        position     = 3,
        dependencies = .bfsdSummaryEvidenceDependencies
      )
    },
    priors = if (options[["designSpecification"]]) {
      .bfdCreateTable(
        parent       = jaspResults,
        key          = "sequentialEvidencePriors",
        title        = gettext("Design Specification"),
        position     = 4,
        dependencies = .bfsdSummarySpecificationDependencies
      )
    },
    stagewiseTotal = if (options[["cumulativeDecisionProbabilities"]]) {
      .bfdCreateTable(
        parent       = jaspResults,
        key          = "sequentialEvidenceStagewiseTotal",
        title        = gettext("Cumulative Decision Probabilities by Look"),
        position     = 5,
        dependencies = .bfsdStagewiseEvidenceDependencies
      )
    },
    stagewiseIncremental = if (options[["incrementalDecisionProbabilities"]]) {
      .bfdCreateTable(
        parent       = jaspResults,
        key          = "sequentialEvidenceStagewiseIncremental",
        title        = gettext("New Stop Probabilities by Look"),
        position     = 6,
        dependencies = .bfsdStagewiseIncrementalEvidenceDependencies
      )
    },
    boundaries = if (options[["stoppingBoundariesTable"]]) {
      .bfdCreateTable(
        parent       = jaspResults,
        key          = "sequentialEvidenceBoundaries",
        title        = gettext("Stopping Boundaries"),
        position     = 7,
        dependencies = .bfsdStagewiseStoppingBoundariesDependencies
      )
    }
  )

  if (!is.null(tables[["results"]]))
    .bfsdInitializeResultsTable(tables[["results"]], settings)

  if (!is.null(tables[["sampleSizeSummary"]]))
    .bfsdInitializeSampleSizeSummaryTable(tables[["sampleSizeSummary"]], settings)

  if (!is.null(tables[["designOutcome"]]))
    .bfsdInitializeDesignOutcomeTable(tables[["designOutcome"]], settings)

  if (!is.null(tables[["priors"]]))
    .bfsdPopulatePriorsTable(tables[["priors"]], settings)

  if (!is.null(tables[["stagewiseTotal"]]))
    .bfsdInitializeStagewiseTotalTable(tables[["stagewiseTotal"]], settings)

  if (!is.null(tables[["stagewiseIncremental"]]))
    .bfsdInitializeStagewiseIncrementalTable(tables[["stagewiseIncremental"]], settings)

  if (!is.null(tables[["boundaries"]]))
    .bfsdInitializeBoundariesTable(tables[["boundaries"]], settings)

  return(tables)
}

.bfsdInitializeResultsTable <- function(table, settings) {
  .bfsdAddResultsColumns(table, settings)
  table$setData(.bfsdEmptyResultsRow(settings))
  .bfsdAddResultsTableInitializationFootnotes(table, settings)

  return(invisible(TRUE))
}

.bfsdInitializeSampleSizeSummaryTable <- function(table, settings) {
  .bfsdAddSampleSizeSummaryColumns(table, settings)
  table$setData(.bfsdEmptySampleSizeSummaryRow(settings))

  return(invisible(TRUE))
}

.bfsdInitializeDesignOutcomeTable <- function(table, settings) {
  .bfsdAddDesignOutcomeColumns(table)
  table$setData(.bfdEmptyDesignOutcomeRow())

  return(invisible(TRUE))
}

.bfsdInitializeStagewiseTotalTable <- function(table, settings) {
  .bfsdAddStagewiseTotalColumns(table, settings)
  table$setData(.bfsdEmptyStagewiseRow(settings, "total"))

  return(invisible(TRUE))
}

.bfsdInitializeStagewiseIncrementalTable <- function(table, settings) {
  .bfsdAddStagewiseIncrementalColumns(table, settings)
  table$setData(.bfsdEmptyStagewiseRow(settings, "incremental"))

  return(invisible(TRUE))
}

.bfsdInitializeBoundariesTable <- function(table, settings) {
  .bfsdAddBoundariesColumns(table, settings)
  table$setData(.bfsdEmptyBoundariesRow(settings))

  return(invisible(TRUE))
}

.bfsdAddResultsTableInitializationFootnotes <- function(table, settings) {
  twoSidedWarningShown <- .bfsdAddTwoSidedSequentialWarning(table, settings)

  if (.bfsdUsesAdaptiveSampleSizeSearch(settings)) {
    table$addFootnote(gettext(
      "Fast adaptive maximum sample-size search is enabled. It is not guaranteed to find the smallest maximum sample size when discrete interim looks make the power curve non-monotone. Use \"Sample-size search: Exhaustive\" in the Advanced section to obtain the smallest maximum sample size."
    ))
  }

  if (!isTRUE(twoSidedWarningShown))
    table$addFootnote(.bfsdExactIntegrationFootnote(settings))

  return(invisible(TRUE))
}

.bfsdExactIntegrationFootnote <- function(settings) {
  if (settings[["strictIntegration"]]) {
    return(gettext(
      "Exact integration over all regions is enabled. It can be slow for designs with many looks."
    ))
  } else {
    return(gettext(
      "Exact integration over all regions is disabled. Use \"Exact integration over all regions\" in the Advanced section to obtain exact sequential-design calculations."
    ))
  }
}

.bfsdAddTwoSidedSequentialWarning <- function(table, settings) {
  if (!.bfsdShowsTwoSidedSequentialWarning(settings))
    return(invisible(FALSE))

  table$addFootnote(
    gettext(
      "Two-sided sequential sample-size and power calculations can take a long time. To speed up exploratory calculations set \"Exact integration over all regions\" to FALSE."
    ),
    symbol = gettext("Warning:")
  )

  return(invisible(TRUE))
}

.bfsdShowsTwoSidedSequentialWarning <- function(settings) {
  return(
    identical(settings[["alternative"]], "two.sided") &&
      isTRUE(settings[["strictIntegration"]]) &&
      settings[["calculation"]] %in% c("sampleSize", "evidenceProbability")
  )
}

.bfsdPopulateInitializedOutputTables <- function(tables, settings, result) {
  if (!is.null(tables[["results"]]))
    .bfsdPopulateResultsTable(tables[["results"]], settings, result, columnsReady = TRUE)

  if (!is.null(tables[["sampleSizeSummary"]]))
    .bfsdPopulateSampleSizeSummaryTable(
      tables[["sampleSizeSummary"]],
      settings,
      result,
      columnsReady                 = TRUE,
      includeTargetStatusFootnotes = is.null(tables[["results"]])
    )

  if (!is.null(tables[["designOutcome"]]))
    .bfsdPopulateDesignOutcomeTable(tables[["designOutcome"]], settings, result, columnsReady = TRUE)

  if (!is.null(tables[["stagewiseTotal"]]))
    .bfsdPopulateStagewiseTotalTable(tables[["stagewiseTotal"]], settings, result, columnsReady = TRUE)

  if (!is.null(tables[["stagewiseIncremental"]]))
    .bfsdPopulateStagewiseIncrementalTable(tables[["stagewiseIncremental"]], settings, result, columnsReady = TRUE)

  if (!is.null(tables[["boundaries"]]))
    .bfsdPopulateBoundariesTable(tables[["boundaries"]], settings, result, columnsReady = TRUE)
}

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
    sampleSizeSearchStrategy = .bfsdSampleSizeSearchStrategy(options[["sampleSizeSearchStrategy"]]),
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
    colorPalette         = options[["colorPalette"]],
    explanatoryText      = options[["explanatoryText"]]
  )

  settings <- .bfsdAddContinuousSettings(settings, options)

  return(settings)
}

.bfsdAddContinuousSettings <- function(settings, options) {
  settings[["standardDeviation"]] <- options[["knownStandardDeviation"]]
  settings[["generalZParameterization"]] <- options[["generalZParameterization"]]
  settings[["unitInformationSd"]]        <- options[["unitInformationSd"]]
  settings[["standardErrorSchedule"]]    <- options[["standardErrorSchedule"]]
  settings[["alternative"]]       <- .bfdAnalysisPriorAlternative(options[["analysisPriorDirection"]])

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

.bfsdUsesSampleSizeSearchStrategy <- function(settings) {
  return(
    .bfsdUsesSampleSizeSearch(settings) &&
      !identical(settings[["lookScheduleMode"]], "increase")
  )
}

.bfsdSampleSizeSearchStrategy <- function(strategy) {
  if (is.null(strategy) || length(strategy) != 1 || is.na(strategy) || strategy == "")
    return("adaptive")

  .bfdRequireKnownOption(gettext("sample-size search strategy"), strategy, c("adaptive", "exhaustive"))
  return(strategy)
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

  nrange   <- c(minimumN, maximumN)
  progress <- .bfsdMaximumSampleSizeSearchProgress(settings, nrange)
  targetComputations <- lapply(settings[["planningTargets"]], function(target) {
    .bfsdFindMaximumSampleSizeForTarget(settings, target, nrange, .bfsdProgressCallback(progress))
  })
  .bfsdFinishProgress(progress)
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

.bfsdMaximumSampleSizeSearchProgress <- function(settings, nrange) {
  if (!jaspBase::jaspResultsCalledFromJasp())
    return(NULL)

  expectedTicks <- .bfsdMaximumSampleSizeSearchProgressTicks(settings, nrange)
  if (!is.finite(expectedTicks) || expectedTicks < 1)
    return(NULL)

  startProgressbar(expectedTicks, gettext("Searching maximum sample size"))

  targets        <- settings[["planningTargets"]]
  ticks          <- 0L
  targetTicks    <- stats::setNames(integer(length(targets)), targets)
  ticksPerTarget <- .bfsdMaximumSampleSizeSearchProgressTicksForTarget(nrange)

  tickTo <- function(totalTicks) {
    totalTicks <- min(expectedTicks, max(0L, as.integer(ceiling(totalTicks))))
    tickCount  <- totalTicks - ticks
    if (tickCount <= 0L)
      return(invisible(FALSE))

    for (i in seq_len(tickCount))
      progressbarTick()

    ticks <<- ticks + tickCount
    invisible(TRUE)
  }

  callback <- function(info) {
    target <- info[["target"]]
    targetIndex <- match(target, targets)
    if (is.na(targetIndex))
      targetIndex <- 1L

    previousTargetTicks <- targetTicks[[targetIndex]]
    currentTargetTicks  <- .bfsdProgressTicksFromSearchInfo(
      info               = info,
      nrange             = nrange,
      previousTargetTicks = previousTargetTicks,
      maximumTargetTicks = ticksPerTarget
    )

    targetTicks[[targetIndex]] <<- currentTargetTicks
    tickTo((targetIndex - 1L) * ticksPerTarget + currentTargetTicks)
  }

  list(
    callback = callback,
    finish   = function() tickTo(expectedTicks)
  )
}

.bfsdMaximumSampleSizeSearchProgressTicks <- function(settings, nrange) {
  targets <- length(settings[["planningTargets"]])
  maximumEvaluationsPerTarget <- .bfsdMaximumSampleSizeSearchProgressTicksForTarget(nrange)

  return(targets * maximumEvaluationsPerTarget)
}

.bfsdMaximumSampleSizeSearchProgressTicksForTarget <- function(nrange) {
  return(ceiling(nrange[2]) - ceiling(nrange[1]) + 1L)
}

.bfsdProgressTicksFromSearchInfo <- function(info, nrange, previousTargetTicks, maximumTargetTicks) {
  ticks <- previousTargetTicks + 1L

  n <- info[["n"]]
  if (length(n) == 1 && is.finite(n)) {
    infoRange <- info[["nrange"]]
    if (is.null(infoRange) || length(infoRange) != 2 || !all(is.finite(infoRange)))
      infoRange <- nrange

    lower <- ceiling(infoRange[1])
    ticks <- max(ticks, ceiling(n) - lower + 1L)
  }

  return(min(maximumTargetTicks, max(1L, ticks)))
}

.bfsdProgressCallback <- function(progress) {
  if (is.null(progress))
    return(NULL)

  return(progress[["callback"]])
}

.bfsdFinishProgress <- function(progress) {
  if (is.null(progress))
    return(invisible(FALSE))

  progress[["finish"]]()
  invisible(TRUE)
}

.bfsdFindMaximumSampleSizeForTarget <- function(settings, target, nrange, progress = NULL) {
  search         <- .bfsdRunMaximumSampleSizeSearch(settings, target, nrange, progress)
  maximumN       <- .bfsdSearchMaximumN(search)
  targetSettings <- .bfsdSettingsForSequentialSearch(settings, search, maximumN)
  designResult   <- .bfsdComputeDesignResult(targetSettings)
  targetResult   <- .bfsdTargetResults(settings, designResult, target)
  reached        <- isTRUE(search[["reached"]]) && isTRUE(targetResult[["reached"]][1])

  targetResult[["reached"]] <- reached
  if (!reached)
    targetResult[["actualProbability"]] <- NA_real_

  targetResult <- .bfsdTargetResultWithMaximumN(
    targetResult,
    if (reached) max(targetSettings[["n1Seq"]], na.rm = TRUE) else NA_integer_
  )

  return(list(
    maximumN     = max(targetSettings[["n1Seq"]], na.rm = TRUE),
    targetResult = targetResult,
    settings     = targetSettings,
    designResult = designResult,
    limitReached = !reached
  ))
}

.bfsdRunMaximumSampleSizeSearch <- function(settings, target, nrange, progress = NULL) {
  searchFunction <- .bfsdMaximumSampleSizeSearchFunction(settings)
  if (!is.null(progress))
    .bfsdRequireBfpwrSearchProgress(searchFunction)

  args <- c(
    .bfsdMaximumSampleSizeSearchArguments(settings, target, nrange),
    .bfsdIntegrationArguments(settings)
  )

  if (!is.null(progress))
    args[["progress"]] <- progress

  do.call(
    what = searchFunction,
    args = args
  )
}

.bfsdMaximumSampleSizeSearchFunction <- function(settings) {
  if (settings[["isZTest"]])
    return(bfpwr::nbf01seq)

  return(bfpwr::ntbf01seq)
}

.bfsdRequireBfpwrSearchProgress <- function(searchFunction) {
  if ("progress" %in% names(formals(searchFunction)))
    return(invisible(TRUE))

  stop(gettext("The installed bfpwr package does not support progress callbacks for sequential sample-size search."))
}

.bfsdMaximumSampleSizeSearchArguments <- function(settings, target, nrange) {
  args <- if (settings[["isZTest"]]) {
    .bfsdZMaximumSampleSizeSearchArguments(settings, target)
  } else {
    .bfsdTMaximumSampleSizeSearchArguments(settings, target)
  }
  searchArgs <- list(
    target  = target,
    nrange  = nrange,
    strict  = settings[["strictIntegration"]],
    integer = TRUE,
    details = TRUE
  )

  if (.bfsdUsesSampleSizeSearchStrategy(settings))
    searchArgs[["search"]] <- settings[["sampleSizeSearchStrategy"]]

  return(c(
    args,
    searchArgs,
    .bfsdSearchScheduleArguments(settings)
  ))
}

.bfsdZMaximumSampleSizeSearchArguments <- function(settings, target) {
  analysisPrior <- .bfsdZAnalysisPrior(settings)

  return(list(
    k1    = settings[["k1"]],
    k0    = settings[["k0"]],
    power = .bfdTargetPower(settings, target),
    usd   = .bfsdZSearchUnitStandardDeviation(settings),
    null  = 0,
    pm    = analysisPrior[["pm"]],
    psd   = analysisPrior[["psd"]],
    dpm   = .bfsdDirectionalMean(settings, .bfsdSearchDesignMean(settings, target)),
    dpsd  = .bfsdSearchDesignSd(settings, target),
    type  = analysisPrior[["type"]]
  ))
}

.bfsdTMaximumSampleSizeSearchArguments <- function(settings, target) {
  return(list(
    k1          = settings[["k1"]],
    k0          = settings[["k0"]],
    power       = .bfdTargetPower(settings, target),
    null        = 0,
    plocation   = settings[["tPriorLocationRelative"]],
    pscale      = settings[["tPriorScale"]],
    pdf         = settings[["tPriorDf"]],
    dpm         = .bfsdSearchDesignMean(settings, target),
    dpsd        = .bfsdSearchDesignSd(settings, target),
    type        = settings[["testType"]],
    alternative = settings[["alternative"]],
    ratio       = if (settings[["isIndependentSamples"]]) settings[["sampleSizeRatio"]] else 1,
    trange      = .bfsdTrange(settings)
  ))
}

.bfsdSearchScheduleArguments <- function(settings) {
  if (settings[["lookScheduleMode"]] == "increase") {
    return(list(
      minN = ceiling(settings[["sampleSizeFirstLook"]]),
      by   = ceiling(settings[["sampleSizeIncrease"]])
    ))
  }

  return(list(timing = .bfsdInformationFractions(settings)))
}

.bfsdZSearchUnitStandardDeviation <- function(settings) {
  if (isTRUE(settings[["isGeneralZ"]]))
    return(.bfdGeneralZUnitInformationSd(settings))

  if (settings[["isIndependentSamples"]])
    return(settings[["standardDeviation"]] * sqrt(1 + 1 / settings[["sampleSizeRatio"]]))

  return(settings[["standardDeviation"]])
}

.bfsdSearchMaximumN <- function(search) {
  maximumN <- search[["maximumN"]]

  if (length(maximumN) != 1 || !is.finite(maximumN))
    stop(gettext("The bfpwr sample-size search did not return a finite maximum sample size."))

  return(as.integer(ceiling(maximumN)))
}

.bfsdSettingsForSequentialSearch <- function(settings, search, maximumN) {
  candidate <- .bfsdSettingsForMaximumN(settings, maximumN)
  n1Seq     <- .bfsdSearchResultN1(search)

  candidate[["n1Seq"]] <- as.integer(ceiling(n1Seq))

  n2Seq <- .bfsdSearchResultN2(search)
  candidate[["n2Seq"]] <- if (!is.null(n2Seq)) {
    as.integer(ceiling(n2Seq))
  } else if (candidate[["isIndependentSamples"]]) {
    as.integer(ceiling(candidate[["n1Seq"]] * candidate[["sampleSizeRatio"]]))
  } else {
    candidate[["n1Seq"]]
  }

  if (candidate[["isIndependentSamples"]])
    .bfsdValidateSecondGroupSampleSize(candidate[["n2Seq"]])

  candidate[["n1"]] <- max(candidate[["n1Seq"]])
  candidate[["n2"]] <- max(candidate[["n2Seq"]])

  return(candidate)
}

.bfsdSearchResultN1 <- function(search) {
  design <- search[["result"]]
  if (is.null(design))
    stop(gettext("The bfpwr sample-size search did not return a sequential design."))

  if (!is.null(design[["n1"]]))
    return(design[["n1"]])

  if (!is.null(design[["n"]]))
    return(design[["n"]])

  stop(gettext("The bfpwr sample-size search result did not contain a sample-size schedule."))
}

.bfsdSearchResultN2 <- function(search) {
  design <- search[["result"]]
  if (is.null(design))
    stop(gettext("The bfpwr sample-size search did not return a sequential design."))

  if (is.null(design[["n2"]]))
    return(NULL)

  return(design[["n2"]])
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

  return(.bfdSampleSizeBasisTarget(settings[["designSampleSizeBasis"]], under))
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

  return(.bfdCommonSampleSizeBasisTarget(settings[["designSampleSizeBasis"]]))
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
        plocation   = settings[["tPriorLocationRelative"]],
        pscale      = settings[["tPriorScale"]],
        pdf         = settings[["tPriorDf"]],
        dpm         = designPriorMean,
        dpsd        = designPriorSd,
        type        = settings[["testType"]],
        alternative = settings[["alternative"]],
        strict      = settings[["strictIntegration"]],
        trange      = .bfsdTrange(settings)
      ),
      .bfdTTestSampleSizeArguments(settings, settings[["n1Seq"]], settings[["n2Seq"]]),
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

.bfsdTrange <- function(settings) {
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

  .bfsdInitializeResultsTable(table, settings)
  .bfsdPopulateResultsTable(table, settings, result, columnsReady = TRUE)
}

.bfsdPopulateResultsTable <- function(table, settings, result, columnsReady = FALSE) {
  if (!isTRUE(columnsReady))
    .bfsdAddResultsColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute Bayes factor sequential design: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.bfsdResultsRow(settings, result))

  combinedSampleSizeFootnote <- .bfsdCombinedMaximumSampleSizeFootnote(settings, result)
  if (!is.null(combinedSampleSizeFootnote))
    table$addFootnote(combinedSampleSizeFootnote)

  displayMaximumFootnote <- .bfsdDisplayMaximumFootnote(settings, result)
  if (!is.null(displayMaximumFootnote))
    table$addFootnote(displayMaximumFootnote)

  unreachedTargetsFootnote <- .bfsdUnreachedTargetsFootnote(result)
  if (!is.null(unreachedTargetsFootnote))
    table$addFootnote(unreachedTargetsFootnote)

  generalZUisdFootnote <- .bfdGeneralZKnownUisdFootnote(settings)
  if (!is.null(generalZUisdFootnote))
    table$addFootnote(generalZUisdFootnote)

  if (.bfsdShowsIntegerSampleSizeScheduleFootnote(settings))
    table$addFootnote(gettext("Due to integer sample-size schedules, Pr(Conclusive Evidence) can exceed the target probability."))

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested N\u2082/N\u2081 sample-size ratio, unless a custom group 2 schedule is supplied."))

  if (.bfsdUsesStandardErrorOnly(settings))
    table$addFootnote(gettext("This design uses the supplied standard error schedule, so sample-size summaries are omitted."))

  .bfdAddExplanationFootnotes(table, settings, .bfsdResultsTableExplanation(settings))
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

.bfsdEmptyResultsRow <- function(settings) {
  columns <- c(
    "designPrior",
    "decisionRule",
    if (.bfsdUsesSampleSizeSearch(settings)) "targetProbability",
    "actualProbability",
    if (.bfsdUsesStandardErrorOnly(settings)) "finalSE",
    if (!.bfsdUsesStandardErrorOnly(settings) && settings[["isIndependentSamples"]]) c("maximumN1", "maximumN2"),
    if (!.bfsdUsesStandardErrorOnly(settings) && !settings[["isIndependentSamples"]]) "maximumN"
  )

  return(.bfdEmptyTableRow(
    columns        = columns,
    stringColumns  = c("designPrior", "decisionRule"),
    integerColumns = if (!.bfsdUsesStandardErrorOnly(settings)) {
      if (settings[["isIndependentSamples"]]) c("maximumN1", "maximumN2") else "maximumN"
    } else {
      character()
    }
  ))
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

.bfsdResultsTableExplanation <- function(settings) {
  if (.bfsdUsesSampleSizeSearch(settings)) {
    return(gettext(
      "This table reports the maximum sample size schedule needed to reach the target probability of conclusive evidence at the planned looks."
    ))
  }

  gettext(
    "This table evaluates the selected sequential design through the final look. Achieved probabilities are cumulative stopping probabilities for conclusive evidence under the corresponding design prior."
  )
}

.bfsdUsesAdaptiveSampleSizeSearch <- function(settings) {
  return(
    .bfsdUsesSampleSizeSearchStrategy(settings) &&
      identical(settings[["sampleSizeSearchStrategy"]], "adaptive")
  )
}

.bfsdShowsIntegerSampleSizeScheduleFootnote <- function(settings) {
  return(
    .bfsdUsesSampleSizeSearch(settings) &&
      !.bfsdUsesAdaptiveSampleSizeSearch(settings)
  )
}

.bfsdSampleSizeSummaryExplanation <- function() {
  gettext(
    "Mean sample size and SD describe how much data the sequential design is expected to use under each design prior. They are averages over hypothetical repetitions of the planned study."
  )
}

.bfsdDesignOutcomeTableExplanation <- function() {
  gettext(
    "Rows condition on the design prior used to generate hypothetical sequential studies. Diagonal cells are conclusive evidence for the true hypothesis, off-diagonal cells are misleading evidence, and the middle column is the chance of reaching the final look without a conclusion."
  )
}

.bfsdStagewiseTotalExplanation <- function() {
  gettext(
    "Cumulative probabilities show the chance that the study has stopped by each look. The inconclusive column is the chance that no stopping rule has been crossed yet."
  )
}

.bfsdStagewiseIncrementalExplanation <- function() {
  gettext(
    "New stop probabilities isolate decisions first made at a given look. Summing them over looks gives the cumulative stopping probability for the corresponding decision."
  )
}

.bfsdBoundariesTableExplanation <- function() {
  gettext(
    "Stopping boundaries are the test-statistic values that correspond to the selected Bayes factor thresholds at each look. Crossing a boundary triggers stopping for H\u2081 or H\u2080."
  )
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

  .bfsdPopulateSampleSizeSummaryTable(table, settings, result, includeTargetStatusFootnotes = TRUE)
}

.bfsdPopulateSampleSizeSummaryTable <- function(table, settings, result, columnsReady = FALSE, includeTargetStatusFootnotes = FALSE) {
  if (!isTRUE(columnsReady))
    .bfsdAddSampleSizeSummaryColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute sample size summary: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.bfsdSampleSizeSummaryRows(settings, result))

  if (isTRUE(includeTargetStatusFootnotes)) {
    unreachedTargetsFootnote <- .bfsdUnreachedTargetsFootnote(result)
    if (!is.null(unreachedTargetsFootnote))
      table$addFootnote(unreachedTargetsFootnote)
  }

  .bfdAddExplanationFootnotes(table, settings, .bfsdSampleSizeSummaryExplanation())
}

.bfsdAddSampleSizeSummaryColumns <- function(table, settings) {
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
}

.bfsdEmptySampleSizeSummaryRow <- function(settings) {
  columns <- c(
    "designPrior",
    if (.bfsdUsesSampleSizeSearch(settings)) "decisionRule",
    "looks",
    if (settings[["isIndependentSamples"]]) {
      c("firstN1", "firstN2", "maximumN1", "maximumN2", "meanN1", "meanN2", "sdN1", "sdN2")
    } else {
      c("firstN", "maximumN", "meanN", "sdN")
    }
  )
  integerColumns <- c(
    "looks",
    if (settings[["isIndependentSamples"]]) c("firstN1", "firstN2", "maximumN1", "maximumN2") else c("firstN", "maximumN")
  )

  return(.bfdEmptyTableRow(
    columns        = columns,
    stringColumns  = c("designPrior", "decisionRule"),
    integerColumns = integerColumns
  ))
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

  .bfsdPopulateDesignOutcomeTable(table, settings, result)
}

.bfsdPopulateDesignOutcomeTable <- function(table, settings, result, columnsReady = FALSE) {
  if (!isTRUE(columnsReady))
    .bfsdAddDesignOutcomeColumns(table)

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
  .bfdAddExplanationFootnotes(table, settings, .bfsdDesignOutcomeTableExplanation())
}

.bfsdAddDesignOutcomeColumns <- function(table) {
  .bfdAddDesignOutcomeColumns(
    table,
    underTitle       = gettext("Design Prior"),
    overtitle        = gettext("Bayes Factor Decision"),
    nullTitle        = gettext("Evidence for H\u2080"),
    alternativeTitle = gettext("Evidence for H\u2081")
  )
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

  .bfsdPopulateStagewiseTotalTable(table, settings, result)
}

.bfsdPopulateStagewiseTotalTable <- function(table, settings, result, columnsReady = FALSE) {
  if (jaspBase::isTryError(result)) {
    if (!isTRUE(columnsReady))
      .bfsdAddStagewiseTotalColumns(table, settings)
    table$setError(gettextf("Unable to compute cumulative decision probabilities: %1$s", .bfdCleanError(result)))
    return()
  }

  if (!isTRUE(columnsReady))
    .bfsdAddStagewiseTotalColumns(table, settings, result)

  table$setData(.bfsdStagewiseTotalRows(
    settings     = settings,
    result       = result,
    layoutResult = if (isTRUE(columnsReady)) NULL else result
  ))
  .bfdAddExplanationFootnotes(table, settings, .bfsdStagewiseTotalExplanation())
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

  .bfsdPopulateStagewiseIncrementalTable(table, settings, result)
}

.bfsdPopulateStagewiseIncrementalTable <- function(table, settings, result, columnsReady = FALSE) {
  if (jaspBase::isTryError(result)) {
    if (!isTRUE(columnsReady))
      .bfsdAddStagewiseIncrementalColumns(table, settings)
    table$setError(gettextf("Unable to compute incremental stagewise stops: %1$s", .bfdCleanError(result)))
    return()
  }

  if (!isTRUE(columnsReady))
    .bfsdAddStagewiseIncrementalColumns(table, settings, result)

  table$setData(.bfsdStagewiseIncrementalRows(
    settings     = settings,
    result       = result,
    layoutResult = if (isTRUE(columnsReady)) NULL else result
  ))
  .bfdAddExplanationFootnotes(table, settings, .bfsdStagewiseIncrementalExplanation())
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

.bfsdStagewiseTotalRows <- function(settings, result, layoutResult = result) {
  if (.bfsdUsesSeparateLookSchedules(settings, layoutResult)) {
    rows     <- .bfsdSeparateLookRows(settings, result)
    h1Design <- .bfsdBasisDesignForUnder(settings, result, "h1")
    h0Design <- .bfsdBasisDesignForUnder(settings, result, "h0")
    rows[["h1Alternative"]] <- .bfsdPadByLooks(h1Design[["cumpH1"]], nrow(rows))
    rows[["h1Undecided"]]   <- .bfsdPadByLooks(h1Design[["cumpInc"]], nrow(rows))
    rows[["h1Null"]]        <- .bfsdPadByLooks(h1Design[["cumpH0"]], nrow(rows))
    rows[["h0Alternative"]] <- .bfsdPadByLooks(h0Design[["cumpH1"]], nrow(rows))
    rows[["h0Undecided"]]   <- .bfsdPadByLooks(h0Design[["cumpInc"]], nrow(rows))
    rows[["h0Null"]]        <- .bfsdPadByLooks(h0Design[["cumpH0"]], nrow(rows))

    return(.bfsdOrderStagewiseRows(rows, settings, layoutResult, "total"))
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

  return(.bfsdOrderStagewiseRows(rows, settings, layoutResult, "total"))
}

.bfsdStagewiseIncrementalRows <- function(settings, result, layoutResult = result) {
  h1Design <- .bfsdBasisDesignForUnder(settings, result, "h1")
  h0Design <- .bfsdBasisDesignForUnder(settings, result, "h0")

  h1AlternativeStop <- diff(c(0, h1Design[["cumpH1"]]))
  h1NullStop        <- diff(c(0, h1Design[["cumpH0"]]))
  h0AlternativeStop <- diff(c(0, h0Design[["cumpH1"]]))
  h0NullStop        <- diff(c(0, h0Design[["cumpH0"]]))

  rows <- if (.bfsdUsesSeparateLookSchedules(settings, layoutResult)) .bfsdSeparateLookRows(settings, result) else .bfsdLookRows(.bfsdBasisSettings(settings, result, "h1"))
  rows[["h1AlternativeStop"]] <- .bfsdPadByLooks(h1AlternativeStop, nrow(rows))
  rows[["h1NullStop"]]        <- .bfsdPadByLooks(h1NullStop, nrow(rows))
  rows[["h1AnyStop"]]         <- .bfsdPadByLooks(h1AlternativeStop + h1NullStop, nrow(rows))
  rows[["h0AlternativeStop"]] <- .bfsdPadByLooks(h0AlternativeStop, nrow(rows))
  rows[["h0NullStop"]]        <- .bfsdPadByLooks(h0NullStop, nrow(rows))
  rows[["h0AnyStop"]]         <- .bfsdPadByLooks(h0AlternativeStop + h0NullStop, nrow(rows))

  return(.bfsdOrderStagewiseRows(rows, settings, layoutResult, "incremental"))
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

.bfsdEmptyStagewiseRow <- function(settings, type) {
  separateSchedules <- .bfsdUsesSeparateLookSchedules(settings, result = NULL)
  columns <- if (separateSchedules) {
    c(
      "look",
      .bfsdStagewiseSampleSizeColumnNames(settings, "h1"),
      .bfsdStagewiseEvidenceColumnNames("h1", type),
      .bfsdStagewiseSampleSizeColumnNames(settings, "h0"),
      .bfsdStagewiseEvidenceColumnNames("h0", type)
    )
  } else {
    c(
      "look",
      .bfsdStagewiseSampleSizeColumnNames(settings),
      .bfsdStagewiseEvidenceColumnNames("h1", type),
      .bfsdStagewiseEvidenceColumnNames("h0", type)
    )
  }

  sampleSizeColumns <- if (separateSchedules) {
    c(.bfsdStagewiseSampleSizeColumnNames(settings, "h1"), .bfsdStagewiseSampleSizeColumnNames(settings, "h0"))
  } else {
    .bfsdStagewiseSampleSizeColumnNames(settings)
  }
  integerColumns <- c("look", if (!.bfsdUsesStandardErrorOnly(settings)) sampleSizeColumns)

  return(.bfdEmptyTableRow(
    columns        = columns,
    integerColumns = integerColumns
  ))
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

  .bfsdPopulateBoundariesTable(table, settings, result)
}

.bfsdPopulateBoundariesTable <- function(table, settings, result, columnsReady = FALSE) {
  if (!isTRUE(columnsReady))
    .bfsdAddBoundariesColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stopping boundaries: %1$s", .bfdCleanError(result)))
    return()
  }

  boundarySettings <- .bfsdCommonBasisSettings(settings, result)
  boundaryResult   <- .bfsdCommonBasisDesignResult(result, settings)
  rows <- .bfsdBoundaryRows(boundarySettings, boundaryResult[["design"]])
  table$setData(rows)

  table$addFootnote(gettext("Boundaries are shown on the test-statistic scale. Empty cells mean no finite statistic reaches the decision rule at that look."))
  .bfdAddExplanationFootnotes(table, settings, .bfsdBoundariesTableExplanation())
}

.bfsdAddBoundariesColumns <- function(table, settings) {
  .bfsdAddLookColumns(table, settings, separate = FALSE)

  h1Overtitle <- .bfsdBoundaryDecisionRuleOvertitle(settings, "h1")
  h0Overtitle <- .bfsdBoundaryDecisionRuleOvertitle(settings, "h0")
  table$addColumnInfo(name = "h1Lower", title = gettext("Lower Statistic"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1Upper", title = gettext("Upper Statistic"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h0Lower", title = gettext("Lower Statistic"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0Upper", title = gettext("Upper Statistic"), type = "number", overtitle = h0Overtitle)
}

.bfsdEmptyBoundariesRow <- function(settings) {
  sampleSizeColumns <- if (.bfsdUsesStandardErrorOnly(settings)) {
    "standardError"
  } else if (settings[["isIndependentSamples"]]) {
    c("n1", "n2")
  } else {
    "n"
  }

  return(.bfdEmptyTableRow(
    columns        = c("look", sampleSizeColumns, "h1Lower", "h1Upper", "h0Lower", "h0Upper"),
    integerColumns = c("look", if (!.bfsdUsesStandardErrorOnly(settings)) sampleSizeColumns)
  ))
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

  .bfsdPopulatePriorsTable(table, settings)
}

.bfsdPopulatePriorsTable <- function(table, settings) {
  .bfdAddPriorsTableColumns(table)
  table$setData(.bfdPriorsRows(settings))

  if (!identical(settings[["nullValue"]], 0))
    table$addFootnote(gettext("Sequential bfpwr calculations are performed on the parameter scale centered at the null value."))

  .bfdAddExplanationFootnotes(table, settings, .bfdPriorsTableExplanation())
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
  if (.bfsdUsesSampleSizeSearch(settings)) {
    return(.bfdFormatRCall(
      "bfpwr::nbf01seq",
      c(
        .bfsdMaximumSampleSizeSearchArguments(
          settings = settings,
          target   = target,
          nrange   = c(.bfsdSearchLowerBound(settings), ceiling(settings[["rangeMax"]]))
        ),
        .bfsdIntegrationArguments(settings)
      )
    ))
  }

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
  if (.bfsdUsesSampleSizeSearch(settings)) {
    return(.bfdFormatRCall(
      "bfpwr::ntbf01seq",
      c(
        .bfsdMaximumSampleSizeSearchArguments(
          settings = settings,
          target   = target,
          nrange   = c(.bfsdSearchLowerBound(settings), ceiling(settings[["rangeMax"]]))
        ),
        .bfsdIntegrationArguments(settings)
      )
    ))
  }

  args <- c(
    list(
      k1          = settings[["k1"]],
      k0          = settings[["k0"]]
    ),
    .bfdTTestSampleSizeArguments(settings, settings[["n1Seq"]], settings[["n2Seq"]]),
    list(
      plocation   = settings[["tPriorLocationRelative"]],
      pscale      = settings[["tPriorScale"]],
      pdf         = settings[["tPriorDf"]],
      dpm         = .bfsdSearchDesignMean(settings, target),
      dpsd        = .bfsdSearchDesignSd(settings, target),
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      strict      = settings[["strictIntegration"]],
      trange      = .bfsdTrange(settings)
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
  .bfdAddExplanationHtml(
    parent       = jaspResults,
    key          = paste0(key, "Note"),
    settings     = settings,
    position     = position + 0.1,
    dependencies = .bfsdStoppingProbabilitiesPlotDependencies,
    text         = .bfsdStoppingProbabilitiesPlotExplanation()
  )
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
      .bfsdLookXAxisScale(settings, data[["n"]]) +
      .bfdProbabilityYScale() +
      ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Outcome"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::geom_point(size = 2) +
      .bfsdLookXAxisScale(settings, data[["n"]]) +
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

.bfsdStoppingProbabilitiesPlotExplanation <- function() {
  gettext(
    "The curves accumulate over planned looks: conclusive and misleading curves increase as studies stop for those outcomes, while the inconclusive curve decreases as decisions are made."
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
  .bfdAddExplanationHtml(
    parent       = jaspResults,
    key          = paste0(key, "Note"),
    settings     = settings,
    position     = position + 0.1,
    dependencies = .bfsdStoppingBoundariesPlotDependencies,
    text         = .bfsdStoppingBoundariesPlotExplanation()
  )
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
    .bfsdLookXAxisScale(settings, data[["n"]]) +
    .pwrPrettyYAxisScale(c(0, data[["criticalValue"]])) +
    ggplot2::labs(x = xLabel, y = gettext("Critical value"), color = gettext("Target"), linetype = gettext("Boundary")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "#666666")

  return(.bfdApplyPlotTheme(plot, settings))
}

.bfsdLookXAxisScale <- function(settings, x) {
  return(.pwrPrettyIntegerXAxisScale(x))
}

.bfsdStoppingBoundariesPlotExplanation <- function() {
  gettext(
    "Each curve gives the critical test-statistic value that corresponds to a Bayes factor stopping threshold at a look. Crossing a boundary triggers stopping for H\u2081 or H\u2080."
  )
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
