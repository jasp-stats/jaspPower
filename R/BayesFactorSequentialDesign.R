BayesFactorSequentialDesign <- function(jaspResults, dataset, options) {
  settings    <- .evSeqPrepareSettings(options)
  computation <- .evSeqCachedComputation(jaspResults, settings)
  settings    <- computation[["settings"]]
  result      <- computation[["result"]]

  .evSeqResultsTable(jaspResults, settings, result)
  .evSeqSampleSizeSummaryTable(jaspResults, settings, result)
  .evSeqDesignOutcomeTable(jaspResults, settings, result)
  .evSeqStagewiseTotalTable(jaspResults, settings, result)
  .evSeqStagewiseIncrementalTable(jaspResults, settings, result)
  .evSeqBoundariesTable(jaspResults, settings, result)
  .evSeqPriorsTable(jaspResults, settings)

  if (.evObservedAnalysisReady(dataset, options, settings)) {
    .evObservedAnalysisTable(
      jaspResults  = jaspResults,
      dataset      = dataset,
      options      = options,
      settings     = settings,
      key          = "sequentialEvidenceObservedAnalysis",
      position     = 8,
      sequential   = TRUE,
      dependencies = .evSeqDependencies
    )
  }

  if (isTRUE(options[["text"]]))
    .evSeqText(jaspResults, settings, result)

  if (isTRUE(options[["generateReport"]]))
    .evSeqReport(jaspResults, settings, result)

  if (isTRUE(options[["generateRCode"]]))
    .evSeqRCode(jaspResults, settings, result)

  if (isTRUE(options[["stoppingProbabilitiesPlot"]]))
    .evSeqStoppingProbabilitiesPlot(jaspResults, settings, result)

  if (isTRUE(options[["stoppingBoundariesPlot"]]))
    .evSeqStoppingBoundariesPlot(jaspResults, settings, result)

  if (isTRUE(options[["priorDistribution"]]))
    .evSeqPriorPlot(jaspResults, settings)

  return()
}

.evSeqDependencies <- c(
  "test", "calculation", "bf10Threshold", "bf01Threshold", "evidenceTarget",
  "evidenceProbability", "targetPowerH1", "targetPowerH0", "alternative", "lookScheduleMode",
  "numberOfLooks", "sampleSizeFirstLook", "sampleSizeIncrease", "sampleSize", "sampleSizeSchedule",
  "sampleSizeSecondGroupSchedule", "sampleSizeRatio", "informationFractionFirstLook",
  "informationFractionSchedule", "sampleSizeRangeMin", "sampleSizeRangeMax",
  "standardDeviation",
  "generalZParameterization", "unitInformationSd", "standardErrorSchedule",
  "nullPriorDistribution", "nullValue", "analysisPriorDistribution",
  "analysisPriorPoint", "analysisPriorMean", "analysisPriorSd",
  "momentPriorSpread", "momentPriorMode", "tPriorLocation", "tPriorScale",
  "tPriorDf", "designNullPrior", "designNullPriorMean", "designNullPriorSd",
  "designPrior", "designPriorMean", "designPriorSd",
  "strictIntegration", "integrationMethod", "integrationAbsEps",
  "integrationRelEps", "integrationMaxPts", "drangeMode", "drangeLower",
  "drangeUpper"
)

.evSeqCachedComputation <- function(jaspResults, settings) {
  state <- jaspResults[["sequentialEvidenceComputation"]]
  if (!is.null(state) && !is.null(state$object))
    return(.evSeqWithCurrentDisplaySettings(state$object, settings))

  state <- createJaspState()
  state$dependOn(.evSeqDependencies)
  jaspResults[["sequentialEvidenceComputation"]] <- state

  result           <- try(.evSeqComputeResult(settings), silent = TRUE)
  computedSettings <- settings
  if (!jaspBase::isTryError(result) && !is.null(result[["settings"]]))
    computedSettings <- result[["settings"]]

  state$object <- list(
    settings = computedSettings,
    result   = result
  )

  return(.evSeqWithCurrentDisplaySettings(state$object, settings))
}

.evSeqWithCurrentDisplaySettings <- function(computation, currentSettings) {
  settings <- computation[["settings"]]
  settings[["plotPoints"]]         <- currentSettings[["plotPoints"]]
  settings[["showNullReference"]]  <- currentSettings[["showNullReference"]]
  settings[["mergeH1H0Figures"]]   <- currentSettings[["mergeH1H0Figures"]]
  settings[["priorPlotDesign"]]    <- currentSettings[["priorPlotDesign"]]
  settings[["priorPlotAnalysis"]]  <- currentSettings[["priorPlotAnalysis"]]
  settings[["priorPlotMerge"]]     <- currentSettings[["priorPlotMerge"]]
  settings[["reportLatex"]]        <- currentSettings[["reportLatex"]]

  return(list(
    settings = settings,
    result   = computation[["result"]]
  ))
}

.evSeqPrepareSettings <- function(options) {
  test <- options[["test"]]

  settings <- list(
    test                 = test,
    testLabel            = .evTestLabel(test),
    testType             = .evTestType(test),
    isIndependentSamples = grepl("independentSamples", test, fixed = TRUE),
    isTTest              = grepl("TTest", test, fixed = TRUE),
    isGeneralZ           = identical(test, "generalZApproximation"),
    isZTest              = grepl("ZTest", test, fixed = TRUE) || identical(test, "generalZApproximation"),
    isBinomial           = FALSE,
    calculation          = options[["calculation"]],
    bf10Threshold        = options[["bf10Threshold"]],
    bf01Threshold        = options[["bf01Threshold"]],
    evidenceTarget       = .evOption(options, "evidenceTarget", "h1"),
    targetPowerH1        = .evOption(options, "targetPowerH1", .evOption(options, "evidenceProbability", 0.9)),
    targetPowerH0        = .evOption(options, "targetPowerH0", .evOption(options, "evidenceProbability", 0.9)),
    planningTargets      = .evPlanningTargets(options, .evOption(options, "evidenceTarget", NULL)),
    k1                   = 1 / options[["bf10Threshold"]],
    k0                   = options[["bf01Threshold"]],
    lookScheduleMode     = options[["lookScheduleMode"]],
    numberOfLooks        = options[["numberOfLooks"]],
    sampleSizeFirstLook  = options[["sampleSizeFirstLook"]],
    sampleSizeIncrease   = .evOption(options, "sampleSizeIncrease", 20),
    sampleSize           = options[["sampleSize"]],
    sampleSizeSchedule   = options[["sampleSizeSchedule"]],
    sampleSizeSecondGroupSchedule = options[["sampleSizeSecondGroupSchedule"]],
    sampleSizeRatio      = options[["sampleSizeRatio"]],
    informationFractionFirstLook = options[["informationFractionFirstLook"]],
    informationFractionSchedule  = options[["informationFractionSchedule"]],
    rangeMin             = options[["sampleSizeRangeMin"]],
    rangeMax             = options[["sampleSizeRangeMax"]],
    nullPriorDistribution = options[["nullPriorDistribution"]],
    nullValue            = options[["nullValue"]],
    strictIntegration    = options[["strictIntegration"]],
    integrationMethod    = options[["integrationMethod"]],
    integrationAbsEps    = options[["integrationAbsEps"]],
    integrationRelEps    = options[["integrationRelEps"]],
    integrationMaxPts    = options[["integrationMaxPts"]],
    drangeMode           = options[["drangeMode"]],
    drangeLower          = options[["drangeLower"]],
    drangeUpper          = options[["drangeUpper"]],
    plotPoints           = options[["plotPoints"]],
    showNullReference    = options[["showNullReference"]],
    mergeH1H0Figures     = .evOption(options, "mergeH1H0Figures", FALSE),
    priorPlotDesign      = .evOption(options, "priorDistributionDesign", TRUE),
    priorPlotAnalysis    = .evOption(options, "priorDistributionAnalysis", TRUE),
    priorPlotMerge       = .evOption(options, "priorDistributionMerge", FALSE),
    reportLatex          = .evOption(options, "generateReportLatex", FALSE)
  )

  settings <- .evSeqAddContinuousSettings(settings, options)

  return(settings)
}

.evSeqAddContinuousSettings <- function(settings, options) {
  settings[["standardDeviation"]] <- options[["standardDeviation"]]
  settings[["generalZParameterization"]] <- options[["generalZParameterization"]]
  settings[["unitInformationSd"]]        <- options[["unitInformationSd"]]
  settings[["standardErrorSchedule"]]    <- options[["standardErrorSchedule"]]
  settings[["alternative"]]       <- switch(options[["alternative"]], twoSided = "two.sided", options[["alternative"]])

  if (settings[["isZTest"]]) {
    settings[["analysisPriorIsCauchy"]] <- FALSE
    analysisPriorDistribution <- .evOption(options, "analysisPriorDistribution", "normal")
    if (length(analysisPriorDistribution) != 1 || is.na(analysisPriorDistribution) || analysisPriorDistribution == "")
      analysisPriorDistribution <- "normal"
    if (settings[["alternative"]] != "two.sided")
      analysisPriorDistribution <- "normal"

    settings[["analysisPriorDistribution"]] <- analysisPriorDistribution
    settings[["analysisPriorMean"]]         <- if (analysisPriorDistribution == "point") .evOption(options, "analysisPriorPoint", 0) else .evOption(options, "analysisPriorMean", 0)
    settings[["analysisPriorSd"]]           <- if (analysisPriorDistribution == "point") 0 else .evOption(options, "analysisPriorSd", 1)
    settings[["analysisPriorMeanRelative"]] <- settings[["analysisPriorMean"]] - settings[["nullValue"]]
    settings[["momentPriorSpread"]]         <- if (analysisPriorDistribution == "normalMomentMode") {
      .evOption(options, "momentPriorMode", sqrt(2)) / sqrt(2)
    } else {
      .evOption(options, "momentPriorSpread", 1)
    }
    settings[["momentPriorMode"]] <- sqrt(2) * settings[["momentPriorSpread"]]
    settings[["isDirectionalZTest"]] <- .evSeqUsesDirectionalZ(settings)
  } else {
    analysisPriorDistribution <- .evOption(options, "analysisPriorDistribution", "cauchy")
    if (length(analysisPriorDistribution) != 1 || is.na(analysisPriorDistribution) || analysisPriorDistribution == "")
      analysisPriorDistribution <- "cauchy"
    settings[["analysisPriorIsCauchy"]]     <- identical(analysisPriorDistribution, "cauchy")
    settings[["analysisPriorDistribution"]] <- "t"
    settings[["tPriorLocation"]]            <- options[["tPriorLocation"]]
    settings[["tPriorScale"]]               <- options[["tPriorScale"]]
    settings[["tPriorDf"]]                  <- if (isTRUE(settings[["analysisPriorIsCauchy"]])) 1 else .evOption(options, "tPriorDf", 1)
    settings[["tPriorLocationRelative"]]    <- settings[["tPriorLocation"]] - settings[["nullValue"]]
    settings[["isDirectionalZTest"]]        <- FALSE
  }

  settings <- .evSeqAddDesignPriors(settings, options)

  return(settings)
}

.evSeqAddDesignPriors <- function(settings, options) {
  designNullPrior <- .evOption(options, "designNullPrior", "point")
  designH0 <- list(
    distribution = designNullPrior,
    mean         = .evOption(options, "designNullPriorMean", settings[["nullValue"]]),
    sd           = if (designNullPrior == "point") 0 else .evOption(options, "designNullPriorSd", 0.1),
    label        = if (designNullPrior == "point") gettext("Point") else gettext("Normal"),
    parameters   = NULL
  )
  designH0[["parameters"]] <- .evContinuousDesignParameters(designH0)

  designPrior <- .evOption(options, "designPrior", "point")
  designH1 <- list(
    distribution = designPrior,
    mean         = .evOption(options, "designPriorMean", 0.5),
    sd           = if (designPrior == "point") 0 else .evOption(options, "designPriorSd", 0.1),
    label        = if (designPrior == "point") gettext("Point") else gettext("Normal"),
    parameters   = NULL
  )
  designH1[["parameters"]] <- .evContinuousDesignParameters(designH1)

  settings[["designPriorUnderH0"]] <- designH0
  settings[["designPriorUnderH1"]] <- designH1
  settings[["designPrior"]]        <- designH1[["distribution"]]
  settings[["designPriorMean"]]    <- designH1[["mean"]]
  settings[["designPriorSd"]]      <- designH1[["sd"]]
  settings[["designPriorMeanRelative"]] <- designH1[["mean"]] - settings[["nullValue"]]
  settings[["designNullPriorMeanRelative"]] <- designH0[["mean"]] - settings[["nullValue"]]

  return(settings)
}

.evSeqUsesSampleSizeSearch <- function(settings) {
  return(identical(settings[["calculation"]], "sampleSize"))
}

.evSeqUsesStandardErrorOnly <- function(settings) {
  return(
    isTRUE(settings[["isGeneralZ"]]) &&
      identical(settings[["generalZParameterization"]], "standardErrorSchedule") &&
      !.evSeqUsesSampleSizeSearch(settings)
  )
}

.evSeqCompleteSettings <- function(settings) {
  if (.evSeqUsesStandardErrorOnly(settings))
    return(.evSeqApplyStandardErrorSchedule(settings))

  if (!.evSeqUsesSampleSizeSearch(settings))
    return(.evSeqApplyFixedSchedule(settings))

  return(settings)
}

.evSeqApplyFixedSchedule <- function(settings) {
  settings[["n1Seq"]] <- .evSeqSampleSizeSchedule(settings)
  settings[["n2Seq"]] <- .evSeqSampleSizeSecondGroup(settings, settings[["n1Seq"]])
  settings[["n1"]]    <- max(settings[["n1Seq"]])
  settings[["n2"]]    <- max(settings[["n2Seq"]])

  return(settings)
}

.evSeqApplyStandardErrorSchedule <- function(settings) {
  se <- .evSeqParseNumericSchedule(settings[["standardErrorSchedule"]], gettext("standard error schedule"))

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

.evSeqSampleSizeSchedule <- function(settings) {
  if (settings[["lookScheduleMode"]] == "custom") {
    n <- .evSeqParseNumericSchedule(settings[["sampleSizeSchedule"]], gettext("sample size schedule"), integer = TRUE)

    if (length(n) < 1)
      stop(gettext("The custom sample size schedule must contain at least one look."))

    if (any(n < 2))
      stop(gettext("All sample sizes in the custom schedule must be at least 2."))

    if (any(diff(n) <= 0))
      stop(gettext("The custom sample size schedule must be strictly increasing."))

    return(n)
  }

  if (settings[["lookScheduleMode"]] == "increase")
    return(.evSeqSampleSizeIncreaseSchedule(settings))

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

.evSeqSampleSizeIncreaseSchedule <- function(settings, maximumN = settings[["sampleSize"]]) {
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

.evSeqSampleSizeSecondGroup <- function(settings, n1) {
  if (!settings[["isIndependentSamples"]])
    return(n1)

  if (settings[["lookScheduleMode"]] == "custom") {
    n2 <- .evSeqParseNumericSchedule(settings[["sampleSizeSecondGroupSchedule"]], gettext("group 2 sample size schedule"), integer = TRUE)

    if (length(n2) != length(n1))
      stop(gettext("The group 1 and group 2 custom sample size schedules must have the same number of looks."))

    if (any(n2 < 2))
      stop(gettext("All group 2 sample sizes in the custom schedule must be at least 2."))

    if (any(diff(n2) <= 0))
      stop(gettext("The custom group 2 sample size schedule must be strictly increasing."))

    return(n2)
  }

  n2 <- ceiling(n1 * settings[["sampleSizeRatio"]])
  .evSeqValidateSecondGroupSampleSize(n2)

  return(n2)
}

.evSeqValidateSecondGroupSampleSize <- function(n2) {
  if (any(n2 < 2))
    stop(gettext("The sample-size ratio leads to a second-group sample size smaller than 2."))

  return(invisible(TRUE))
}

.evSeqParseNumericSchedule <- function(text, label, integer = FALSE) {
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

.evSeqComputeResult <- function(settings) {
  settings <- .evSeqCompleteSettings(settings)

  if (.evSeqUsesSampleSizeSearch(settings))
    return(.evSeqFindMaximumSampleSize(settings))

  result <- .evSeqComputeDesignResult(settings)
  result[["settings"]] <- settings

  return(result)
}

.evSeqComputeDesignResult <- function(settings) {
  design <- .evSeqRunDesign(
    settings,
    designPriorMean = settings[["designPriorMeanRelative"]],
    designPriorSd   = settings[["designPriorSd"]]
  )

  null <- .evSeqRunDesign(
    settings,
    designPriorMean = settings[["designNullPriorMeanRelative"]],
    designPriorSd   = settings[["designPriorUnderH0"]][["sd"]]
  )

  return(list(
    design = design,
    null   = null
  ))
}

.evSeqFindMaximumSampleSize <- function(settings) {
  .evValidateTargetPowers(settings)

  if (length(settings[["planningTargets"]]) == 0)
    stop(gettext("Select at least one Bayes factor planning target."))

  if (isTRUE(settings[["isGeneralZ"]]) && settings[["generalZParameterization"]] == "standardErrorSchedule")
    stop(gettext("Maximum sample size search is not available with a fixed standard error schedule."))

  minimumN <- ceiling(settings[["rangeMin"]])
  maximumN <- ceiling(settings[["rangeMax"]])

  if (minimumN >= maximumN)
    stop(gettext("The lower search bound must be smaller than the upper search bound."))

  cache <- new.env(parent = emptyenv())
  targetComputations <- lapply(settings[["planningTargets"]], function(target) {
    .evSeqFindMaximumSampleSizeForTarget(settings, target, minimumN, maximumN, cache)
  })
  names(targetComputations) <- settings[["planningTargets"]]

  displayMaximumN <- max(vapply(targetComputations, function(x) x[["maximumN"]], numeric(1)), na.rm = TRUE)
  displaySettings <- .evSeqSettingsForMaximumN(settings, displayMaximumN)
  result          <- .evSeqComputeDesignResult(displaySettings)
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

.evSeqFindMaximumSampleSizeForTarget <- function(settings, target, minimumN, maximumN, cache) {
  lower <- .evSeqEvaluateTargetMaximumN(settings, target, minimumN, cache)
  if (is.finite(lower[["criterion"]]) && lower[["criterion"]] >= 0)
    return(.evSeqSolvedTargetMaximumSampleSize(settings, target, minimumN, cache))

  upper <- .evSeqEvaluateTargetMaximumN(settings, target, maximumN, cache)
  if (!is.finite(upper[["criterion"]])) {
    stop(gettextf(
      "The upper search bound does not produce a valid sequential design: %1$s",
      upper[["error"]]
    ))
  }

  if (upper[["criterion"]] < 0)
    return(.evSeqTargetMaximumSampleSizeLimit(upper))

  lowerN <- minimumN
  upperN <- maximumN

  while ((upperN - lowerN) > 1) {
    midpoint <- floor((lowerN + upperN) / 2)
    current  <- .evSeqEvaluateTargetMaximumN(settings, target, midpoint, cache)

    if (is.finite(current[["criterion"]]) && current[["criterion"]] >= 0) {
      upperN <- midpoint
    } else {
      lowerN <- midpoint
    }
  }

  foundN <- upperN

  while (foundN > minimumN) {
    previous <- .evSeqEvaluateTargetMaximumN(settings, target, foundN - 1, cache)
    if (!is.finite(previous[["criterion"]]) || previous[["criterion"]] < 0)
      break

    foundN <- foundN - 1
  }

  while (foundN <= maximumN) {
    current <- .evSeqEvaluateTargetMaximumN(settings, target, foundN, cache)
    if (is.finite(current[["criterion"]]) && current[["criterion"]] >= 0)
      return(.evSeqSolvedTargetMaximumSampleSize(settings, target, foundN, cache))

    foundN <- foundN + 1
  }

  return(.evSeqTargetMaximumSampleSizeLimit(.evSeqEvaluateTargetMaximumN(settings, target, maximumN, cache)))
}

.evSeqEvaluateTargetMaximumN <- function(settings, target, maximumN, cache) {
  value <- .evSeqEvaluateMaximumN(settings, maximumN, cache)
  if (!is.null(value[["error"]]))
    return(list(criterion = NA_real_, error = value[["error"]]))

  targetResult <- .evSeqTargetResults(settings, value[["designResult"]], target)

  return(list(
    criterion    = targetResult[["actualProbability"]] - targetResult[["targetProbability"]],
    targetResult = targetResult,
    settings     = value[["settings"]],
    designResult = value[["designResult"]]
  ))
}

.evSeqEvaluateMaximumN <- function(settings, maximumN, cache) {
  key <- as.character(maximumN)
  if (exists(key, envir = cache, inherits = FALSE))
    return(get(key, envir = cache, inherits = FALSE))

  value <- try({
    candidateSettings <- .evSeqSettingsForMaximumN(settings, maximumN)
    designResult      <- .evSeqComputeDesignResult(candidateSettings)

    list(
      settings     = candidateSettings,
      designResult = designResult
    )
  }, silent = TRUE)

  if (jaspBase::isTryError(value))
    value <- list(error = .evCleanError(value))

  assign(key, value, envir = cache)
  return(value)
}

.evSeqSolvedTargetMaximumSampleSize <- function(settings, target, maximumN, cache) {
  evaluated <- .evSeqEvaluateTargetMaximumN(settings, target, maximumN, cache)

  return(list(
    maximumN     = maximumN,
    targetResult = .evSeqTargetResultWithMaximumN(evaluated[["targetResult"]], maximumN),
    settings     = evaluated[["settings"]],
    designResult = evaluated[["designResult"]]
  ))
}

.evSeqTargetMaximumSampleSizeLimit <- function(evaluated) {
  targetResult <- evaluated[["targetResult"]]
  targetResult[["actualProbability"]][!targetResult[["reached"]]] <- NA_real_

  return(list(
    maximumN     = max(evaluated[["settings"]][["n1Seq"]], na.rm = TRUE),
    targetResult = .evSeqTargetResultWithMaximumN(targetResult, NA_integer_),
    settings     = evaluated[["settings"]],
    designResult = evaluated[["designResult"]],
    limitReached = TRUE
  ))
}

.evSeqTargetResultWithMaximumN <- function(targetResult, maximumN) {
  targetResult[["maximumN"]] <- maximumN

  return(targetResult)
}

.evSeqTargetComputation <- function(result, target) {
  targetComputations <- result[["solver"]][["targetComputations"]]
  if (is.null(targetComputations) || is.null(targetComputations[[target]]))
    return(NULL)

  return(targetComputations[[target]])
}

.evSeqTargetSettings <- function(settings, result, target) {
  computation <- .evSeqTargetComputation(result, target)
  if (is.null(computation) || is.null(computation[["settings"]]))
    return(settings)

  return(computation[["settings"]])
}

.evSeqTargetDesignResult <- function(result, target) {
  computation <- .evSeqTargetComputation(result, target)
  if (is.null(computation) || is.null(computation[["designResult"]]))
    return(result)

  return(computation[["designResult"]])
}

.evSeqTargetDesign <- function(result, target) {
  targetResult <- .evSeqTargetDesignResult(result, target)
  if (target == "h0")
    return(targetResult[["null"]])

  return(targetResult[["design"]])
}

.evSeqTargetSampleSizeSchedule <- function(settings, result, target) {
  targetSettings <- .evSeqTargetSettings(settings, result, target)

  return(list(
    n1Seq = targetSettings[["n1Seq"]],
    n2Seq = targetSettings[["n2Seq"]]
  ))
}

.evSeqTargetResults <- function(settings, result, targets = settings[["planningTargets"]]) {
  rows <- lapply(targets, function(target) {
    design <- if (target == "h0") result[["null"]] else result[["design"]]
    targetPower <- .evTargetPower(settings, target)
    actualPower <- .evSeqFinalTargetProbability(design, target)
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

.evSeqDisplayMaximumFootnote <- function(result) {
  targetRows <- result[["solver"]][["targetResults"]]
  if (is.null(targetRows) || is.null(targetRows[["maximumN"]]))
    return(NULL)

  reached <- !is.na(targetRows[["maximumN"]])
  if (sum(reached) < 2 || length(unique(targetRows[["maximumN"]][reached])) < 2)
    return(NULL)

  return(gettext("Stagewise evidence tables and figures are evaluated through the largest computed maximum sample size."))
}

.evSeqSearchDesignMean <- function(settings, target = settings[["evidenceTarget"]]) {
  if (target == "h0")
    return(settings[["designNullPriorMeanRelative"]])

  return(settings[["designPriorMeanRelative"]])
}

.evSeqSearchDesignSd <- function(settings, target = settings[["evidenceTarget"]]) {
  if (target == "h0")
    return(settings[["designPriorUnderH0"]][["sd"]])

  return(settings[["designPriorSd"]])
}

.evSeqFinalTargetProbability <- function(design, target) {
  if (target == "h1")
    return(.evClampProbability(utils::tail(design[["cumpH1"]], 1)))

  return(.evClampProbability(utils::tail(design[["cumpH0"]], 1)))
}

.evSeqSettingsForMaximumN <- function(settings, maximumN) {
  maximumN <- as.integer(ceiling(maximumN))
  if (settings[["lookScheduleMode"]] == "increase") {
    n1Seq <- .evSeqSampleSizeIncreaseSchedule(settings, maximumN)
  } else {
    fractions <- .evSeqInformationFractions(settings)
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
    .evSeqValidateSecondGroupSampleSize(candidate[["n2Seq"]])
  candidate[["n1"]]    <- max(candidate[["n1Seq"]])
  candidate[["n2"]]    <- max(candidate[["n2Seq"]])

  return(candidate)
}

.evSeqInformationFractions <- function(settings) {
  if (settings[["lookScheduleMode"]] != "custom") {
    if (settings[["numberOfLooks"]] == 1)
      return(1)

    return(seq(settings[["informationFractionFirstLook"]], 1, length.out = settings[["numberOfLooks"]]))
  }

  fractions <- .evSeqParseNumericSchedule(settings[["informationFractionSchedule"]], gettext("information fraction schedule"))

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

.evSeqRunDesign <- function(settings, designPriorMean, designPriorSd) {
  if (settings[["isZTest"]])
    return(.evSeqRunZDesign(settings, designPriorMean, designPriorSd))

  return(.evSeqRunTDesign(settings, designPriorMean, designPriorSd))
}

.evSeqRunZDesign <- function(settings, designPriorMean, designPriorSd) {
  se              <- .evSeqStandardErrors(settings)
  analysisPrior   <- .evSeqZAnalysisPrior(settings)
  designPriorMean <- .evSeqDirectionalMean(settings, designPriorMean)

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

  if (!.evSeqUsesStandardErrorOnly(settings))
    args[["n"]] <- settings[["n1Seq"]]

  do.call(
    what = bfpwr::pbf01seq,
    args = c(args, .evSeqIntegrationArguments(settings))
  )
}

.evSeqRunTDesign <- function(settings, designPriorMean, designPriorSd) {
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
        drange      = .evSeqDrange(settings)
      ),
      .evSeqIntegrationArguments(settings)
    )
  )
}

.evSeqStandardErrors <- function(settings) {
  if (isTRUE(settings[["isGeneralZ"]]))
    return(.evSeqGeneralZStandardErrors(settings))

  if (settings[["isIndependentSamples"]])
    return(settings[["standardDeviation"]] * sqrt(1 / settings[["n1Seq"]] + 1 / settings[["n2Seq"]]))

  return(settings[["standardDeviation"]] / sqrt(settings[["n1Seq"]]))
}

.evSeqZAnalysisPrior <- function(settings) {
  if (isTRUE(settings[["isDirectionalZTest"]])) {
    return(list(
      type = "directional",
      pm   = .evSeqDirectionalMean(settings, settings[["analysisPriorMeanRelative"]]),
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

.evSeqUsesDirectionalZ <- function(settings) {
  if (!settings[["isZTest"]])
    return(FALSE)

  if (!settings[["analysisPriorDistribution"]] %in% c("normal", "directional"))
    return(FALSE)

  return(settings[["alternative"]] != "two.sided" || settings[["analysisPriorDistribution"]] == "directional")
}

.evSeqDirectionalMean <- function(settings, mean) {
  if (!isTRUE(settings[["isDirectionalZTest"]]) || settings[["alternative"]] != "less")
    return(mean)

  return(-mean)
}

.evSeqGeneralZStandardErrors <- function(settings) {
  if (!is.null(settings[["standardErrors"]]))
    return(settings[["standardErrors"]])

  if (settings[["generalZParameterization"]] == "standardErrorSchedule") {
    se <- .evSeqParseNumericSchedule(settings[["standardErrorSchedule"]], gettext("standard error schedule"))

    if (length(se) != length(settings[["n1Seq"]]))
      stop(gettext("The standard error schedule must have the same number of entries as the look schedule."))

    if (any(diff(se) >= 0))
      stop(gettext("The standard error schedule must be strictly decreasing."))

    return(se)
  }

  unitSd <- .evGeneralZUnitInformationSd(settings)

  return(unitSd / sqrt(settings[["n1Seq"]]))
}

.evSeqIntegrationArguments <- function(settings) {
  args <- list(method = settings[["integrationMethod"]])

  if (settings[["integrationMethod"]] == "pmvnorm") {
    args[["abseps"]] <- settings[["integrationAbsEps"]]
    args[["releps"]] <- settings[["integrationRelEps"]]
    args[["maxpts"]] <- settings[["integrationMaxPts"]]
  }

  return(args)
}

.evSeqDrange <- function(settings) {
  if (settings[["drangeMode"]] != "custom")
    return("adaptive")

  if (settings[["drangeLower"]] >= settings[["drangeUpper"]])
    stop(gettext("The lower t search bound must be smaller than the upper bound."))

  return(c(settings[["drangeLower"]], settings[["drangeUpper"]]))
}

.evSeqResultsTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceResults"]]))
    return()

  table <- createJaspTable(title = gettext("Bayes Factor Sequential Design"))
  table$dependOn(.evSeqDependencies)
  table$position <- 1
  jaspResults[["sequentialEvidenceResults"]] <- table

  .evSeqAddResultsColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute Bayes factor sequential design: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqResultsRow(settings, result))

  generalZUisdFootnote <- .evGeneralZKnownUisdFootnote(settings)
  if (!is.null(generalZUisdFootnote))
    table$addFootnote(generalZUisdFootnote)

  if (.evSeqUsesSampleSizeSearch(settings))
    table$addFootnote(gettext("Due to integer sample-size schedules, Pr(Conclusive Evidence) can exceed the target probability."))

  displayMaximumFootnote <- .evSeqDisplayMaximumFootnote(result)
  if (!is.null(displayMaximumFootnote))
    table$addFootnote(displayMaximumFootnote)

  unreachedTargetsFootnote <- .evSeqUnreachedTargetsFootnote(result)
  if (!is.null(unreachedTargetsFootnote))
    table$addFootnote(unreachedTargetsFootnote)

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested N\u2082/N\u2081 sample-size ratio, unless a custom group 2 schedule is supplied."))

  if (isTRUE(settings[["strictIntegration"]]) && length(settings[["n1Seq"]]) > 10)
    table$addFootnote(gettext("Exact integration over all regions can be slow for designs with many looks."))

  if (.evSeqUsesStandardErrorOnly(settings))
    table$addFootnote(gettext("This design uses the supplied standard error schedule, so sample-size summaries are omitted."))
}

.evSeqAddResultsColumns <- function(table, settings) {
  computed <- gettext("Computed")
  userDefined <- gettext("User Defined")

  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "target", title = gettext("Planned Target"), type = "string")

  if (.evSeqUsesSampleSizeSearch(settings)) {
    table$addColumnInfo(name = "targetProbability", title = gettext("Target Pr(Conclusive Evidence)"), type = "number", overtitle = userDefined)
  }

  table$addColumnInfo(name = "actualProbability", title = gettext("Pr(Conclusive Evidence)"), type = "number", overtitle = computed)

  if (.evSeqUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "finalSE", title = gettext("Final SE"), type = "number", overtitle = computed)
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "expectedN1", title = "N\u2081", type = "number", overtitle = gettext("Expected Sample Size"))
    table$addColumnInfo(name = "expectedN2", title = "N\u2082", type = "number", overtitle = gettext("Expected Sample Size"))
  } else {
    table$addColumnInfo(name = "expectedN", title = "N", type = "number", overtitle = gettext("Expected Sample Size"))
  }

  table$addColumnInfo(name = "threshold", title = gettext("BF threshold"), type = "number", overtitle = userDefined)
}

.evSeqResultsRow <- function(settings, result) {
  targetRows <- .evSeqMainTargetRows(settings, result)
  row <- data.frame(
    designPrior      = vapply(targetRows[["target"]], .evSeqTargetDesignPriorLabel, character(1)),
    target           = vapply(targetRows[["target"]], .evTargetLabel, character(1)),
    actualProbability = targetRows[["actualProbability"]],
    threshold        = vapply(targetRows[["target"]], function(target) .evThreshold(settings, target), numeric(1)),
    stringsAsFactors = FALSE
  )

  if (.evSeqUsesSampleSizeSearch(settings))
    row[["targetProbability"]] <- targetRows[["targetProbability"]]

  if (.evSeqUsesStandardErrorOnly(settings)) {
    row[["finalSE"]] <- utils::tail(.evSeqStandardErrors(settings), 1)
  } else {
    for (i in seq_len(nrow(targetRows))) {
      expected <- .evSeqExpectedSampleSizes(settings, result, targetRows[["target"]][i])
      if (settings[["isIndependentSamples"]]) {
        row[["expectedN1"]][i] <- expected[["n1"]][["mean"]]
        row[["expectedN2"]][i] <- expected[["n2"]][["mean"]]
      } else {
        row[["expectedN"]][i] <- expected[["n1"]][["mean"]]
      }
    }
  }

  if (.evSeqUsesSampleSizeSearch(settings))
    row <- .evSeqBlankUnreachedRows(row, result[["solver"]][["targetResults"]])

  columnOrder <- c(
    "designPrior", "target",
    if (.evSeqUsesSampleSizeSearch(settings)) "targetProbability",
    "actualProbability",
    if (.evSeqUsesStandardErrorOnly(settings)) "finalSE",
    if (!.evSeqUsesStandardErrorOnly(settings) && settings[["isIndependentSamples"]]) c("expectedN1", "expectedN2"),
    if (!.evSeqUsesStandardErrorOnly(settings) && !settings[["isIndependentSamples"]]) "expectedN",
    "threshold"
  )
  row <- row[, columnOrder, drop = FALSE]
  row.names(row) <- NULL

  return(row)
}

.evSeqSampleSizeSummaryTable <- function(jaspResults, settings, result) {
  if (.evSeqUsesStandardErrorOnly(settings))
    return()

  if (!is.null(jaspResults[["sequentialEvidenceSampleSizeSummary"]]))
    return()

  table <- createJaspTable(title = gettext("Sample Size Summary"))
  table$dependOn(.evSeqDependencies)
  table$position <- 2
  jaspResults[["sequentialEvidenceSampleSizeSummary"]] <- table

  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  if (.evSeqUsesSampleSizeSearch(settings))
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
    table$setError(gettextf("Unable to compute sample size summary: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqSampleSizeSummaryRows(settings, result))

  displayMaximumFootnote <- .evSeqDisplayMaximumFootnote(result)
  if (!is.null(displayMaximumFootnote))
    table$addFootnote(displayMaximumFootnote)

  unreachedTargetsFootnote <- .evSeqUnreachedTargetsFootnote(result)
  if (!is.null(unreachedTargetsFootnote))
    table$addFootnote(unreachedTargetsFootnote)
}

.evSeqSampleSizeSummaryRows <- function(settings, result) {
  targetRows <- .evSeqMainTargetRows(settings, result)
  if (.evSeqUsesSampleSizeSearch(settings)) {
    rows <- data.frame(
      designPrior = vapply(targetRows[["target"]], .evSeqTargetDesignPriorLabel, character(1)),
      target      = vapply(targetRows[["target"]], .evTargetLabel, character(1)),
      looks       = length(settings[["n1Seq"]]),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- data.frame(
      designPrior = vapply(targetRows[["target"]], .evSeqTargetDesignPriorLabel, character(1)),
      looks       = length(settings[["n1Seq"]]),
      stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(targetRows))) {
    target   <- targetRows[["target"]][i]
    expected <- .evSeqExpectedSampleSizes(settings, result, target)
    schedule <- .evSeqTargetSampleSizeSchedule(settings, result, target)
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

  if (.evSeqUsesSampleSizeSearch(settings))
    rows <- .evSeqBlankUnreachedRows(rows, result[["solver"]][["targetResults"]])

  row.names(rows) <- NULL

  return(rows)
}

.evSeqMainTargetRows <- function(settings, result) {
  if (.evSeqUsesSampleSizeSearch(settings))
    return(result[["solver"]][["targetResults"]])

  data.frame(
    target            = c("h1", "h0"),
    actualProbability = c(
      .evSeqFinalTargetProbability(result[["design"]], "h1"),
      .evSeqFinalTargetProbability(result[["null"]], "h0")
    ),
    stringsAsFactors = FALSE
  )
}

.evSeqExpectedSampleSizes <- function(settings, result, target) {
  schedule <- .evSeqTargetSampleSizeSchedule(settings, result, target)
  design   <- .evSeqTargetDesign(result, target)

  return(list(
    n1 = .evSeqExpectedSampleSize(design, schedule[["n1Seq"]]),
    n2 = .evSeqExpectedSampleSize(design, schedule[["n2Seq"]])
  ))
}

.evSeqTargetDesignPriorLabel <- function(target) {
  if (target == "h1")
    return(gettext("Under H\u2081 Design Prior"))

  return(gettext("Under H\u2080 Design Prior"))
}

.evSeqUnreachedTargetLabels <- function(result) {
  targetRows <- result[["solver"]][["targetResults"]]
  if (is.null(targetRows) || is.null(targetRows[["reached"]]))
    return(character(0))

  unreached <- targetRows[["target"]][!targetRows[["reached"]]]
  return(vapply(unreached, .evTargetLabel, character(1)))
}

.evSeqUnreachedTargetsFootnote <- function(result) {
  unreachedTargets <- .evSeqUnreachedTargetLabels(result)
  if (length(unreachedTargets) == 0)
    return(NULL)

  return(gettextf(
    "The requested BF target could not be reached within the selected maximum sample size for %1$s; entries that depend on reaching the target are left empty.",
    paste(unreachedTargets, collapse = ", ")
  ))
}

.evSeqBlankUnreachedRows <- function(row, targetRows) {
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

.evSeqExpectedSampleSize <- function(design, n) {
  stageStop <- diff(c(0, design[["cumpH1"]] + design[["cumpH0"]]))
  expected  <- sum(stageStop * n) + (1 - sum(stageStop)) * max(n)
  expected2 <- sum(stageStop * n^2) + (1 - sum(stageStop)) * max(n)^2
  variance  <- max(0, expected2 - expected^2)

  return(c(mean = expected, sd = sqrt(variance)))
}

.evSeqDesignOutcomeTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceDesignOutcome"]]))
    return()

  table <- createJaspTable(title = gettext("Design Evidence"))
  table$dependOn(.evSeqDependencies)
  table$position <- 3
  jaspResults[["sequentialEvidenceDesignOutcome"]] <- table

  table$addColumnInfo(name = "under",       title = gettext("Under"),       type = "string")
  table$addColumnInfo(name = "null",        title = gettext("Null"),         type = "number", overtitle = gettext("Bayes Factor Evidence"))
  table$addColumnInfo(name = "undecided",   title = gettext("Inconclusive"), type = "number", overtitle = gettext("Bayes Factor Evidence"))
  table$addColumnInfo(name = "alternative", title = gettext("Alternative"),  type = "number", overtitle = gettext("Bayes Factor Evidence"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute design evidence: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqDesignOutcomeRows(result))
  table$addFootnote(gettext("Probabilities are cumulative through the final look. Rows use the corresponding design prior under H\u2081 or H\u2080."))
}

.evSeqDesignOutcomeRows <- function(result) {
  h1Outcome <- .evSeqFinalOutcome(result[["design"]])
  h0Outcome <- .evSeqFinalOutcome(result[["null"]])

  return(data.frame(
    under       = c(gettext("H\u2081"), gettext("H\u2080")),
    null        = c(h1Outcome[["null"]],        h0Outcome[["null"]]),
    undecided   = c(h1Outcome[["undecided"]],   h0Outcome[["undecided"]]),
    alternative = c(h1Outcome[["alternative"]], h0Outcome[["alternative"]]),
    stringsAsFactors = FALSE
  ))
}

.evSeqFinalOutcome <- function(design) {
  return(c(
    null        = .evClampProbability(utils::tail(design[["cumpH0"]], 1)),
    undecided   = .evClampProbability(utils::tail(design[["cumpInc"]], 1)),
    alternative = .evClampProbability(utils::tail(design[["cumpH1"]], 1))
  ))
}

.evSeqStagewiseTotalTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceStagewiseTotal"]]))
    return()

  table <- createJaspTable(title = gettext("Total Stagewise Evidence"))
  table$dependOn(.evSeqDependencies)
  table$position <- 4
  jaspResults[["sequentialEvidenceStagewiseTotal"]] <- table

  .evSeqAddStagewiseBaseColumns(table, settings)

  h1Overtitle <- gettext("Under H\u2081 Design Prior")
  h0Overtitle <- gettext("Under H\u2080 Design Prior")
  table$addColumnInfo(name = "h1Alternative", title = gettext("Alternative"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1Undecided",   title = gettext("Inconclusive"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1Null",        title = gettext("Null"),        type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h0Alternative", title = gettext("Alternative"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0Undecided",   title = gettext("Inconclusive"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0Null",        title = gettext("Null"),        type = "number", overtitle = h0Overtitle)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute total stagewise evidence: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqStagewiseTotalRows(settings, result))
}

.evSeqStagewiseIncrementalTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceStagewiseIncremental"]]))
    return()

  table <- createJaspTable(title = gettext("Incremental Stagewise Stops"))
  table$dependOn(.evSeqDependencies)
  table$position <- 5
  jaspResults[["sequentialEvidenceStagewiseIncremental"]] <- table

  .evSeqAddStagewiseBaseColumns(table, settings)

  h1Overtitle <- gettext("Under H\u2081 Design Prior")
  h0Overtitle <- gettext("Under H\u2080 Design Prior")
  table$addColumnInfo(name = "h1AlternativeStop", title = gettext("Alternative"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1NullStop",        title = gettext("Null"),        type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1AnyStop",         title = gettext("Any"),         type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h0AlternativeStop", title = gettext("Alternative"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0NullStop",        title = gettext("Null"),        type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0AnyStop",         title = gettext("Any"),         type = "number", overtitle = h0Overtitle)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute incremental stagewise stops: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqStagewiseIncrementalRows(settings, result))
}

.evSeqAddStagewiseBaseColumns <- function(table, settings) {
  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  if (.evSeqUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "standardError", title = gettext("SE"), type = "number")
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer")
    table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer")
  } else {
    table$addColumnInfo(name = "n", title = "N", type = "integer")
  }
}

.evSeqStagewiseTotalRows <- function(settings, result) {
  rows <- .evSeqStagewiseBaseRows(settings)
  rows[["h1Alternative"]] <- result[["design"]][["cumpH1"]]
  rows[["h1Undecided"]]   <- result[["design"]][["cumpInc"]]
  rows[["h1Null"]]        <- result[["design"]][["cumpH0"]]
  rows[["h0Alternative"]] <- result[["null"]][["cumpH1"]]
  rows[["h0Undecided"]]   <- result[["null"]][["cumpInc"]]
  rows[["h0Null"]]        <- result[["null"]][["cumpH0"]]

  return(rows)
}

.evSeqStagewiseIncrementalRows <- function(settings, result) {
  h1AlternativeStop <- diff(c(0, result[["design"]][["cumpH1"]]))
  h1NullStop        <- diff(c(0, result[["design"]][["cumpH0"]]))
  h0AlternativeStop <- diff(c(0, result[["null"]][["cumpH1"]]))
  h0NullStop        <- diff(c(0, result[["null"]][["cumpH0"]]))

  rows <- .evSeqStagewiseBaseRows(settings)
  rows[["h1AlternativeStop"]] <- h1AlternativeStop
  rows[["h1NullStop"]]        <- h1NullStop
  rows[["h1AnyStop"]]         <- h1AlternativeStop + h1NullStop
  rows[["h0AlternativeStop"]] <- h0AlternativeStop
  rows[["h0NullStop"]]        <- h0NullStop
  rows[["h0AnyStop"]]         <- h0AlternativeStop + h0NullStop

  return(rows)
}

.evSeqStagewiseBaseRows <- function(settings) {
  rows <- data.frame(
    look = seq_along(settings[["n1Seq"]]),
    stringsAsFactors = FALSE
  )

  if (.evSeqUsesStandardErrorOnly(settings)) {
    rows[["standardError"]] <- .evSeqStandardErrors(settings)
  } else if (settings[["isIndependentSamples"]]) {
    rows[["n1"]] <- settings[["n1Seq"]]
    rows[["n2"]] <- settings[["n2Seq"]]
  } else {
    rows[["n"]] <- settings[["n1Seq"]]
  }

  return(rows)
}

.evSeqBoundariesTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceBoundaries"]]))
    return()

  table <- createJaspTable(title = gettext("Stopping Boundaries"))
  table$dependOn(.evSeqDependencies)
  table$position <- 6
  jaspResults[["sequentialEvidenceBoundaries"]] <- table

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  if (.evSeqUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "standardError", title = gettext("SE"), type = "number")
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer")
    table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer")
  } else {
    table$addColumnInfo(name = "n", title = "N", type = "integer")
  }

  table$addColumnInfo(name = "h1Lower", title = gettext("Lower"), type = "number", overtitle = gettext("BF\u2081\u2080 target"))
  table$addColumnInfo(name = "h1Upper", title = gettext("Upper"), type = "number", overtitle = gettext("BF\u2081\u2080 target"))
  table$addColumnInfo(name = "h0Lower", title = gettext("Lower"), type = "number", overtitle = gettext("BF\u2080\u2081 target"))
  table$addColumnInfo(name = "h0Upper", title = gettext("Upper"), type = "number", overtitle = gettext("BF\u2080\u2081 target"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stopping boundaries: %1$s", .evCleanError(result)))
    return()
  }

  rows <- .evSeqBoundaryRows(settings, result[["design"]])
  table$setData(rows)

  if (.evSeqHasEmptyBoundaryCells(rows))
    table$addFootnote(gettext("Empty boundary cells indicate that no finite test statistic value reaches the corresponding BF target for that look and tail under the current design settings."))
}

.evSeqBoundaryRows <- function(settings, design) {
  h1 <- .evSeqBoundaryMatrix(design[["zk1"]], design[["zk0"]])
  h0 <- .evSeqBoundaryMatrix(design[["zk0"]], design[["zk1"]])

  rows <- data.frame(
    look    = seq_along(settings[["n1Seq"]]),
    h1Lower = h1[1, ],
    h1Upper = h1[2, ],
    h0Lower = h0[1, ],
    h0Upper = h0[2, ],
    stringsAsFactors = FALSE
  )

  if (.evSeqUsesStandardErrorOnly(settings)) {
    rows[["standardError"]] <- .evSeqStandardErrors(settings)
  } else if (settings[["isIndependentSamples"]]) {
    rows[["n1"]] <- settings[["n1Seq"]]
    rows[["n2"]] <- settings[["n2Seq"]]
  } else {
    rows[["n"]] <- settings[["n1Seq"]]
  }

  return(rows)
}

.evSeqHasEmptyBoundaryCells <- function(rows) {
  boundaryColumns <- intersect(c("h1Lower", "h1Upper", "h0Lower", "h0Upper"), names(rows))
  if (length(boundaryColumns) == 0)
    return(FALSE)

  return(any(is.na(rows[, boundaryColumns, drop = FALSE])))
}

.evSeqBoundaryMatrix <- function(boundary, otherBoundary) {
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

.evSeqPriorsTable <- function(jaspResults, settings) {
  if (!is.null(jaspResults[["sequentialEvidencePriors"]]))
    return()

  table <- createJaspTable(title = gettext("Design Specification"))
  table$dependOn(.evSeqDependencies)
  table$position <- 7
  jaspResults[["sequentialEvidencePriors"]] <- table

  table$addColumnInfo(name = "hypothesis", title = gettext("Hypothesis"), type = "string")
  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "analysisPrior", title = gettext("Analysis Prior"), type = "string")

  table$setData(data.frame(
    hypothesis    = c(gettext("H\u2081"), gettext("H\u2080")),
    designPrior   = c(.evDesignPriorString(settings, "h1"), .evDesignPriorString(settings, "h0")),
    analysisPrior = c(.evAnalysisPriorString(settings), .evNullPriorString(settings)),
    stringsAsFactors = FALSE
  ))

  if (!identical(settings[["nullValue"]], 0))
    table$addFootnote(gettext("Sequential bfpwr calculations are performed on the parameter scale centered at the null value."))
}

.evSeqText <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceText"]]))
    return()

  html <- createJaspHtml(title = gettext("Explanation"))
  html$dependOn(c(.evSeqDependencies, "text"))
  html$position <- 9
  jaspResults[["sequentialEvidenceText"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested Bayes factor sequential design could not be completed with the current settings.")
    return()
  }

  outcome <- .evSeqFinalOutcome(result[["design"]])
  firstSentence <- if (.evSeqUsesSampleSizeSearch(settings)) {
    if (isTRUE(result[["solver"]][["limitReached"]])) {
      gettextf(
        "This group-sequential design is evaluated at the selected maximum sample size for %1$s at %2$s planned looks.",
        .evPlanningTargetText(settings),
        length(settings[["n1Seq"]])
      )
    } else {
      gettextf(
        "This group-sequential design finds separate maximum sample sizes for %1$s at %2$s planned looks.",
        .evPlanningTargetText(settings),
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
      .evFormatNumber(outcome[["alternative"]]),
      .evFormatNumber(outcome[["null"]]),
      .evFormatNumber(outcome[["undecided"]])
    ),
    "</p>"
  )
}

.evSeqReport <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceReport"]]))
    return()

  html <- createJaspHtml(title = gettext("Report"))
  html$dependOn(c(.evSeqDependencies, "generateReport", "generateReportLatex"))
  html$position <- 13
  jaspResults[["sequentialEvidenceReport"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The report could not be generated because the Bayes factor sequential design could not be completed with the current settings.")
    return()
  }

  paragraph <- paste(
    .evReportOpeningSentence(settings, designType = gettext("sequential")),
    .evReportThresholdSentence(settings),
    .evSeqReportSampleSizeSentence(settings, result),
    .evReportPriorSentence(settings),
    .evReportProbabilitySentence(
      settings,
      h1Outcome = .evSeqFinalOutcome(result[["design"]]),
      h0Outcome = .evSeqFinalOutcome(result[["null"]])
    ),
    .evReportSoftwareSentence()
  )

  html[["text"]] <- .evReportHtml(paragraph, settings)
}

.evSeqReportSampleSizeSentence <- function(settings, result) {
  if (.evSeqUsesStandardErrorOnly(settings)) {
    return(gettext(
      "The design used a fixed standard-error schedule, so n_min, n_max, batch size, expected sample size, and sample-size SD are not defined."
    ))
  }

  pieces <- c(
    if (.evSeqUsesSampleSizeSearch(settings)) .evSeqReportTargetSentence(settings),
    .evSeqReportScheduleSentence(settings, result),
    .evSeqReportExpectedSampleSizeText(settings, result)
  )

  paste(pieces, collapse = " ")
}

.evSeqReportTargetSentence <- function(settings) {
  gettextf(
    "The design targeted conclusive evidence probabilities of %1$s under H\u2081 and %2$s under H\u2080.",
    .evReportPercent(.evTargetPower(settings, "h1")),
    .evReportPercent(.evTargetPower(settings, "h0"))
  )
}

.evSeqReportScheduleSentence <- function(settings, result = NULL) {
  if (.evSeqUsesSampleSizeSearch(settings) && !is.null(result))
    return(.evSeqReportSearchedScheduleSentence(settings, result))

  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "The design started at n_min,1 = %1$s and n_min,2 = %2$s, used %3$s, and stopped at n_max,1 = %4$s and n_max,2 = %5$s.",
      .evReportNumber(settings[["n1Seq"]][1]),
      .evReportNumber(settings[["n2Seq"]][1]),
      .evSeqReportBatchText(settings),
      .evReportNumber(max(settings[["n1Seq"]], na.rm = TRUE)),
      .evReportNumber(max(settings[["n2Seq"]], na.rm = TRUE))
    ))
  }

  return(gettextf(
    "The design started at n_min = %1$s, used %2$s, and stopped at n_max = %3$s.",
    .evReportNumber(settings[["n1Seq"]][1]),
    .evSeqReportBatchText(settings),
    .evReportNumber(max(settings[["n1Seq"]], na.rm = TRUE))
  ))
}

.evSeqReportSearchedScheduleSentence <- function(settings, result) {
  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "The design started at n_min,1 = %1$s and n_min,2 = %2$s, used %3$s, and found maximum sample sizes n_max,1 = %4$s and n_max,2 = %5$s under H\u2081 and n_max,1 = %6$s and n_max,2 = %7$s under H\u2080.",
      .evReportNumber(settings[["n1Seq"]][1]),
      .evReportNumber(settings[["n2Seq"]][1]),
      .evSeqReportBatchText(settings),
      .evSeqReportTargetMaximumText(settings, result, "h1", "n1"),
      .evSeqReportTargetMaximumText(settings, result, "h1", "n2"),
      .evSeqReportTargetMaximumText(settings, result, "h0", "n1"),
      .evSeqReportTargetMaximumText(settings, result, "h0", "n2")
    ))
  }

  return(gettextf(
    "The design started at n_min = %1$s, used %2$s, and found maximum sample sizes n_max = %3$s under H\u2081 and n_max = %4$s under H\u2080.",
    .evReportNumber(settings[["n1Seq"]][1]),
    .evSeqReportBatchText(settings),
    .evSeqReportTargetMaximumText(settings, result, "h1", "n1"),
    .evSeqReportTargetMaximumText(settings, result, "h0", "n1")
  ))
}

.evSeqReportTargetMaximumText <- function(settings, result, target, sampleSize = "n1") {
  targetRows <- result[["solver"]][["targetResults"]]
  index <- which(targetRows[["target"]] == target)
  if (length(index) == 0 || is.na(targetRows[["maximumN"]][index[1]]))
    return(gettext("not reached"))

  schedule <- .evSeqTargetSampleSizeSchedule(settings, result, target)
  n <- if (sampleSize == "n2") max(schedule[["n2Seq"]], na.rm = TRUE) else max(schedule[["n1Seq"]], na.rm = TRUE)

  return(.evReportNumber(n))
}

.evSeqReportRangeText <- function(settings) {
  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "minimum sample sizes n_min,1 = %1$s and n_min,2 = %2$s and maximum sample sizes n_max,1 = %3$s and n_max,2 = %4$s",
      .evReportNumber(settings[["n1Seq"]][1]),
      .evReportNumber(settings[["n2Seq"]][1]),
      .evReportNumber(max(settings[["n1Seq"]], na.rm = TRUE)),
      .evReportNumber(max(settings[["n2Seq"]], na.rm = TRUE))
    ))
  }

  return(gettextf(
    "minimum sample size n_min = %1$s and maximum sample size n_max = %2$s",
    .evReportNumber(settings[["n1Seq"]][1]),
    .evReportNumber(max(settings[["n1Seq"]], na.rm = TRUE))
  ))
}

.evSeqReportBatchText <- function(settings) {
  n1Increments <- diff(settings[["n1Seq"]])

  if (length(n1Increments) == 0)
    return(gettext("a single planned look"))

  if (settings[["isIndependentSamples"]]) {
    n2Increments <- diff(settings[["n2Seq"]])
    if (.evSeqReportConstantIncrements(n1Increments) && .evSeqReportConstantIncrements(n2Increments)) {
      return(gettextf(
        "batches of %1$s for n\u2081 and %2$s for n\u2082",
        .evReportNumber(n1Increments[1]),
        .evReportNumber(n2Increments[1])
      ))
    }

    return(gettextf(
      "batch-size increments of %1$s for n\u2081 and %2$s for n\u2082",
      .evSeqReportVectorText(n1Increments),
      .evSeqReportVectorText(n2Increments)
    ))
  }

  if (.evSeqReportConstantIncrements(n1Increments))
    return(gettextf("batches of %1$s", .evReportNumber(n1Increments[1])))

  return(gettextf("batch-size increments of %1$s", .evSeqReportVectorText(n1Increments)))
}

.evSeqReportExpectedSampleSizeText <- function(settings, result) {
  h1 <- .evSeqExpectedSampleSizes(settings, result, "h1")
  h0 <- .evSeqExpectedSampleSizes(settings, result, "h0")

  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "Under the H\u2081 design prior, the expected sample sizes were n\u2081 = %1$s (SD = %2$s) and n\u2082 = %3$s (SD = %4$s); under the H\u2080 design prior, the expected sample sizes were n\u2081 = %5$s (SD = %6$s) and n\u2082 = %7$s (SD = %8$s).",
      .evReportMomentNumber(h1[["n1"]][["mean"]]),
      .evReportMomentNumber(h1[["n1"]][["sd"]]),
      .evReportMomentNumber(h1[["n2"]][["mean"]]),
      .evReportMomentNumber(h1[["n2"]][["sd"]]),
      .evReportMomentNumber(h0[["n1"]][["mean"]]),
      .evReportMomentNumber(h0[["n1"]][["sd"]]),
      .evReportMomentNumber(h0[["n2"]][["mean"]]),
      .evReportMomentNumber(h0[["n2"]][["sd"]])
    ))
  }

  return(gettextf(
    "Under the H\u2081 design prior, the expected sample size was %1$s (SD = %2$s); under the H\u2080 design prior, the expected sample size was %3$s (SD = %4$s).",
    .evReportMomentNumber(h1[["n1"]][["mean"]]),
    .evReportMomentNumber(h1[["n1"]][["sd"]]),
    .evReportMomentNumber(h0[["n1"]][["mean"]]),
    .evReportMomentNumber(h0[["n1"]][["sd"]])
  ))
}

.evSeqReportConstantIncrements <- function(increments) {
  length(unique(increments)) == 1
}

.evSeqReportVectorText <- function(x) {
  if (length(x) <= 6)
    return(paste(vapply(x, .evReportNumber, character(1)), collapse = ", "))

  return(gettextf(
    "%1$s to %2$s",
    .evReportNumber(min(x, na.rm = TRUE)),
    .evReportNumber(max(x, na.rm = TRUE))
  ))
}

.evSeqRCode <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceRCode"]]))
    return()

  html <- createJaspHtml(title = gettext("R Code"))
  html$dependOn(c(.evSeqDependencies, "generateRCode"))
  html$position <- 15
  jaspResults[["sequentialEvidenceRCode"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("R code could not be generated because the sequential design could not be computed.")
    return()
  }

  code <- try(.evSeqBfpwrCall(settings, result), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .evCleanError(code))
    return()
  }

  html[["text"]] <- .evCodeHtml(code)
}

.evSeqBfpwrCall <- function(settings, result = NULL) {
  if (.evSeqUsesSampleSizeSearch(settings)) {
    calls <- vapply(settings[["planningTargets"]], function(target) {
      targetSettings <- .evSeqRCodeTargetSettings(settings, result, target)
      prefix <- if (target == "h1") "# Plan for evidence for H1" else "# Plan for evidence for H0"
      call <- if (targetSettings[["isZTest"]]) .evSeqZBfpwrCall(targetSettings, target) else .evSeqTBfpwrCall(targetSettings, target)
      paste(prefix, call, sep = "\n")
    }, character(1))

    return(paste(calls, collapse = "\n\n"))
  }

  if (settings[["isZTest"]])
    return(.evSeqZBfpwrCall(settings))

  return(.evSeqTBfpwrCall(settings))
}

.evSeqRCodeTargetSettings <- function(settings, result, target) {
  if (is.null(result))
    return(settings)

  return(.evSeqTargetSettings(settings, result, target))
}

.evSeqZBfpwrCall <- function(settings, target = settings[["evidenceTarget"]]) {
  analysisPrior <- .evSeqZAnalysisPrior(settings)
  dpm           <- .evSeqDirectionalMean(settings, .evSeqSearchDesignMean(settings, target))

  args <- list(
    k1 = settings[["k1"]],
    k0 = settings[["k0"]],
    se = .evSeqStandardErrors(settings)
  )

  if (!.evSeqUsesStandardErrorOnly(settings))
    args[["n"]] <- settings[["n1Seq"]]

  args <- c(
    args,
    list(
      pm     = analysisPrior[["pm"]],
      psd    = analysisPrior[["psd"]],
      dpm    = dpm,
      dpsd   = .evSeqSearchDesignSd(settings, target),
      type   = analysisPrior[["type"]],
      strict = settings[["strictIntegration"]]
    ),
    .evSeqIntegrationArguments(settings)
  )

  return(.evFormatRCall("bfpwr::pbf01seq", args))
}

.evSeqTBfpwrCall <- function(settings, target = settings[["evidenceTarget"]]) {
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
      dpm         = .evSeqSearchDesignMean(settings, target),
      dpsd        = .evSeqSearchDesignSd(settings, target),
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      strict      = settings[["strictIntegration"]],
      drange      = .evSeqDrange(settings)
    ),
    .evSeqIntegrationArguments(settings)
  )

  return(.evFormatRCall("bfpwr::ptbf01seq", args))
}

.evSeqStoppingProbabilitiesPlot <- function(jaspResults, settings, result) {
  for (spec in .evSeqUnderPlotSpecs(settings, "sequentialEvidenceStoppingProbabilities", gettext("Stopping Probabilities"), 10)) {
    .evSeqStoppingProbabilitiesOutcomePlot(
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

.evSeqStoppingProbabilitiesOutcomePlot <- function(jaspResults, settings, result, key, title, position, under) {
  if (!is.null(jaspResults[[key]]))
    return()

  plot <- createJaspPlot(title = title, width = 735, height = 350)
  plot$dependOn(c(.evSeqDependencies, "stoppingProbabilitiesPlot", "showNullReference", "mergeH1H0Figures"))
  plot$position <- position
  jaspResults[[key]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute stopping probabilities: %1$s", .evCleanError(result)))
    return()
  }

  plotResult <- try(.evSeqBuildStoppingProbabilitiesPlot(settings, result, under), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute stopping probabilities: %1$s", .evCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.evSeqUnderPlotSpecs <- function(settings, keyPrefix, title, position) {
  if (isTRUE(settings[["mergeH1H0Figures"]])) {
    return(list(list(
      key      = paste0(keyPrefix, "Plot"),
      title    = title,
      position = position,
      under    = NULL
    )))
  }

  unders <- .evSeqPlotUnders(settings)
  specs <- lapply(seq_along(unders), function(i) {
    under <- unders[i]
    list(
      key      = paste0(keyPrefix, .evUnderKeySuffix(under), "Plot"),
      title    = .evPlotTitleUnder(title, under),
      position = position + i - 1,
      under    = under
    )
  })

  return(specs)
}

.evSeqPlotUnders <- function(settings, under = NULL) {
  if (!is.null(under))
    return(under)

  unders <- "h1"
  if (isTRUE(settings[["showNullReference"]]))
    unders <- c(unders, "h0")

  return(unders)
}

.evSeqDesignForUnder <- function(result, under) {
  if (under == "h0")
    return(result[["null"]])

  return(result[["design"]])
}

.evSeqBuildStoppingProbabilitiesPlot <- function(settings, result, under = NULL) {
  data       <- .evSeqStoppingProbabilityPlotData(settings, result, under)
  xLabel     <- .evSeqLookAxisLabel(settings)
  showUnder  <- length(unique(data[["under"]])) > 1

  if (showUnder) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome, linetype = under)) +
      ggplot2::geom_line(size = 1.1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Evidence"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome)) +
      ggplot2::geom_line(size = 1.1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Evidence"))
  }

  if (.evSeqUsesSampleSizeSearch(settings)) {
    targets <- .evSeqPlotTargets(settings, under)
    if (length(targets) > 0) {
      plot <- plot + ggplot2::geom_hline(yintercept = unique(vapply(targets, function(target) .evTargetPower(settings, target), numeric(1))), linetype = "dotted", color = "#555555")
    }
  }

  return(.pwrApplyPlotTheme(plot))
}

.evSeqPlotTargets <- function(settings, under = NULL) {
  if (is.null(under))
    return(settings[["planningTargets"]])

  return(intersect(settings[["planningTargets"]], under))
}

.evSeqStoppingProbabilityPlotData <- function(settings, result, under = NULL) {
  unders <- .evSeqPlotUnders(settings, under)
  rows   <- lapply(unders, function(under) {
    .evSeqStoppingProbabilityRows(settings, .evSeqDesignForUnder(result, under), under)
  })

  return(do.call(rbind, rows))
}

.evSeqStoppingProbabilityRows <- function(settings, design, under) {
  h1Outcome <- if (under == "h1") gettext("Conclusive") else gettext("Misleading")
  h0Outcome <- if (under == "h0") gettext("Conclusive") else gettext("Misleading")

  data.frame(
    n           = rep(.evSeqLookAxisValues(settings), 3),
    probability = c(design[["cumpH1"]], design[["cumpInc"]], design[["cumpH0"]]),
    outcome     = rep(c(h1Outcome, gettext("Inconclusive"), h0Outcome), each = length(settings[["n1Seq"]])),
    under       = .evUnderLabel(under),
    stringsAsFactors = FALSE
  )
}

.evSeqStoppingBoundariesPlot <- function(jaspResults, settings, result) {
  .evSeqClearSplitStoppingBoundaryPlots(jaspResults)

  .evSeqStoppingBoundariesOutcomePlot(
    jaspResults = jaspResults,
    settings    = settings,
    result      = result,
    key         = "sequentialEvidenceStoppingBoundariesPlot",
    title       = gettext("Stopping Boundaries"),
    position    = 12
  )
}

.evSeqClearSplitStoppingBoundaryPlots <- function(jaspResults) {
  for (key in c("sequentialEvidenceStoppingBoundariesH1Plot", "sequentialEvidenceStoppingBoundariesH0Plot")) {
    if (!is.null(jaspResults[[key]]))
      jaspResults[[key]] <- NULL
  }
}

.evSeqStoppingBoundariesOutcomePlot <- function(jaspResults, settings, result, key, title, position) {
  if (!is.null(jaspResults[[key]]))
    return()

  plot <- createJaspPlot(title = title, width = 735, height = 350)
  plot$dependOn(c(.evSeqDependencies, "stoppingBoundariesPlot"))
  plot$position <- position
  jaspResults[[key]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute stopping boundaries: %1$s", .evCleanError(result)))
    return()
  }

  plotResult <- try(.evSeqBuildStoppingBoundariesPlot(settings, result), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute stopping boundaries: %1$s", .evCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.evSeqBuildStoppingBoundariesPlot <- function(settings, result) {
  data <- .evSeqBoundaryPlotData(settings, result[["design"]])

  if (nrow(data) == 0)
    stop(gettext("No finite stopping boundaries are available for the current settings."))

  xLabel <- .evSeqLookAxisLabel(settings)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = criticalValue, color = target, linetype = boundary)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = xLabel, y = gettext("Critical value"), color = gettext("Target"), linetype = gettext("Boundary")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "#666666")

  return(.pwrApplyPlotTheme(plot))
}

.evSeqBoundaryPlotData <- function(settings, design) {
  rows <- .evSeqBoundaryRows(settings, design)
  n    <- .evSeqLookAxisValues(settings)

  data <- rbind(
    data.frame(n = n, criticalValue = rows[["h1Lower"]], target = gettext("H\u2081 (BF\u2081\u2080)"), boundary = gettext("Lower"), stringsAsFactors = FALSE),
    data.frame(n = n, criticalValue = rows[["h1Upper"]], target = gettext("H\u2081 (BF\u2081\u2080)"), boundary = gettext("Upper"), stringsAsFactors = FALSE),
    data.frame(n = n, criticalValue = rows[["h0Lower"]], target = gettext("H\u2080 (BF\u2080\u2081)"), boundary = gettext("Lower"), stringsAsFactors = FALSE),
    data.frame(n = n, criticalValue = rows[["h0Upper"]], target = gettext("H\u2080 (BF\u2080\u2081)"), boundary = gettext("Upper"), stringsAsFactors = FALSE)
  )

  data <- data[is.finite(data[["criticalValue"]]), , drop = FALSE]

  return(data)
}

.evSeqLookAxisValues <- function(settings) {
  if (.evSeqUsesStandardErrorOnly(settings))
    return(seq_along(settings[["n1Seq"]]))

  if (settings[["isIndependentSamples"]])
    return(settings[["n1Seq"]])

  return(settings[["n1Seq"]])
}

.evSeqLookAxisLabel <- function(settings) {
  if (.evSeqUsesStandardErrorOnly(settings))
    return(gettext("Look"))

  if (settings[["isIndependentSamples"]])
    return(gettext("Sample size (group 1)"))

  return(gettext("Sample size"))
}

.evSeqPriorPlot <- function(jaspResults, settings) {
  .evPriorPlotContainer(
    jaspResults  = jaspResults,
    settings     = settings,
    key          = "sequentialEvidencePriorPlot",
    position     = 14,
    dependencies = .evSeqDependencies
  )
}
