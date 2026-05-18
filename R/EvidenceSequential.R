EvidenceSequential <- function(jaspResults, dataset, options) {
  settings    <- .evSeqPrepareSettings(options)
  computation <- .evSeqCachedComputation(jaspResults, settings)
  settings    <- computation[["settings"]]
  result      <- computation[["result"]]

  .evSeqResultsTable(jaspResults, settings, result)
  .evSeqDesignOutcomeTable(jaspResults, settings, result)
  .evSeqStagewiseTable(jaspResults, settings, result)
  .evSeqBoundariesTable(jaspResults, settings, result)
  .evSeqPriorsTable(jaspResults, settings)

  if (isTRUE(options[["text"]]))
    .evSeqText(jaspResults, settings, result)

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
  "evidenceProbability", "alternative", "lookScheduleMode",
  "numberOfLooks", "sampleSizeFirstLook", "sampleSize", "sampleSizeSchedule",
  "sampleSizeSecondGroupSchedule", "sampleSizeRatio", "informationFractionFirstLook",
  "informationFractionSchedule", "sampleSizeRangeMin", "sampleSizeRangeMax",
  "standardDeviation",
  "generalZParameterization", "unitInformationSd", "standardErrorSchedule",
  "nullPriorDistribution", "nullValue", "analysisPriorDistribution",
  "analysisPriorPoint", "analysisPriorMean", "analysisPriorSd",
  "momentPriorSpread", "momentPriorMode", "tPriorLocation", "tPriorScale",
  "tPriorDf", "designPrior", "designPriorMean", "designPriorSd",
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
  settings[["plotPoints"]]        <- currentSettings[["plotPoints"]]
  settings[["showNullReference"]] <- currentSettings[["showNullReference"]]

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
    evidenceTarget       = options[["evidenceTarget"]],
    targetProbability    = options[["evidenceProbability"]],
    k1                   = 1 / options[["bf10Threshold"]],
    k0                   = options[["bf01Threshold"]],
    lookScheduleMode     = options[["lookScheduleMode"]],
    numberOfLooks        = options[["numberOfLooks"]],
    sampleSizeFirstLook  = options[["sampleSizeFirstLook"]],
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
    showNullReference    = options[["showNullReference"]]
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

  settings[["designPrior"]]     <- options[["designPrior"]]
  settings[["designPriorMean"]] <- options[["designPriorMean"]]
  settings[["designPriorSd"]]   <- if (options[["designPrior"]] == "point") 0 else options[["designPriorSd"]]

  settings[["designPriorMeanRelative"]] <- settings[["designPriorMean"]] - settings[["nullValue"]]

  if (settings[["isZTest"]]) {
    analysisPriorDistribution <- options[["analysisPriorDistribution"]]
    if (settings[["alternative"]] != "two.sided")
      analysisPriorDistribution <- "normal"

    settings[["analysisPriorDistribution"]] <- analysisPriorDistribution
    settings[["analysisPriorMean"]]         <- if (analysisPriorDistribution == "point") options[["analysisPriorPoint"]] else options[["analysisPriorMean"]]
    settings[["analysisPriorSd"]]           <- if (analysisPriorDistribution == "point") 0 else options[["analysisPriorSd"]]
    settings[["analysisPriorMeanRelative"]] <- settings[["analysisPriorMean"]] - settings[["nullValue"]]
    settings[["momentPriorSpread"]]         <- if (analysisPriorDistribution == "normalMomentMode") {
      options[["momentPriorMode"]] / sqrt(2)
    } else {
      options[["momentPriorSpread"]]
    }
    settings[["momentPriorMode"]] <- sqrt(2) * settings[["momentPriorSpread"]]
    settings[["isDirectionalZTest"]] <- .evSeqUsesDirectionalZ(settings)
  } else {
    settings[["analysisPriorDistribution"]] <- "t"
    settings[["tPriorLocation"]]            <- options[["tPriorLocation"]]
    settings[["tPriorScale"]]               <- options[["tPriorScale"]]
    settings[["tPriorDf"]]                  <- options[["tPriorDf"]]
    settings[["tPriorLocationRelative"]]    <- settings[["tPriorLocation"]] - settings[["nullValue"]]
    settings[["isDirectionalZTest"]]        <- FALSE
  }

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
    designPriorMean = 0,
    designPriorSd   = 0
  )

  return(list(
    design = design,
    null   = null
  ))
}

.evSeqFindMaximumSampleSize <- function(settings) {
  if (isTRUE(settings[["isGeneralZ"]]) && settings[["generalZParameterization"]] == "standardErrorSchedule")
    stop(gettext("Maximum sample size search is not available with a fixed standard error schedule."))

  minimumN <- ceiling(settings[["rangeMin"]])
  maximumN <- ceiling(settings[["rangeMax"]])

  if (minimumN >= maximumN)
    stop(gettext("The lower search bound must be smaller than the upper search bound."))

  cache <- new.env(parent = emptyenv())
  evaluate <- function(maximumN) {
    key <- as.character(maximumN)
    if (exists(key, envir = cache, inherits = FALSE))
      return(get(key, envir = cache, inherits = FALSE))

    value <- try({
      candidateSettings <- .evSeqSettingsForMaximumN(settings, maximumN)
      design <- .evSeqRunDesign(
        candidateSettings,
        designPriorMean = candidateSettings[["designPriorMeanRelative"]],
        designPriorSd   = candidateSettings[["designPriorSd"]]
      )

      list(
        probability = .evSeqFinalTargetProbability(design, settings[["evidenceTarget"]]),
        settings    = candidateSettings,
        design      = design
      )
    }, silent = TRUE)

    if (jaspBase::isTryError(value)) {
      value <- list(
        probability = NA_real_,
        error       = .evCleanError(value)
      )
    }

    assign(key, value, envir = cache)
    return(value)
  }

  lower <- evaluate(minimumN)
  if (is.finite(lower[["probability"]]) && lower[["probability"]] >= settings[["targetProbability"]])
    return(.evSeqSolvedMaximumSampleSizeResult(settings, minimumN))

  upper <- evaluate(maximumN)
  if (!is.finite(upper[["probability"]])) {
    stop(gettextf(
      "The upper search bound does not produce a valid sequential design: %1$s",
      upper[["error"]]
    ))
  }

  if (upper[["probability"]] < settings[["targetProbability"]])
    stop(gettext("Target evidence probability is not reached within the selected maximum sample-size range."))

  lowerN <- minimumN
  upperN <- maximumN

  while ((upperN - lowerN) > 1) {
    midpoint <- floor((lowerN + upperN) / 2)
    current  <- evaluate(midpoint)

    if (is.finite(current[["probability"]]) && current[["probability"]] >= settings[["targetProbability"]]) {
      upperN <- midpoint
    } else {
      lowerN <- midpoint
    }
  }

  foundN <- upperN

  while (foundN > minimumN) {
    previous <- evaluate(foundN - 1)
    if (!is.finite(previous[["probability"]]) || previous[["probability"]] < settings[["targetProbability"]])
      break

    foundN <- foundN - 1
  }

  while (foundN <= maximumN) {
    current <- evaluate(foundN)
    if (is.finite(current[["probability"]]) && current[["probability"]] >= settings[["targetProbability"]])
      return(.evSeqSolvedMaximumSampleSizeResult(settings, foundN))

    foundN <- foundN + 1
  }

  stop(gettext("Target evidence probability is not reached within the selected maximum sample-size range."))
}

.evSeqSolvedMaximumSampleSizeResult <- function(settings, maximumN) {
  solvedSettings <- .evSeqSettingsForMaximumN(settings, maximumN)
  result <- .evSeqComputeDesignResult(solvedSettings)

  result[["settings"]] <- solvedSettings
  result[["solver"]] <- list(
    maximumN          = maximumN,
    target            = settings[["evidenceTarget"]],
    targetProbability = settings[["targetProbability"]],
    actualProbability = .evSeqFinalTargetProbability(result[["design"]], settings[["evidenceTarget"]])
  )

  return(result)
}

.evSeqFinalTargetProbability <- function(design, target) {
  if (target == "h1")
    return(.evClampProbability(utils::tail(design[["cumpH1"]], 1)))

  return(.evClampProbability(utils::tail(design[["cumpH0"]], 1)))
}

.evSeqSettingsForMaximumN <- function(settings, maximumN) {
  maximumN <- as.integer(ceiling(maximumN))
  fractions <- .evSeqInformationFractions(settings)
  n1Seq <- as.integer(ceiling(maximumN * fractions - sqrt(.Machine$double.eps)))
  n1Seq[length(n1Seq)] <- maximumN

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

  unitSd <- if (settings[["generalZParameterization"]] == "unitInformationSd") settings[["unitInformationSd"]] else 1

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

  table <- createJaspTable(title = gettext("Sequential Evidence Analysis"))
  table$dependOn(.evSeqDependencies)
  table$position <- 1
  jaspResults[["sequentialEvidenceResults"]] <- table

  .evSeqAddResultsColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute sequential evidence results: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqResultsRow(settings, result))

  if (.evSeqUsesSampleSizeSearch(settings))
    table$addFootnote(gettext("Due to integer sample-size schedules, the actual evidence probability can exceed the target probability."))

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested N\u2082/N\u2081 sample-size ratio, unless a custom group 2 schedule is supplied."))

  if (isTRUE(settings[["strictIntegration"]]) && length(settings[["n1Seq"]]) > 10)
    table$addFootnote(gettext("Exact integration over all regions can be slow for designs with many looks."))

  if (.evSeqUsesStandardErrorOnly(settings))
    table$addFootnote(gettext("This design uses the supplied standard error schedule, so sample-size summaries are omitted."))
}

.evSeqAddResultsColumns <- function(table, settings) {
  design <- gettext("Design")
  expectedH1 <- gettext("Expected Sample Size Under H\u2081")
  expectedH0 <- gettext("Expected Sample Size Under H\u2080")
  thresholds <- gettext("Evidence Thresholds")
  userDefined <- gettext("User Defined")

  table$addColumnInfo(name = "test", title = gettext("Test"), type = "string", overtitle = design)
  table$addColumnInfo(name = "looks", title = gettext("Looks"), type = "integer", overtitle = design)

  if (.evSeqUsesSampleSizeSearch(settings)) {
    table$addColumnInfo(name = "target", title = gettext("Evidence"), type = "string", overtitle = userDefined)
    table$addColumnInfo(name = "targetProbability", title = gettext("Target probability"), type = "number", overtitle = userDefined)
    table$addColumnInfo(name = "actualProbability", title = gettext("Actual probability"), type = "number", overtitle = userDefined)
  }

  if (.evSeqUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "finalSE", title = gettext("Final SE"), type = "number", overtitle = design)
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "maximumN1", title = "N\u2081", type = "integer", overtitle = gettext("Maximum Sample Size"))
    table$addColumnInfo(name = "maximumN2", title = "N\u2082", type = "integer", overtitle = gettext("Maximum Sample Size"))
    table$addColumnInfo(name = "expectedN1", title = "N\u2081", type = "number", overtitle = expectedH1)
    table$addColumnInfo(name = "expectedN2", title = "N\u2082", type = "number", overtitle = expectedH1)
    table$addColumnInfo(name = "sdN1", title = gettext("SD N\u2081"), type = "number", overtitle = expectedH1)
    table$addColumnInfo(name = "sdN2", title = gettext("SD N\u2082"), type = "number", overtitle = expectedH1)
    table$addColumnInfo(name = "expectedN1Null", title = "N\u2081", type = "number", overtitle = expectedH0)
    table$addColumnInfo(name = "expectedN2Null", title = "N\u2082", type = "number", overtitle = expectedH0)
    table$addColumnInfo(name = "sdN1Null", title = gettext("SD N\u2081"), type = "number", overtitle = expectedH0)
    table$addColumnInfo(name = "sdN2Null", title = gettext("SD N\u2082"), type = "number", overtitle = expectedH0)
  } else {
    table$addColumnInfo(name = "maximumN", title = "N", type = "integer", overtitle = gettext("Maximum Sample Size"))
    table$addColumnInfo(name = "expectedN", title = "N", type = "number", overtitle = expectedH1)
    table$addColumnInfo(name = "sdN", title = gettext("SD N"), type = "number", overtitle = expectedH1)
    table$addColumnInfo(name = "expectedNNull", title = "N", type = "number", overtitle = expectedH0)
    table$addColumnInfo(name = "sdNNull", title = gettext("SD N"), type = "number", overtitle = expectedH0)
  }

  table$addColumnInfo(name = "bf10", title = "BF\u2081\u2080", type = "number", overtitle = thresholds)
  table$addColumnInfo(name = "bf01", title = "BF\u2080\u2081", type = "number", overtitle = thresholds)
}

.evSeqResultsRow <- function(settings, result) {
  design <- result[["design"]]
  null   <- result[["null"]]
  row <- data.frame(
    test   = settings[["testLabel"]],
    looks  = length(settings[["n1Seq"]]),
    bf10   = settings[["bf10Threshold"]],
    bf01   = settings[["bf01Threshold"]],
    stringsAsFactors = FALSE
  )

  if (.evSeqUsesSampleSizeSearch(settings)) {
    row[["target"]] <- .evTargetLabel(settings[["evidenceTarget"]])
    row[["targetProbability"]] <- settings[["targetProbability"]]
    row[["actualProbability"]] <- result[["solver"]][["actualProbability"]]
  }

  if (.evSeqUsesStandardErrorOnly(settings)) {
    row[["finalSE"]] <- utils::tail(.evSeqStandardErrors(settings), 1)
    return(row)
  }

  expectedN1 <- .evSeqExpectedSampleSize(design, settings[["n1Seq"]])
  expectedN2 <- .evSeqExpectedSampleSize(design, settings[["n2Seq"]])
  expectedN1Null <- .evSeqExpectedSampleSize(null, settings[["n1Seq"]])
  expectedN2Null <- .evSeqExpectedSampleSize(null, settings[["n2Seq"]])

  if (settings[["isIndependentSamples"]]) {
    row[["maximumN1"]] <- max(settings[["n1Seq"]])
    row[["maximumN2"]] <- max(settings[["n2Seq"]])
    row[["expectedN1"]] <- expectedN1[["mean"]]
    row[["expectedN2"]] <- expectedN2[["mean"]]
    row[["sdN1"]] <- expectedN1[["sd"]]
    row[["sdN2"]] <- expectedN2[["sd"]]
    row[["expectedN1Null"]] <- expectedN1Null[["mean"]]
    row[["expectedN2Null"]] <- expectedN2Null[["mean"]]
    row[["sdN1Null"]] <- expectedN1Null[["sd"]]
    row[["sdN2Null"]] <- expectedN2Null[["sd"]]
  } else {
    row[["maximumN"]] <- max(settings[["n1Seq"]])
    row[["expectedN"]] <- expectedN1[["mean"]]
    row[["sdN"]] <- expectedN1[["sd"]]
    row[["expectedNNull"]] <- expectedN1Null[["mean"]]
    row[["sdNNull"]] <- expectedN1Null[["sd"]]
  }

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

  table <- createJaspTable(title = gettext("Design Outcome"))
  table$dependOn(.evSeqDependencies)
  table$position <- 2
  jaspResults[["sequentialEvidenceDesignOutcome"]] <- table

  table$addColumnInfo(name = "under",       title = gettext("Under"),       type = "string")
  table$addColumnInfo(name = "null",        title = gettext("Null"),        type = "number", overtitle = gettext("Evidence"))
  table$addColumnInfo(name = "undecided",   title = gettext("Undecided"),   type = "number", overtitle = gettext("Evidence"))
  table$addColumnInfo(name = "alternative", title = gettext("Alternative"), type = "number", overtitle = gettext("Evidence"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute design outcome: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqDesignOutcomeRows(result))
  table$addFootnote(gettext("Probabilities are cumulative through the final look. The H\u2081 row uses the design prior; the H\u2080 row uses the null value."))
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

.evSeqStagewiseTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceStagewise"]]))
    return()

  table <- createJaspTable(title = gettext("Stagewise Evidence"))
  table$dependOn(.evSeqDependencies)
  table$position <- 3
  jaspResults[["sequentialEvidenceStagewise"]] <- table

  table$addColumnInfo(name = "under", title = gettext("Under"), type = "string")
  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  if (.evSeqUsesStandardErrorOnly(settings)) {
    table$addColumnInfo(name = "standardError", title = gettext("SE"), type = "number")
  } else if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer")
    table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer")
  } else {
    table$addColumnInfo(name = "n", title = "N", type = "integer")
  }

  table$addColumnInfo(name = "alternativeStop", title = gettext("Alternative"), type = "number", overtitle = gettext("Incremental Stop"))
  table$addColumnInfo(name = "nullStop",        title = gettext("Null"),        type = "number", overtitle = gettext("Incremental Stop"))
  table$addColumnInfo(name = "anyStop",         title = gettext("Any"),         type = "number", overtitle = gettext("Incremental Stop"))

  table$addColumnInfo(name = "alternative", title = gettext("Alternative"), type = "number", overtitle = gettext("Cumulative Evidence"))
  table$addColumnInfo(name = "undecided",   title = gettext("Undecided"),   type = "number", overtitle = gettext("Cumulative Evidence"))
  table$addColumnInfo(name = "null",        title = gettext("Null"),        type = "number", overtitle = gettext("Cumulative Evidence"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stagewise evidence: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqStagewiseRows(settings, result))
}

.evSeqStagewiseRows <- function(settings, result) {
  rows <- rbind(
    .evSeqStagewiseRowsForDesign(settings, result[["design"]], gettext("H\u2081")),
    .evSeqStagewiseRowsForDesign(settings, result[["null"]], gettext("H\u2080"))
  )

  return(rows)
}

.evSeqStagewiseRowsForDesign <- function(settings, design, under) {
  alternativeStop <- diff(c(0, design[["cumpH1"]]))
  nullStop        <- diff(c(0, design[["cumpH0"]]))

  rows <- data.frame(
    under           = under,
    look            = seq_along(settings[["n1Seq"]]),
    alternativeStop = alternativeStop,
    nullStop        = nullStop,
    anyStop         = alternativeStop + nullStop,
    alternative     = design[["cumpH1"]],
    undecided       = design[["cumpInc"]],
    null            = design[["cumpH0"]],
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
  table$position <- 4
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

  table$addColumnInfo(name = "h1Lower", title = gettext("Lower"), type = "number", overtitle = gettext("Evidence for H\u2081"))
  table$addColumnInfo(name = "h1Upper", title = gettext("Upper"), type = "number", overtitle = gettext("Evidence for H\u2081"))
  table$addColumnInfo(name = "h0Lower", title = gettext("Lower"), type = "number", overtitle = gettext("Evidence for H\u2080"))
  table$addColumnInfo(name = "h0Upper", title = gettext("Upper"), type = "number", overtitle = gettext("Evidence for H\u2080"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stopping boundaries: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evSeqBoundaryRows(settings, result[["design"]]))
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

  table <- createJaspTable(title = gettext("Priors"))
  table$dependOn(.evSeqDependencies)
  table$position <- 5
  jaspResults[["sequentialEvidencePriors"]] <- table

  table$addColumnInfo(name = "section", title = gettext("Section"), type = "string")
  table$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
  table$addColumnInfo(name = "parameters", title = gettext("Parameters"), type = "string")

  table$setData(data.frame(
    section      = c(gettext("Prior Under H\u2080"), gettext("Prior Under H\u2081"), gettext("Design Prior")),
    distribution = c(.evNullPriorLabel(settings), .evAnalysisPriorLabel(settings), .evDesignPriorLabel(settings)),
    parameters   = c(.evNullPriorParameters(settings), .evAnalysisPriorParameters(settings), .evDesignPriorParameters(settings)),
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
  html$position <- 6
  jaspResults[["sequentialEvidenceText"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested sequential evidence calculation could not be completed with the current settings.")
    return()
  }

  outcome <- .evSeqFinalOutcome(result[["design"]])
  firstSentence <- if (.evSeqUsesSampleSizeSearch(settings)) {
    gettextf(
      "This group-sequential design finds the smallest maximum sample size for %1$s at %2$s planned looks.",
      .evTargetLabel(settings[["evidenceTarget"]]),
      length(settings[["n1Seq"]])
    )
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
      "At the final look, the probability of evidence for H\u2081 is %1$s, the probability of evidence for H\u2080 is %2$s, and the probability of remaining undecided is %3$s under the design prior.",
      .evFormatNumber(outcome[["alternative"]]),
      .evFormatNumber(outcome[["null"]]),
      .evFormatNumber(outcome[["undecided"]])
    ),
    "</p>"
  )
}

.evSeqRCode <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceRCode"]]))
    return()

  html <- createJaspHtml(title = gettext("R Code"))
  html$dependOn(c(.evSeqDependencies, "generateRCode"))
  html$position <- 10
  jaspResults[["sequentialEvidenceRCode"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("R code could not be generated because the sequential design could not be computed.")
    return()
  }

  code <- try(.evSeqBfpwrCall(settings), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .evCleanError(code))
    return()
  }

  html[["text"]] <- .evCodeHtml(code)
}

.evSeqBfpwrCall <- function(settings) {
  if (settings[["isZTest"]])
    return(.evSeqZBfpwrCall(settings))

  return(.evSeqTBfpwrCall(settings))
}

.evSeqZBfpwrCall <- function(settings) {
  analysisPrior <- .evSeqZAnalysisPrior(settings)
  dpm           <- .evSeqDirectionalMean(settings, settings[["designPriorMeanRelative"]])

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
      dpsd   = settings[["designPriorSd"]],
      type   = analysisPrior[["type"]],
      strict = settings[["strictIntegration"]]
    ),
    .evSeqIntegrationArguments(settings)
  )

  return(.evFormatRCall("bfpwr::pbf01seq", args))
}

.evSeqTBfpwrCall <- function(settings) {
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
      dpm         = settings[["designPriorMeanRelative"]],
      dpsd        = settings[["designPriorSd"]],
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
  if (!is.null(jaspResults[["sequentialEvidenceStoppingProbabilitiesPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Stopping Probabilities"), width = 735, height = 350)
  plot$dependOn(c(.evSeqDependencies, "stoppingProbabilitiesPlot", "showNullReference"))
  plot$position <- 7
  jaspResults[["sequentialEvidenceStoppingProbabilitiesPlot"]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute stopping probabilities: %1$s", .evCleanError(result)))
    return()
  }

  plotResult <- try(.evSeqBuildStoppingProbabilitiesPlot(settings, result), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute stopping probabilities: %1$s", .evCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.evSeqBuildStoppingProbabilitiesPlot <- function(settings, result) {
  data <- .evSeqStoppingProbabilityPlotData(settings, result)
  xLabel <- .evSeqLookAxisLabel(settings)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome, linetype = under)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(x = xLabel, y = gettext("Cumulative probability"), color = gettext("Evidence"), linetype = gettext("Under"))

  if (.evSeqUsesSampleSizeSearch(settings))
    plot <- plot + ggplot2::geom_hline(yintercept = settings[["targetProbability"]], linetype = "dotted", color = "#555555")

  return(.pwrApplyPlotTheme(plot))
}

.evSeqStoppingProbabilityPlotData <- function(settings, result) {
  rows <- list(.evSeqStoppingProbabilityRows(settings, result[["design"]], gettext("H\u2081")))

  if (isTRUE(settings[["showNullReference"]]))
    rows[[length(rows) + 1]] <- .evSeqStoppingProbabilityRows(settings, result[["null"]], gettext("H\u2080"))

  return(do.call(rbind, rows))
}

.evSeqStoppingProbabilityRows <- function(settings, design, under) {
  data.frame(
    n           = rep(.evSeqLookAxisValues(settings), 3),
    probability = c(design[["cumpH1"]], design[["cumpInc"]], design[["cumpH0"]]),
    outcome     = rep(c(gettext("Alternative"), gettext("Undecided"), gettext("Null")), each = length(settings[["n1Seq"]])),
    under       = under,
    stringsAsFactors = FALSE
  )
}

.evSeqStoppingBoundariesPlot <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["sequentialEvidenceStoppingBoundariesPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Stopping Boundaries"), width = 735, height = 350)
  plot$dependOn(c(.evSeqDependencies, "stoppingBoundariesPlot"))
  plot$position <- 8
  jaspResults[["sequentialEvidenceStoppingBoundariesPlot"]] <- plot

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
  if (!is.null(jaspResults[["sequentialEvidencePriorPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Prior Distribution"), width = 735, height = 350)
  plot$dependOn(c(.evSeqDependencies, "priorDistribution", "plotPoints"))
  plot$position <- 9
  jaspResults[["sequentialEvidencePriorPlot"]] <- plot

  plotResult <- try(.evBuildPriorPlot(settings), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute prior distribution plot: %1$s", .evCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}
