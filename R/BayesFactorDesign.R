BayesFactorDesign <- function(jaspResults, dataset, options) {
  settings    <- .bfdPrepareSettings(options)
  tables      <- .bfdInitializeOutputTables(jaspResults, options, settings)
  if (.bfdPriorPlotRequested(options))
    .bfdPriorPlot(jaspResults, settings)

  computation <- .bfdCachedComputation(jaspResults, settings)
  settings    <- computation[["settings"]]
  result      <- computation[["result"]]

  .bfdPopulateInitializedOutputTables(tables, settings, result)

  if (.bfdObservedAnalysisReady(dataset, options, settings))
    .bfdObservedAnalysisTable(jaspResults, dataset, options, settings, key = "evidenceObservedAnalysis", position = 16)

  if (options[["generateReport"]])
    .bfdReport(jaspResults, settings, result)

  if (options[["generateRCode"]])
    .bfdRCode(jaspResults, settings)

  if (options[["decisionProbabilitiesByEffectSize"]])
    .bfdEffectSizePlot(jaspResults, settings, result)

  if (options[["decisionProbabilitiesBySampleSize"]])
    .bfdSampleSizePlot(jaspResults, settings, result)

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
.bfdSummaryDesignDependencies        <- c(.bfdDesignDependencies, "designSummary", "explanatoryText")
.bfdSummaryEvidenceDependencies      <- c(.bfdDesignDependencies, "decisionProbabilities", "explanatoryText")
.bfdSummarySpecificationDependencies <- c(.bfdDesignDependencies, "designSpecification", "explanatoryText")

.bfdSampleSizePlotDependencies <- c(
  .bfdDesignDependencies, "decisionProbabilitiesBySampleSize", "combineH1H0Figures",
  "curvePoints", "logSampleSizeAxis", "legendPosition", "colorPalette", "explanatoryText"
)
.bfdEffectSizePlotDependencies <- c(
  .bfdDesignDependencies, "decisionProbabilitiesByEffectSize", "combineH1H0Figures",
  "curvePoints", "legendPosition", "colorPalette", "explanatoryText"
)
.bfdPriorPlotDependencies <- c(
  .bfdDesignDependencies, "designPriorDistributionFigure", "analysisPriorDistributionFigure", "combineDesignAnalysisPriorFigures",
  "curvePoints", "legendPosition", "colorPalette", "explanatoryText"
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

  result <- try(.bfdComputeResult(settings), silent = TRUE)

  state$object <- list(
    settings = settings,
    result   = result
  )

  return(.bfdApplyCurrentSettings(state$object, settings, .bfdDisplaySettingNames))
}

.bfdDisplaySettingNames <- c(
  "curvePoints", "logSampleSizeAxis", "legendPosition", "colorPalette",
  "combineH1H0Figures", "designPriorDistributionFigure", "analysisPriorDistributionFigure",
  "combineDesignAnalysisPriorFigures", "generateReportLatexFormattedOutput", "explanatoryText"
)

.bfdInitializeOutputTables <- function(jaspResults, options, settings) {
  tables <- list(
    results = if (options[["designSummary"]] && is.null(jaspResults[["evidenceResults"]])) {
      table <- createJaspTable(title = gettext("Bayes Factor Design"))
      table$dependOn(.bfdSummaryDesignDependencies)
      table$position <- 1
      jaspResults[["evidenceResults"]] <- table
      table
    },
    designOutcome = if (options[["decisionProbabilities"]] && is.null(jaspResults[["evidenceDesignOutcome"]])) {
      table <- createJaspTable(title = gettext("Bayes Factor Decision Probabilities"))
      table$dependOn(.bfdSummaryEvidenceDependencies)
      table$position <- 2
      jaspResults[["evidenceDesignOutcome"]] <- table
      table
    },
    priors = if (options[["designSpecification"]] && is.null(jaspResults[["evidencePriors"]])) {
      table <- createJaspTable(title = gettext("Design Specification"))
      table$dependOn(.bfdSummarySpecificationDependencies)
      table$position <- 3
      jaspResults[["evidencePriors"]] <- table
      table
    }
  )

  if (!is.null(tables[["results"]]))
    .bfdInitializeResultsTable(tables[["results"]], settings)

  if (!is.null(tables[["designOutcome"]]))
    .bfdInitializeDesignOutcomeTable(tables[["designOutcome"]])

  if (!is.null(tables[["priors"]]))
    .bfdPopulatePriorsTable(tables[["priors"]], settings)

  return(tables)
}

.bfdPopulateInitializedOutputTables <- function(tables, settings, result) {
  if (!is.null(tables[["results"]]))
    .bfdPopulateResultsTable(tables[["results"]], settings, result, columnsReady = TRUE)

  if (!is.null(tables[["designOutcome"]]))
    .bfdPopulateDesignOutcomeTable(tables[["designOutcome"]], settings, result, columnsReady = TRUE)

  return(invisible(TRUE))
}

.bfdPrepareSettings <- function(options) {
  settings <- as.list(options)
  derivedSettings <- list(
    testType             = .bfdTestType(settings[["statisticalTest"]]),
    isIndependentSamples = grepl("independentSamples", settings[["statisticalTest"]], fixed = TRUE),
    isTTest              = grepl("TTest", settings[["statisticalTest"]], fixed = TRUE),
    isGeneralZ           = identical(settings[["statisticalTest"]], "generalZApproximation"),
    isZTest              = grepl("ZTest", settings[["statisticalTest"]], fixed = TRUE) ||
      identical(settings[["statisticalTest"]], "generalZApproximation"),
    isBinomial           = identical(settings[["statisticalTest"]], "oneSampleProportion")
  )
  settings[names(derivedSettings)] <- derivedSettings

  if (!settings[["isBinomial"]])
    settings[["alternative"]] <- .bfdAnalysisPriorAlternative(settings[["analysisPriorDirection"]])

  settings[["n1"]] <- settings[["sampleSize"]]
  settings[["n2"]] <- if (settings[["isBinomial"]]) {
    NA_integer_
  } else if (settings[["isIndependentSamples"]]) {
    ceiling(settings[["sampleSize"]] * settings[["sampleSizeAllocationRatio"]])
  } else {
    settings[["sampleSize"]]
  }

  return(settings)
}

.bfdComputeResult <- function(settings) {
  targets       <- c("h1", "h0")
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
    n1 <- if (settings[["calculationTarget"]] == "sampleSize") {
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
    designPrior     <- .bfdContinuousDesignPrior(settings, under)
    designPriorMean <- designPrior[["mean"]]
    designPriorSd   <- designPrior[["sd"]]
  } else if (is.null(designPriorSd)) {
    designPriorSd <- .bfdContinuousDesignPrior(settings, under)[["sd"]]
  }

  if (settings[["isZTest"]])
    return(.bfdEvidenceProbabilityZ(settings, n1, k, lowerTail, designPriorMean, designPriorSd))

  return(.bfdEvidenceProbabilityT(settings, n1, k, lowerTail, designPriorMean, designPriorSd))
}

.bfdEvidenceProbabilityZ <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  unitSd <- .bfdZUnitStandardDeviation(settings, n1)

  if (settings[["analysisPriorDistribution"]] %in% c("point", "normal")) {
    return(bfpwr::pbf01(
      k          = k,
      n          = n1,
      usd        = unitSd,
      null       = settings[["nullValue"]],
      pm         = .bfdZAnalysisPriorMean(settings),
      psd        = .bfdZAnalysisPriorSd(settings),
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
    psd        = .bfdMomentPriorSpread(settings),
    dpm        = designPriorMean,
    dpsd       = designPriorSd,
    lower.tail = lowerTail
  ))
}

.bfdEvidenceProbabilityT <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  args <- c(
    list(
      k           = k,
      null        = settings[["nullValue"]],
      plocation   = .bfdTPriorLocationRelative(settings),
      pscale      = settings[["tPriorScale"]],
      pdf         = .bfdTPriorDf(settings),
      dpm         = designPriorMean,
      dpsd        = designPriorSd,
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      lower.tail  = lowerTail,
      drange      = .bfdTSearchRangeArgument(settings)
    ),
    .bfdTTestSampleSizeArguments(settings, n1)
  )

  return(do.call(bfpwr::ptbf01, args))
}

.bfdTTestSampleSizeArguments <- function(settings, n1, n2 = .bfdSampleSizeSecondGroup(settings, n1)) {
  if (settings[["isIndependentSamples"]])
    return(list(n1 = n1, n2 = n2))

  return(list(n = n1))
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
  maximumN <- ceiling(settings[["maximumSampleSize"]])

  n <- try(.bfdFindSampleSizeWithBfpwr(settings, minimumN, maximumN, target = target, under = under), silent = TRUE)
  if (jaspBase::isTryError(n))
    stop(.bfdCleanError(n))

  if (length(n) != 1 || !is.finite(n))
    stop(gettext("Target conclusive evidence probability is not reached within the selected sample-size range."))

  return(as.integer(ceiling(n)))
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

  designPrior <- .bfdContinuousDesignPrior(settings, under)

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("point", "normal")) {
    return(bfpwr::nbf01(
      k          = k,
      power      = .bfdTargetPower(settings, target),
      usd        = .bfdZUnitStandardDeviation(settings, minimumN),
      null       = settings[["nullValue"]],
      pm         = .bfdZAnalysisPriorMean(settings),
      psd        = .bfdZAnalysisPriorSd(settings),
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
      psd        = .bfdMomentPriorSpread(settings),
      dpm        = designPrior[["mean"]],
      dpsd       = designPrior[["sd"]],
      nrange     = nrange,
      lower.tail = lowerTail
    ))
  }

  return(bfpwr::ntbf01(
    k           = k,
    power       = .bfdTargetPower(settings, target),
    null        = settings[["nullValue"]],
    plocation   = .bfdTPriorLocationRelative(settings),
    pscale      = settings[["tPriorScale"]],
    pdf         = .bfdTPriorDf(settings),
    type        = settings[["testType"]],
    alternative = settings[["alternative"]],
    dpm         = designPrior[["mean"]],
    dpsd        = designPrior[["sd"]],
    lower.tail  = lowerTail,
    nrange      = nrange,
    ratio       = if (settings[["isIndependentSamples"]]) settings[["sampleSizeAllocationRatio"]] else 1,
    drange      = .bfdTSearchRangeArgument(settings)
  ))
}

.bfdInitializeResultsTable <- function(table, settings) {
  .bfdAddResultsTableColumns(table, settings)
  table$setData(.bfdEmptyResultsRow(settings))

  return(invisible(TRUE))
}

.bfdPopulateResultsTable <- function(table, settings, result, columnsReady = FALSE) {
  if (!isTRUE(columnsReady))
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

  if (settings[["calculationTarget"]] == "sampleSize")
    table$addFootnote(gettext("Due to rounding of the sample size, Pr(Conclusive Evidence) can deviate from the target probability."))

  combinedSampleSizeFootnote <- .bfdCombinedSampleSizeFootnote(settings, result)
  if (!is.null(combinedSampleSizeFootnote))
    table$addFootnote(combinedSampleSizeFootnote)

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested N\u2082/N\u2081 sample-size ratio."))

  .bfdAddExplanationFootnotes(table, settings, .bfdResultsTableExplanation(settings))
}

.bfdAddResultsTableColumns <- function(table, settings) {
  computed    <- gettext("Computed")
  userDefined <- gettext("User Defined")

  table$addColumnInfo(name = "under", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "decisionRule", title = gettext("Decision Rule"), type = "string", overtitle = userDefined)

  if (settings[["calculationTarget"]] == "sampleSize") {
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

.bfdEmptyResultsRow <- function(settings) {
  columns <- c(
    "under",
    "decisionRule",
    if (settings[["calculationTarget"]] == "sampleSize") "targetProbability",
    if (settings[["isIndependentSamples"]]) c("n1", "n2") else "n",
    "probability"
  )

  return(.bfdEmptyTableRow(
    columns        = columns,
    stringColumns  = c("under", "decisionRule"),
    integerColumns = if (settings[["isIndependentSamples"]]) c("n1", "n2") else "n"
  ))
}

.bfdResultsRows <- function(settings, result) {
  rows <- result[["targetResults"]]
  n1   <- if (identical(settings[["calculationTarget"]], "sampleSize")) {
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

  if (settings[["calculationTarget"]] != "sampleSize")
    out[["targetProbability"]] <- NULL

  columnOrder <- c(
    "under",
    "decisionRule",
    if (settings[["calculationTarget"]] == "sampleSize") "targetProbability",
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

.bfdInitializeDesignOutcomeTable <- function(table) {
  .bfdAddDesignOutcomeColumns(
    table,
    underTitle       = gettext("Design Prior"),
    overtitle        = gettext("Bayes Factor Decision"),
    nullTitle        = gettext("Evidence for H\u2080"),
    alternativeTitle = gettext("Evidence for H\u2081")
  )
  table$setData(.bfdEmptyDesignOutcomeRow())

  return(invisible(TRUE))
}

.bfdPopulateDesignOutcomeTable <- function(table, settings, result, columnsReady = FALSE) {
  if (!isTRUE(columnsReady)) {
    .bfdAddDesignOutcomeColumns(
      table,
      underTitle       = gettext("Design Prior"),
      overtitle        = gettext("Bayes Factor Decision"),
      nullTitle        = gettext("Evidence for H\u2080"),
      alternativeTitle = gettext("Evidence for H\u2081")
    )
  }

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
  .bfdAddExplanationFootnotes(table, settings, .bfdDesignOutcomeTableExplanation())
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
  if (!identical(settings[["calculationTarget"]], "sampleSize"))
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
  if (!identical(settings[["calculationTarget"]], "sampleSize"))
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
  if (!identical(settings[["calculationTarget"]], "sampleSize"))
    return(NULL)

  return(.bfdSampleSizeBasisTarget(settings[["designSampleSizeBasis"]], under))
}

.bfdResultN1ForBasis <- function(settings, result, under) {
  if (!identical(settings[["calculationTarget"]], "sampleSize"))
    return(result[["n1"]])

  basisTarget <- .bfdDesignSampleSizeBasisTarget(settings, under)
  if (is.null(basisTarget))
    return(result[["n1"]])

  return(.bfdResultN1ForUnder(result, basisTarget))
}

.bfdDesignOutcomeProbabilities <- function(settings, n1, under) {
  alternative <- .bfdEvidenceProbability(settings, n1 = n1, target = "h1", under = under)
  null        <- .bfdEvidenceProbability(settings, n1 = n1, target = "h0", under = under)
  undecided   <- 1 - alternative - null

  return(c(
    null        = .bfdClampProbability(null),
    undecided   = .bfdClampProbability(undecided),
    alternative = .bfdClampProbability(alternative)
  ))
}

.bfdPopulatePriorsTable <- function(table, settings) {
  .bfdAddPriorsTableColumns(table)

  table$setData(.bfdPriorsRows(settings))
  .bfdAddExplanationFootnotes(table, settings, .bfdPriorsTableExplanation())
}

.bfdResultsTableExplanation <- function(settings) {
  if (settings[["calculationTarget"]] == "sampleSize") {
    return(gettext(
      "This table reports the smallest fixed-design sample size that reaches the target probability of conclusive evidence for each design prior."
    ))
  }

  return(gettext(
    "This table evaluates the selected fixed sample size. Achieved probabilities are the chances that the planned study produces conclusive evidence in the requested direction when data are generated from the corresponding design prior."
  ))
}

.bfdDesignOutcomeTableExplanation <- function() {
  gettext(
    "Rows condition on the design prior used to generate hypothetical data. Diagonal cells are conclusive evidence for the true hypothesis, off-diagonal cells are misleading evidence, and the middle column is the chance of remaining inconclusive."
  )
}

.bfdPriorsTableExplanation <- function() {
  gettext(
    "The analysis prior is used to compute the Bayes factor from the data. The design prior describes the effect sizes assumed during planning; it can differ from the analysis prior."
  )
}

.bfdObservedSummaryExplanation <- function() {
  gettext("Summary statistics are the data summaries used to compute the observed Bayes factor.")
}

.bfdObservedAnalysisExplanation <- function(sequential = FALSE) {
  if (isTRUE(sequential)) {
    return(gettext(
      "The observed-data analysis applies the planned sequential decision rule to the current data. It is an interim or final decision for the observed study, not an operating characteristic of the design."
    ))
  }

  return(gettext(
    "The observed-data analysis applies the planned fixed-design Bayes factor rule to the current data. It is the decision for this observed study, not an operating characteristic of the design."
  ))
}

.bfdObservedAnalysisTable <- function(jaspResults, dataset, options, settings, key, position, sequential = FALSE,
                                     dependencies = .bfdDesignDependencies) {
  if (!is.null(jaspResults[[key]]))
    return()

  showSummaryTable <- options[["observedDataAnalysisInput"]] == "columns"

  container <- createJaspContainer(title = gettext("Observed Data Analysis"))
  container$dependOn(c(dependencies, .bfdObservedDependencies, "explanatoryText"))
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

  if (showSummaryTable)
    .bfdAddExplanationFootnotes(summaryTable, settings, .bfdObservedSummaryExplanation())

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

  .bfdAddExplanationFootnotes(resultTable, settings, .bfdObservedAnalysisExplanation(sequential))
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
    settings[["statisticalTest"]],
    independentSamplesTTest = gettext("Independent Samples T-Test"),
    pairedSamplesTTest      = gettext("Paired Samples T-Test"),
    oneSampleTTest          = gettext("One Sample T-Test"),
    independentSamplesZTest = gettext("Independent Samples Z-Test"),
    pairedSamplesZTest      = gettext("Paired Samples Z-Test"),
    oneSampleZTest          = gettext("One Sample Z-Test"),
    oneSampleProportion     = gettext("One Sample Proportion Test"),
    generalZApproximation   = gettext("General (z-approximation)")
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

  return(.bfdObservedOneSampleSummaryRows(summary))
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

.bfdObservedOneSampleSummaryRows <- function(summary) {
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
  if (settings[["isIndependentSamples"]])
    return(.bfdObservedIndependentTSummaryFromOptions(options, settings, source, options[["observedInputType"]]))

  return(.bfdObservedDependentTSummaryFromOptions(options, settings, source, options[["observedInputType"]]))
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
  standardError <- settings[["knownStandardDeviation"]] * sqrt(1 / n1 + 1 / n2)
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
  standardError <- settings[["knownStandardDeviation"]] / sqrt(n)
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
  standardError <- settings[["knownStandardDeviation"]] / sqrt(n)
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
    pm   <- .bfdZAnalysisPriorMean(settings)
    if (identical(settings[["alternative"]], "less")) {
      estimate <- 2 * null - estimate
      pm       <- 2 * null - pm
    }

    return(bfpwr::dirbf01(
      estimate = estimate,
      se       = se,
      null     = null,
      pm       = pm,
      psd      = .bfdZAnalysisPriorSd(settings)
    ))
  }

  if (settings[["analysisPriorDistribution"]] %in% c("point", "normal")) {
    return(bfpwr::bf01(
      estimate = estimate,
      se       = se,
      null     = settings[["nullValue"]],
      pm       = .bfdZAnalysisPriorMean(settings),
      psd      = .bfdZAnalysisPriorSd(settings)
    ))
  }

  return(bfpwr::nmbf01(
    estimate = estimate,
    se       = se,
    null     = settings[["nullValue"]],
    psd      = .bfdMomentPriorSpread(settings)
  ))
}

.bfdObservedTBayesFactor <- function(settings, summary) {
  n1 <- if (settings[["isIndependentSamples"]]) summary[["n1"]] else summary[["n"]]
  n2 <- if (settings[["isIndependentSamples"]]) summary[["n2"]] else summary[["n"]]

  args <- c(
    list(
      t           = summary[["testStatistic"]],
      plocation   = .bfdTPriorLocationRelative(settings),
      pscale      = settings[["tPriorScale"]],
      pdf         = .bfdTPriorDf(settings),
      type        = settings[["testType"]],
      alternative = settings[["alternative"]]
    ),
    .bfdTTestSampleSizeArguments(settings, n1, n2)
  )

  return(do.call(bfpwr::tbf01, args))
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
  if (bayesFactor[["bf10"]] >= settings[["conclusiveEvidenceThresholdH1"]])
    return(if (isTRUE(sequential)) gettext("Stop for H\u2081") else gettext("BF supports H\u2081"))

  if (bayesFactor[["bf01"]] >= settings[["conclusiveEvidenceThresholdH0"]])
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
  if (!is.null(jaspResults[["evidenceReport"]]))
    return()

  html <- createJaspHtml(title = gettext("Report"))
  html$dependOn(.bfdReportDependencies)
  html$position <- 0
  jaspResults[["evidenceReport"]] <- html

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
    .bfdTestLabel(settings[["statisticalTest"]]),
    .bfdReportHypothesisClause(settings)
  )
}

.bfdReportDesignAction <- function(settings) {
  if (identical(settings[["calculationTarget"]], "sampleSize"))
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
    prior <- if (.bfdAnalysisPriorIsCauchy(settings)) {
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
        .bfdReportNumber(.bfdTPriorDf(settings))
      )
    }
  } else if (settings[["analysisPriorDistribution"]] == "point") {
    return(.bfdReportPointMassPhrase(.bfdReportParameterSymbol(settings), .bfdZAnalysisPriorMean(settings)))
  } else if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    prior <- .bfdReportContinuousZPriorDistribution(settings)
  } else {
    prior <- gettextf(
      "%1$s ~ Normal-moment(spread = %2$s, modes = \u00B1%3$s)",
      .bfdReportParameterSymbol(settings),
      .bfdReportNumber(.bfdMomentPriorSpread(settings)),
      .bfdReportNumber(.bfdMomentPriorMode(settings))
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
    .bfdReportNumber(.bfdZAnalysisPriorMean(settings)),
    .bfdReportNumber(.bfdZAnalysisPriorSd(settings))
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

  prior <- .bfdContinuousDesignPrior(settings, under)
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
  prior <- .bfdBinomialDesignPrior(settings, under)
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
    .bfdReportNumber(settings[["conclusiveEvidenceThresholdH1"]]),
    .bfdReportNumber(settings[["conclusiveEvidenceThresholdH0"]]),
    .bfdReportReciprocalText(settings[["conclusiveEvidenceThresholdH0"]])
  )
}

.bfdReportFixedPlanningSentence <- function(settings, result) {
  h1N <- .bfdResultN1ForUnder(result, "h1")
  h0N <- .bfdResultN1ForUnder(result, "h0")

  if (!identical(settings[["calculationTarget"]], "sampleSize"))
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

.bfdReportProbabilitySentence <- function(h1Outcome, h0Outcome) {
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
  if (!is.null(settings) && isTRUE(settings[["generateReportLatexFormattedOutput"]]))
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

.bfdRCode <- function(jaspResults, settings) {
  if (!is.null(jaspResults[["evidenceRCode"]]))
    return()

  html <- createJaspHtml(title = gettext("R Code"))
  html$dependOn(.bfdRCodeDependencies)
  html$position <- 14
  jaspResults[["evidenceRCode"]] <- html

  code <- try(.bfdBfpwrCall(settings), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .bfdCleanError(code))
    return()
  }

  html[["text"]] <- .bfdCodeHtml(code)
}

.bfdBfpwrCall <- function(settings) {
  calls <- vapply(c("h1", "h0"), function(target) {
    prefix <- if (target == "h1") "# Plan for evidence for H1" else "# Plan for evidence for H0"

    call <- if (settings[["isBinomial"]]) {
      .bfdBinomialBfpwrCall(settings, target)
    } else if (settings[["isZTest"]]) {
      .bfdZBfpwrCall(settings, target)
    } else {
      .bfdTBfpwrCall(settings, target)
    }

    paste(prefix, call, sep = "\n")
  }, character(1))

  return(paste(calls, collapse = "\n\n"))
}

.bfdBinomialBfpwrCall <- function(settings, target) {
  if (settings[["calculationTarget"]] == "sampleSize") {
    args <- c(
      list(
        k          = .bfdEventK(settings, target),
        power      = .bfdTargetPower(settings, target),
        p0         = settings[["nullProportion"]],
        type       = settings[["nullPriorDistribution"]],
        a          = settings[["analysisPriorSuccesses"]],
        b          = settings[["analysisPriorFailures"]],
        lower.tail = .bfdLowerTail(target),
        nrange     = c(.bfdMinimumSampleSize(settings), ceiling(settings[["maximumSampleSize"]]))
      ),
      .bfdBinomialDesignArguments(settings, under = target)
    )

    return(.bfdFormatRCall("bfpwr::nbinbf01", args))
  }

  args <- c(
    list(
      k          = .bfdEventK(settings, target),
      n          = settings[["sampleSize"]],
      p0         = settings[["nullProportion"]],
      type       = settings[["nullPriorDistribution"]],
      a          = settings[["analysisPriorSuccesses"]],
      b          = settings[["analysisPriorFailures"]],
      lower.tail = .bfdLowerTail(target)
    ),
    .bfdBinomialDesignArguments(settings, under = target)
  )

  return(.bfdFormatRCall("bfpwr::pbinbf01", args))
}

.bfdZBfpwrCall <- function(settings, target) {
  normalPrior <- settings[["analysisPriorDistribution"]] %in% c("point", "normal")
  designPrior <- .bfdContinuousDesignPrior(settings, target)

  if (settings[["calculationTarget"]] == "sampleSize") {
    n <- .bfdMinimumSampleSize(settings)
    if (normalPrior) {
      args <- list(
        k          = .bfdEventK(settings, target),
        power      = .bfdTargetPower(settings, target),
        usd        = .bfdZUnitStandardDeviation(settings, n),
        null       = settings[["nullValue"]],
        pm         = .bfdZAnalysisPriorMean(settings),
        psd        = .bfdZAnalysisPriorSd(settings),
        dpm        = designPrior[["mean"]],
        dpsd       = designPrior[["sd"]],
        nrange     = c(n, ceiling(settings[["maximumSampleSize"]])),
        lower.tail = .bfdLowerTail(target)
      )

      return(.bfdFormatRCall("bfpwr::nbf01", args))
    }

    args <- list(
      k          = .bfdEventK(settings, target),
      power      = .bfdTargetPower(settings, target),
      usd        = .bfdZUnitStandardDeviation(settings, n),
      null       = settings[["nullValue"]],
      psd        = .bfdMomentPriorSpread(settings),
      dpm        = designPrior[["mean"]],
      dpsd       = designPrior[["sd"]],
      nrange     = c(n, ceiling(settings[["maximumSampleSize"]])),
      lower.tail = .bfdLowerTail(target)
    )

    return(.bfdFormatRCall("bfpwr::nnmbf01", args))
  }

  n <- settings[["sampleSize"]]

  if (normalPrior) {
    args <- list(
      k          = .bfdEventK(settings, target),
      n          = n,
      usd        = .bfdZUnitStandardDeviation(settings, n),
      null       = settings[["nullValue"]],
      pm         = .bfdZAnalysisPriorMean(settings),
      psd        = .bfdZAnalysisPriorSd(settings),
      dpm        = designPrior[["mean"]],
      dpsd       = designPrior[["sd"]],
      lower.tail = .bfdLowerTail(target)
    )

    return(.bfdFormatRCall("bfpwr::pbf01", args))
  }

  args <- list(
    k          = .bfdEventK(settings, target),
    n          = n,
    usd        = .bfdZUnitStandardDeviation(settings, n),
    null       = settings[["nullValue"]],
    psd        = .bfdMomentPriorSpread(settings),
    dpm        = designPrior[["mean"]],
    dpsd       = designPrior[["sd"]],
    lower.tail = .bfdLowerTail(target)
  )

  return(.bfdFormatRCall("bfpwr::pnmbf01", args))
}

.bfdTBfpwrCall <- function(settings, target) {
  designPrior <- .bfdContinuousDesignPrior(settings, target)

  if (settings[["calculationTarget"]] == "sampleSize") {
    args <- list(
      k           = .bfdEventK(settings, target),
      power       = .bfdTargetPower(settings, target),
      null        = settings[["nullValue"]],
      plocation   = .bfdTPriorLocationRelative(settings),
      pscale      = settings[["tPriorScale"]],
      pdf         = .bfdTPriorDf(settings),
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      dpm         = designPrior[["mean"]],
      dpsd        = designPrior[["sd"]],
      lower.tail  = .bfdLowerTail(target),
      nrange      = c(.bfdMinimumSampleSize(settings), ceiling(settings[["maximumSampleSize"]])),
      ratio       = if (settings[["isIndependentSamples"]]) settings[["sampleSizeAllocationRatio"]] else 1,
      drange      = .bfdTSearchRangeArgument(settings)
    )

    return(.bfdFormatRCall("bfpwr::ntbf01", args))
  }

  n1 <- settings[["sampleSize"]]
  n2 <- .bfdSampleSizeSecondGroup(settings, n1)
  args <- c(
    list(
      k = .bfdEventK(settings, target)
    ),
    .bfdTTestSampleSizeArguments(settings, n1, n2),
    list(
      null        = settings[["nullValue"]],
      plocation   = .bfdTPriorLocationRelative(settings),
      pscale      = settings[["tPriorScale"]],
      pdf         = .bfdTPriorDf(settings),
      dpm         = designPrior[["mean"]],
      dpsd        = designPrior[["sd"]],
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      lower.tail  = .bfdLowerTail(target),
      drange      = .bfdTSearchRangeArgument(settings)
    )
  )

  return(.bfdFormatRCall("bfpwr::ptbf01", args))
}

.bfdSampleSizePlot <- function(jaspResults, settings, result) {
  specs <- .bfdUnderPlotSpecs(settings, "evidenceBySampleSize", gettext("Decision Probabilities by Sample Size"), 9)
  if (!any(vapply(specs, function(spec) is.null(jaspResults[[spec[["key"]]]]), logical(1))))
    return()

  plotData <- if (jaspBase::isTryError(result)) NULL else try(.bfdSampleSizePlotData(settings, result), silent = TRUE)

  for (spec in specs) {
    .bfdSampleSizeOutcomePlot(
      jaspResults = jaspResults,
      settings    = settings,
      result      = result,
      key         = spec[["key"]],
      title       = spec[["title"]],
      position    = spec[["position"]],
      under       = spec[["under"]],
      plotData    = plotData
    )
  }
}

.bfdSampleSizeOutcomePlot <- function(jaspResults, settings, result, key, title, position, under, plotData) {
  if (!is.null(jaspResults[[key]]))
    return()

  plot <- createJaspPlot(title = title, width = .bfdPlotWidth(settings), height = 350)
  plot$dependOn(.bfdSampleSizePlotDependencies)
  plot$position <- position
  jaspResults[[key]] <- plot

  if (jaspBase::isTryError(result)) {
    .bfdSetOutcomePlotError(plot, result)
    return()
  }

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
  .bfdAddExplanationHtml(
    parent       = jaspResults,
    key          = paste0(key, "Note"),
    settings     = settings,
    position     = position + 0.1,
    dependencies = .bfdSampleSizePlotDependencies,
    text         = .bfdSampleSizePlotExplanation()
  )
}

.bfdUnderPlotSpecs <- function(settings, keyPrefix, title, position) {
  if (isTRUE(settings[["combineH1H0Figures"]])) {
    return(list(list(
      key      = paste0(keyPrefix, "Plot"),
      title    = title,
      position = position,
      under    = NULL
    )))
  }

  unders <- c("h1", "h0")
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

  if (settings[["calculationTarget"]] == "sampleSize")
    maximumN <- min(settings[["maximumSampleSize"]], maximumN)

  nValues <- if (isTRUE(settings[["logSampleSizeAxis"]])) {
    unique(ceiling(exp(seq(log(minimumN), log(maximumN), length.out = settings[["curvePoints"]]))))
  } else {
    unique(ceiling(seq(minimumN, maximumN, length.out = settings[["curvePoints"]])))
  }

  return(list(
    data   = .bfdCurveBySampleSize(settings, nValues, c("h1", "h0")),
    xLabel = if (settings[["isIndependentSamples"]]) gettext("Sample size (group 1)") else gettext("Sample size")
  ))
}

.bfdBuildSampleSizePlot <- function(settings, result, under, plotData = NULL) {
  if (is.null(plotData))
    plotData <- .bfdSampleSizePlotData(settings, result)

  unders <- if (is.null(under)) c("h1", "h0") else under
  data   <- .bfdFilterCurveData(plotData[["data"]], under)
  xLabel <- plotData[["xLabel"]]

  xScale <- .pwrPrettyIntegerXAxisScale(data[["n"]], log10 = settings[["logSampleSizeAxis"]])

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

  if (settings[["calculationTarget"]] == "sampleSize") {
    plot <- plot +
      .bfdTargetPowerLine(settings, unders)
  }

  linetypeValues <- if (showUnder) .bfdUnderLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdEffectSizePlot <- function(jaspResults, settings, result) {
  specs <- .bfdUnderPlotSpecs(settings, "evidenceByEffectSize", .bfdEffectSizePlotTitle(settings), 5)
  if (!any(vapply(specs, function(spec) is.null(jaspResults[[spec[["key"]]]]), logical(1))))
    return()

  plotData <- if (jaspBase::isTryError(result)) NULL else try(.bfdEffectSizePlotData(settings, result), silent = TRUE)

  for (spec in specs) {
    .bfdEffectSizeOutcomePlot(
      jaspResults = jaspResults,
      settings    = settings,
      result      = result,
      key         = spec[["key"]],
      title       = spec[["title"]],
      position    = spec[["position"]],
      under       = spec[["under"]],
      plotData    = plotData
    )
  }
}

.bfdEffectSizeOutcomePlot <- function(jaspResults, settings, result, key, title, position, under, plotData) {
  if (!is.null(jaspResults[[key]]))
    return()

  plot <- createJaspPlot(title = title, width = .bfdPlotWidth(settings), height = 350)
  plot$dependOn(.bfdEffectSizePlotDependencies)
  plot$position <- position
  jaspResults[[key]] <- plot

  if (jaspBase::isTryError(result)) {
    .bfdSetOutcomePlotError(plot, result)
    return()
  }

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
  .bfdAddExplanationHtml(
    parent       = jaspResults,
    key          = paste0(key, "Note"),
    settings     = settings,
    position     = position + 0.1,
    dependencies = .bfdEffectSizePlotDependencies,
    text         = .bfdEffectSizePlotExplanation(settings)
  )
}

.bfdEffectSizePlotTitle <- function(settings) {
  if (settings[["isBinomial"]])
    return(gettext("Decision Probabilities by Proportion"))

  return(gettext("Decision Probabilities by Effect Size"))
}

.bfdSampleSizePlotExplanation <- function() {
  gettext(
    "This plot shows how the probabilities of conclusive and misleading evidence change with sample size while priors and thresholds are held fixed. Use it to see how quickly the planned design becomes informative."
  )
}

.bfdEffectSizePlotExplanation <- function(settings) {
  if (settings[["isBinomial"]]) {
    return(gettext(
      "This plot shows how the probabilities of conclusive and misleading evidence change across possible design proportions. Use it as a sensitivity check for the assumed proportion under the design prior."
    ))
  }

  gettext(
    "This plot shows how the probabilities of conclusive and misleading evidence change across possible design-prior effect sizes. Use it as a sensitivity check for the assumed effect size."
  )
}

.bfdEffectSizePlotData <- function(settings, result) {
  if (settings[["isBinomial"]])
    return(.bfdBinomialProportionPlotData(settings, result))

  effectRange <- .bfdEffectRange(settings)
  effect      <- seq(effectRange[1], effectRange[2], length.out = settings[["curvePoints"]])

  return(list(
    data   = .bfdCurveByEffectSize(settings, effect, c("h1", "h0"), function(under) .bfdResultN1ForBasis(settings, result, under)),
    xLabel = .bfdDesignPriorEffectAxisLabel(settings)
  ))
}

.bfdBuildEffectSizePlot <- function(settings, result, under, plotData = NULL) {
  if (settings[["isBinomial"]])
    return(.bfdBuildBinomialProportionPlot(settings, result, under, plotData))

  if (is.null(plotData))
    plotData <- .bfdEffectSizePlotData(settings, result)

  unders <- if (is.null(under)) c("h1", "h0") else under
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

  if (settings[["calculationTarget"]] == "sampleSize") {
    plot <- plot +
      .bfdTargetPowerLine(settings, unders)
  }

  linetypeValues <- if (showUnder) .bfdUnderLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdDesignPriorEffectAxisLabel <- function(settings) {
  designPrior <- .bfdContinuousDesignPrior(settings, "h1")
  if (identical(designPrior[["distribution"]], "point"))
    return(gettext("Design prior location"))

  return(gettext("Design prior mean"))
}

.bfdBinomialProportionPlotData <- function(settings, result) {
  proportionAxis <- .bfdBinomialProportionAxis(settings)
  xRange         <- c(max(proportionAxis[["range"]][1], .Machine$double.eps), min(proportionAxis[["range"]][2], 1 - .Machine$double.eps))
  proportion     <- seq(xRange[1], xRange[2], length.out = settings[["curvePoints"]])

  return(list(
    data           = .bfdCurveByProportion(settings, proportion, c("h1", "h0"), function(under) .bfdResultN1ForBasis(settings, result, under)),
    proportionAxis = proportionAxis
  ))
}

.bfdBuildBinomialProportionPlot <- function(settings, result, under, plotData = NULL) {
  if (is.null(plotData))
    plotData <- .bfdBinomialProportionPlotData(settings, result)

  unders         <- if (is.null(under)) c("h1", "h0") else under
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

  if (settings[["calculationTarget"]] == "sampleSize") {
    plot <- plot +
      .bfdTargetPowerLine(settings, unders)
  }

  linetypeValues <- if (showUnder) .bfdUnderLinetypeValues() else NULL
  return(.bfdApplyPlotTheme(plot, settings, linetypeValues = linetypeValues))
}

.bfdPriorPlot <- function(jaspResults, settings) {
  .bfdPriorPlotContainer(
    jaspResults  = jaspResults,
    settings     = settings,
    key          = "evidencePriorPlot",
    position     = 13,
    dependencies = .bfdPriorPlotDependencies
  )
}

.bfdPriorPlotContainer <- function(jaspResults, settings, key, position, dependencies) {
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

    plotData <- try(.bfdPriorPlotData(settings, spec[["priorSet"]]), silent = TRUE)
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
    .bfdAddExplanationHtml(
      parent       = container,
      key          = paste0(spec[["key"]], "Note"),
      settings     = settings,
      position     = i + 0.1,
      dependencies = "explanatoryText",
      text         = .bfdPriorPlotExplanation()
    )
  }
}

.bfdPriorPlotExplanation <- function() {
  gettext(
    "Analysis priors determine how the Bayes factor is computed from the data. Design priors describe the data-generating assumptions used to evaluate the design, so comparing them helps assess whether the planning assumptions match the intended analysis."
  )
}

.bfdPriorPlotSpecs <- function(settings) {
  priorSets <- .bfdSelectedPriorSets(settings)
  if (length(priorSets) == 0)
    return(list())

  if (isTRUE(settings[["combineDesignAnalysisPriorFigures"]])) {
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
  if (isTRUE(settings[["designPriorDistributionFigure"]]))
    priorSets <- c(priorSets, "design")
  if (isTRUE(settings[["analysisPriorDistributionFigure"]]))
    priorSets <- c(priorSets, "analysis")

  return(priorSets)
}

.bfdPriorSetIncludes <- function(priorSet, value) {
  value %in% priorSet
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
  if (priorLabel %in% c(.bfdAnalysisPriorPlotLabel("h1"), .bfdDesignPriorUnderLabel("h1"), .bfdUnderLabel("h1")))
    return(gettext("H\u2081"))

  if (priorLabel %in% c(.bfdAnalysisPriorPlotLabel("h0"), .bfdDesignPriorUnderLabel("h0"), .bfdUnderLabel("h0")))
    return(gettext("H\u2080"))

  return(priorLabel)
}

.bfdPriorTypeAestheticLabel <- function(priorLabel) {
  if (priorLabel %in% c(.bfdAnalysisPriorPlotLabel("h1"), .bfdAnalysisPriorPlotLabel("h0")))
    return(gettext("Analysis Prior"))

  if (priorLabel %in% c(.bfdDesignPriorUnderLabel("h1"), .bfdDesignPriorUnderLabel("h0")))
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
    labels[labels == .bfdDesignPriorUnderLabel("h0")] <- .bfdUnderLabel("h0")
    labels[labels == .bfdDesignPriorUnderLabel("h1")] <- .bfdUnderLabel("h1")
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
    lapply(c("power", "misleading"), function(outcome) {
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
    lapply(c("power", "misleading"), function(outcome) {
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
    lapply(c("power", "misleading"), function(outcome) {
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

  if (under == "h1")
    return("h0")

  return("h1")
}

.bfdContinuousPriorPlotData <- function(settings, priorSet) {
  priorAxis   <- .bfdPriorAxis(settings, priorSet)
  x           <- seq(priorAxis[["range"]][1], priorAxis[["range"]][2], length.out = settings[["curvePoints"]])
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
      density = stats::dnorm(x, mean = .bfdZAnalysisPriorMean(settings), sd = .bfdZAnalysisPriorSd(settings)),
      prior   = .bfdAnalysisPriorPlotLabel("h1")
    )))
  }

  spread <- .bfdMomentPriorSpread(settings)
  density <- ((x - settings[["nullValue"]])^2 / spread^2) *
    stats::dnorm(x, mean = settings[["nullValue"]], sd = spread)

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
  mean      <- .bfdZAnalysisPriorMean(settings)
  sd        <- .bfdZAnalysisPriorSd(settings)
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
  tPriorDf   <- .bfdTPriorDf(settings)
  rawDensity <- stats::dt((x - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = tPriorDf) / settings[["tPriorScale"]]
  nullValue  <- settings[["nullValue"]]

  if (settings[["alternative"]] == "two.sided") {
    return(list(.bfdPriorDensityRows(
      x       = x,
      density = rawDensity,
      prior   = .bfdAnalysisPriorPlotLabel("h1")
    )))
  }

  if (settings[["alternative"]] == "greater") {
    normalizer <- 1 - stats::pt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = tPriorDf)
    density    <- rawDensity / normalizer
    atNull     <- stats::dt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = tPriorDf) / settings[["tPriorScale"]] / normalizer
    return(list(.bfdPriorDensityRows(
      x            = x,
      density      = density,
      prior        = .bfdAnalysisPriorPlotLabel("h1"),
      lower        = nullValue,
      lowerDensity = atNull
    )))
  } else {
    normalizer <- stats::pt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = tPriorDf)
    density    <- rawDensity / normalizer
    atNull     <- stats::dt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = tPriorDf) / settings[["tPriorScale"]] / normalizer
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
    designPrior <- .bfdContinuousDesignPrior(settings, under)
    if (designPrior[["distribution"]] == "normal") {
      rows[[length(rows) + 1]] <- .bfdPriorDensityRows(
        x       = x,
        density = stats::dnorm(x, mean = designPrior[["mean"]], sd = designPrior[["sd"]]),
        prior   = .bfdDesignPriorUnderLabel(under)
      )
    }
  }

  return(rows)
}

.bfdBinomialPriorPlotData <- function(settings, priorSet) {
  priorAxis   <- .bfdPriorAxis(settings, priorSet)
  xRange      <- c(max(priorAxis[["range"]][1], .Machine$double.eps), min(priorAxis[["range"]][2], 1 - .Machine$double.eps))
  x           <- seq(xRange[1], xRange[2], length.out = settings[["curvePoints"]])
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
    designPrior <- .bfdBinomialDesignPrior(settings, under)
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
        prior        = .bfdDesignPriorUnderLabel(under),
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
      rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(.bfdZAnalysisPriorMean(settings), .bfdAnalysisPriorPlotLabel("h1"), height)
    }
  }

  if (.bfdPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .bfdContinuousDesignPrior(settings, under)
      if (designPrior[["distribution"]] == "point")
        rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(designPrior[["mean"]], .bfdDesignPriorUnderLabel(under), height)
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
      designPrior <- .bfdBinomialDesignPrior(settings, under)
      if (designPrior[["distribution"]] == "point")
        rows[[length(rows) + 1]] <- .bfdPriorSpikeRow(designPrior[["proportion"]], .bfdDesignPriorUnderLabel(under), height)
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
    return(.bfdPriorSpikeRow(numeric(), character(), numeric()))
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
    return(settings[["knownStandardDeviation"]])

  n2 <- .bfdSampleSizeSecondGroup(settings, n1)
  return(settings[["knownStandardDeviation"]] * sqrt(1 + n1 / n2))
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
    effectSize                = 1
  )
}

.bfdGeneralZKnownUisdFootnote <- function(settings) {
  if (!isTRUE(settings[["isGeneralZ"]]))
    return(NULL)

  if (!settings[["generalZParameterization"]] %in% c(
    "standardizedMeanDifference",
    "fisherZCorrelation",
    "logRiskRatio",
    "logOddsRatio",
    "logHazardRatio",
    "logIncidenceRateRatio",
    "effectSize"
  ))
    return(NULL)

  gettextf(
    "The %1$s parameterization assumes a unit information standard deviation (UISD) of %2$s.",
    .bfdGeneralZParameterizationLabel(settings[["generalZParameterization"]]),
    .bfdFormatNumber(.bfdGeneralZUnitInformationSd(settings))
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
    unitInformationSd         = gettext("specified UISD"),
    standardErrorSchedule     = gettext("standard error schedule"),
    effectSize                = gettext("effect size")
  )
}

.bfdSampleSizeSecondGroup <- function(settings, n1) {
  if (!settings[["isIndependentSamples"]])
    return(n1)

  return(ceiling(n1 * settings[["sampleSizeAllocationRatio"]]))
}

.bfdValidateSampleSize <- function(settings, n1) {
  if (settings[["isIndependentSamples"]] && settings[["isTTest"]] && .bfdSampleSizeSecondGroup(settings, n1) <= 1)
    stop(gettext("The sample-size ratio leads to a second-group sample size smaller than 2."))
}

.bfdMinimumSampleSize <- function(settings) {
  minimumN <- max(ceiling(settings[["minimumSampleSize"]]), if (settings[["isBinomial"]]) 1 else 2)

  if (settings[["isIndependentSamples"]] && settings[["isTTest"]]) {
    while (.bfdSampleSizeSecondGroup(settings, minimumN) <= 1)
      minimumN <- minimumN + 1
  }

  return(minimumN)
}

.bfdBinomialDesignArguments <- function(settings, under = "h1") {
  designPrior <- .bfdBinomialDesignPrior(settings, under)
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
      rangeValues <- c(rangeValues, .bfdZAnalysisPriorMean(settings))
    } else if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
      rangeValues <- c(rangeValues, .bfdPriorInterval(.bfdZAnalysisPriorMean(settings), .bfdZAnalysisPriorSd(settings)))
    } else if (settings[["isZTest"]]) {
      rangeValues <- c(rangeValues, .bfdPriorInterval(settings[["nullValue"]], sqrt(3) * .bfdMomentPriorSpread(settings)))
    } else {
      rangeValues <- c(rangeValues, .bfdPriorInterval(settings[["tPriorLocation"]], .bfdStudentTPriorSpread(settings)))

      if (settings[["alternative"]] != "two.sided")
        rangeValues <- c(rangeValues, settings[["nullValue"]])
    }
  }

  if (.bfdPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .bfdContinuousDesignPrior(settings, under)
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
      designPrior <- .bfdBinomialDesignPrior(settings, under)
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
  tPriorDf <- .bfdTPriorDf(settings)
  if (tPriorDf > 2)
    return(settings[["tPriorScale"]] * sqrt(tPriorDf / (tPriorDf - 2)))

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
  designH0 <- .bfdContinuousDesignPrior(settings, "h0")
  designH1 <- .bfdContinuousDesignPrior(settings, "h1")
  anchors <- c(settings[["nullValue"]], designH0[["mean"]], designH1[["mean"]])

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "point") {
    anchors <- c(anchors, .bfdZAnalysisPriorMean(settings))
    spread  <- max(designH0[["sd"]], designH1[["sd"]], abs(diff(range(anchors))), 0.25)
  } else if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    anchors <- c(anchors, .bfdZAnalysisPriorMean(settings))
    spread  <- max(.bfdZAnalysisPriorSd(settings), designH0[["sd"]], designH1[["sd"]], abs(diff(range(anchors))), 0.25)
  } else if (settings[["isZTest"]]) {
    anchors <- c(anchors, settings[["nullValue"]] + c(-1, 1) * .bfdMomentPriorMode(settings))
    spread  <- max(.bfdMomentPriorSpread(settings), designH0[["sd"]], designH1[["sd"]], abs(diff(range(anchors))), 0.25)
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
  labels <- c(
    independentSamplesTTest = gettext("independent samples t-test"),
    pairedSamplesTTest      = gettext("paired samples t-test"),
    oneSampleTTest          = gettext("one sample t-test"),
    independentSamplesZTest = gettext("independent samples z-test"),
    pairedSamplesZTest      = gettext("paired samples z-test"),
    oneSampleZTest          = gettext("one sample z-test"),
    oneSampleProportion     = gettext("one sample proportion test"),
    generalZApproximation   = gettext("general z-approximation")
  )

  return(unname(labels[test]))
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

  if (settings[["isTTest"]] && .bfdAnalysisPriorIsCauchy(settings))
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
    if (.bfdAnalysisPriorIsCauchy(settings)) {
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
      .bfdFormatNumber(.bfdTPriorDf(settings))
    ))
  }

  if (settings[["analysisPriorDistribution"]] == "point") {
    return(gettextf(
      "location = %1$s",
      .bfdFormatNumber(.bfdZAnalysisPriorMean(settings))
    ))
  }

  if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    return(gettextf(
      "mean = %1$s, sd = %2$s",
      .bfdFormatNumber(.bfdZAnalysisPriorMean(settings)),
      .bfdFormatNumber(.bfdZAnalysisPriorSd(settings))
    ))
  }

  return(gettextf(
    "spread = %1$s, modes = +/- %2$s",
    .bfdFormatNumber(.bfdMomentPriorSpread(settings)),
    .bfdFormatNumber(.bfdMomentPriorMode(settings))
  ))
}

.bfdDesignPriorLabel <- function(settings, under = "h1") {
  if (settings[["isBinomial"]])
    return(.bfdBinomialDesignPrior(settings, under)[["label"]])

  return(.bfdContinuousDesignPrior(settings, under)[["label"]])
}

.bfdDesignPriorParameters <- function(settings, under = "h1") {
  if (settings[["isBinomial"]])
    return(.bfdBinomialDesignPrior(settings, under)[["parameters"]])

  return(.bfdContinuousDesignPrior(settings, under)[["parameters"]])
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

.bfdAnalysisPriorTableLabel <- function(settings) {
  label <- .bfdAnalysisPriorLabel(settings)

  if (identical(settings[["alternative"]], "greater"))
    return(paste0(label, "\u208A"))

  if (identical(settings[["alternative"]], "less"))
    return(paste0(label, "\u208B"))

  return(label)
}

.bfdPriorString <- function(distribution, parameters) {
  return(paste0(distribution, "(", parameters, ")"))
}

.bfdAnalysisPriorPlotLabel <- function(under) {
  if (under == "h0")
    return(gettext("Analysis Prior Under H\u2080"))

  return(gettext("Analysis Prior Under H\u2081"))
}
