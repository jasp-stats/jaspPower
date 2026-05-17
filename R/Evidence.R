Evidence <- function(jaspResults, dataset, options) {
  settings <- .evPrepareSettings(options)
  result   <- try(.evComputeResult(settings), silent = TRUE)

  .evResultsTable(jaspResults, settings, result)
  .evDesignOutcomeTable(jaspResults, settings, result)
  .evPriorsTable(jaspResults, settings)

  if (isTRUE(options[["text"]]))
    .evText(jaspResults, settings, result)

  if (isTRUE(options[["evidenceByEffectSize"]]) && !settings[["isBinomial"]])
    .evEffectSizePlot(jaspResults, settings, result)

  if (isTRUE(options[["evidenceBySampleSize"]]))
    .evSampleSizePlot(jaspResults, settings, result)

  if (isTRUE(options[["priorDistribution"]]))
    .evPriorPlot(jaspResults, settings)

  return()
}

.evDependencies <- c(
  "test", "calculation", "evidenceTarget", "bfThreshold",
  "evidenceProbability", "sampleSize", "sampleSizeRatio", "standardDeviation",
  "alternative", "nullPriorDistribution", "nullValue", "nullProportion",
  "analysisPriorDistribution", "analysisPriorMean", "analysisPriorSd",
  "momentPriorSpread", "momentPriorMode", "tPriorLocation", "tPriorScale",
  "tPriorDf", "analysisPriorSuccesses", "analysisPriorFailures",
  "designPrior", "designPriorMean", "designPriorSd", "binomialDesignPrior",
  "designProportion", "designPriorSuccesses", "designPriorFailures",
  "designPriorLower", "designPriorUpper", "sampleSizeRangeMin",
  "sampleSizeRangeMax"
)

.evPrepareSettings <- function(options) {
  test <- options[["test"]]

  settings <- list(
    test                 = test,
    testLabel            = .evTestLabel(test),
    testType             = .evTestType(test),
    isIndependentSamples = grepl("independentSamples", test, fixed = TRUE),
    isTTest              = grepl("TTest", test, fixed = TRUE),
    isZTest              = grepl("ZTest", test, fixed = TRUE),
    isBinomial           = identical(test, "oneSampleProportion"),
    calculation          = options[["calculation"]],
    evidenceTarget       = options[["evidenceTarget"]],
    bfThreshold          = options[["bfThreshold"]],
    targetProbability    = options[["evidenceProbability"]],
    sampleSize           = options[["sampleSize"]],
    sampleSizeRatio      = options[["sampleSizeRatio"]],
    rangeMin             = options[["sampleSizeRangeMin"]],
    rangeMax             = options[["sampleSizeRangeMax"]],
    plotPoints           = options[["plotPoints"]],
    showBothTargets      = options[["showBothEvidenceTargets"]]
  )

  settings[["eventK"]]    <- if (settings[["evidenceTarget"]] == "h1") 1 / settings[["bfThreshold"]] else settings[["bfThreshold"]]
  settings[["lowerTail"]] <- settings[["evidenceTarget"]] == "h1"

  if (settings[["isBinomial"]]) {
    settings <- .evAddBinomialSettings(settings, options)
  } else {
    settings <- .evAddContinuousSettings(settings, options)
  }

  return(settings)
}

.evAddContinuousSettings <- function(settings, options) {
  settings[["nullValue"]]         <- options[["nullValue"]]
  settings[["standardDeviation"]] <- options[["standardDeviation"]]
  settings[["alternative"]]       <- switch(options[["alternative"]], twoSided = "two.sided", options[["alternative"]])
  settings[["n1"]]                <- options[["sampleSize"]]
  settings[["n2"]]                <- if (settings[["isIndependentSamples"]]) ceiling(options[["sampleSize"]] * options[["sampleSizeRatio"]]) else options[["sampleSize"]]

  settings[["designPrior"]]     <- options[["designPrior"]]
  settings[["designPriorMean"]] <- options[["designPriorMean"]]
  settings[["designPriorSd"]]   <- if (options[["designPrior"]] == "point") 0 else options[["designPriorSd"]]

  if (settings[["isZTest"]]) {
    settings[["analysisPriorDistribution"]] <- options[["analysisPriorDistribution"]]
    settings[["analysisPriorMean"]]         <- options[["analysisPriorMean"]]
    settings[["analysisPriorSd"]]           <- options[["analysisPriorSd"]]
    settings[["momentPriorSpread"]]         <- if (options[["analysisPriorDistribution"]] == "normalMomentMode") {
      options[["momentPriorMode"]] / sqrt(2)
    } else {
      options[["momentPriorSpread"]]
    }
    settings[["momentPriorMode"]] <- sqrt(2) * settings[["momentPriorSpread"]]
  } else {
    settings[["analysisPriorDistribution"]] <- "t"
    settings[["tPriorLocation"]]            <- options[["tPriorLocation"]]
    settings[["tPriorScale"]]               <- options[["tPriorScale"]]
    settings[["tPriorDf"]]                  <- options[["tPriorDf"]]
  }

  return(settings)
}

.evAddBinomialSettings <- function(settings, options) {
  settings[["nullPriorDistribution"]] <- options[["nullPriorDistribution"]]
  settings[["nullProportion"]]        <- options[["nullProportion"]]
  settings[["analysisPriorSuccesses"]] <- options[["analysisPriorSuccesses"]]
  settings[["analysisPriorFailures"]]  <- options[["analysisPriorFailures"]]
  settings[["sampleSize"]]             <- options[["sampleSize"]]
  settings[["n1"]]                     <- options[["sampleSize"]]
  settings[["n2"]]                     <- NA_integer_

  settings[["binomialDesignPrior"]] <- options[["binomialDesignPrior"]]
  settings[["designProportion"]]    <- options[["designProportion"]]
  settings[["designPriorSuccesses"]] <- options[["designPriorSuccesses"]]
  settings[["designPriorFailures"]]  <- options[["designPriorFailures"]]
  settings[["designPriorLower"]]     <- options[["designPriorLower"]]
  settings[["designPriorUpper"]]     <- options[["designPriorUpper"]]

  return(settings)
}

.evComputeResult <- function(settings) {
  if (settings[["calculation"]] == "sampleSize") {
    n1 <- .evFindSampleSize(settings)
  } else {
    n1 <- settings[["sampleSize"]]
  }

  probability <- .evEvidenceProbability(settings, n1 = n1)
  n2          <- .evSampleSizeSecondGroup(settings, n1)

  return(list(
    n1          = n1,
    n2          = n2,
    probability = probability
  ))
}

.evEvidenceProbability <- function(settings, n1 = settings[["n1"]], target = settings[["evidenceTarget"]],
                                   designPriorMean = NULL, designPriorSd = NULL) {
  k         <- if (target == "h1") 1 / settings[["bfThreshold"]] else settings[["bfThreshold"]]
  lowerTail <- target == "h1"

  if (settings[["isBinomial"]]) {
    return(.evEvidenceProbabilityBinomial(settings, n1, k, lowerTail))
  }

  if (is.null(designPriorMean))
    designPriorMean <- settings[["designPriorMean"]]
  if (is.null(designPriorSd))
    designPriorSd <- settings[["designPriorSd"]]

  if (settings[["isZTest"]])
    return(.evEvidenceProbabilityZ(settings, n1, k, lowerTail, designPriorMean, designPriorSd))

  return(.evEvidenceProbabilityT(settings, n1, k, lowerTail, designPriorMean, designPriorSd))
}

.evEvidenceProbabilityZ <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  unitSd <- .evZUnitStandardDeviation(settings, n1)

  if (settings[["analysisPriorDistribution"]] == "normal") {
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

.evEvidenceProbabilityT <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  n2 <- .evSampleSizeSecondGroup(settings, n1)

  return(bfpwr::ptbf01(
    k           = k,
    n           = n1,
    n1          = n1,
    n2          = n2,
    null        = settings[["nullValue"]],
    plocation   = settings[["tPriorLocation"]],
    pscale      = settings[["tPriorScale"]],
    pdf         = settings[["tPriorDf"]],
    dpm         = designPriorMean,
    dpsd        = designPriorSd,
    type        = settings[["testType"]],
    alternative = settings[["alternative"]],
    lower.tail  = lowerTail
  ))
}

.evEvidenceProbabilityBinomial <- function(settings, n, k, lowerTail, designArguments = NULL) {
  if (is.null(designArguments))
    designArguments <- .evBinomialDesignArguments(settings)

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

.evFindSampleSize <- function(settings) {
  minimumN <- .evMinimumSampleSize(settings)
  maximumN <- ceiling(settings[["rangeMax"]])

  if (minimumN >= maximumN)
    stop(gettext("The minimum sample size must be smaller than the maximum sample size."))

  packageN <- try(.evFindSampleSizeWithBfpwr(settings, minimumN, maximumN), silent = TRUE)

  if (!jaspBase::isTryError(packageN) && length(packageN) == 1 && is.finite(packageN)) {
    n <- ceiling(packageN)
  } else {
    n <- .evFindSampleSizeBySearch(settings, minimumN, maximumN)
  }

  n <- max(minimumN, min(maximumN, n))
  n <- .evAdjustSampleSize(settings, n, minimumN, maximumN)

  return(n)
}

.evFindSampleSizeWithBfpwr <- function(settings, minimumN, maximumN) {
  nrange <- c(minimumN, maximumN)

  if (settings[["isBinomial"]]) {
    return(do.call(
      what = bfpwr::nbinbf01,
      args = c(
        list(
          k          = settings[["eventK"]],
          power      = settings[["targetProbability"]],
          p0         = settings[["nullProportion"]],
          type       = settings[["nullPriorDistribution"]],
          a          = settings[["analysisPriorSuccesses"]],
          b          = settings[["analysisPriorFailures"]],
          lower.tail = settings[["lowerTail"]],
          nrange     = nrange
        ),
        .evBinomialDesignArguments(settings)
      )
    ))
  }

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "normal") {
    return(bfpwr::nbf01(
      k          = settings[["eventK"]],
      power      = settings[["targetProbability"]],
      usd        = .evZUnitStandardDeviation(settings, minimumN),
      null       = settings[["nullValue"]],
      pm         = settings[["analysisPriorMean"]],
      psd        = settings[["analysisPriorSd"]],
      dpm        = settings[["designPriorMean"]],
      dpsd       = settings[["designPriorSd"]],
      nrange     = nrange,
      lower.tail = settings[["lowerTail"]]
    ))
  }

  if (settings[["isZTest"]]) {
    return(bfpwr::nnmbf01(
      k          = settings[["eventK"]],
      power      = settings[["targetProbability"]],
      usd        = .evZUnitStandardDeviation(settings, minimumN),
      null       = settings[["nullValue"]],
      psd        = settings[["momentPriorSpread"]],
      dpm        = settings[["designPriorMean"]],
      dpsd       = settings[["designPriorSd"]],
      nrange     = nrange,
      lower.tail = settings[["lowerTail"]]
    ))
  }

  if (settings[["isIndependentSamples"]] && settings[["sampleSizeRatio"]] != 1)
    stop(gettext("Sample-size search for unequal group sizes is handled internally."))

  return(bfpwr::ntbf01(
    k           = settings[["eventK"]],
    power       = settings[["targetProbability"]],
    null        = settings[["nullValue"]],
    plocation   = settings[["tPriorLocation"]],
    pscale      = settings[["tPriorScale"]],
    pdf         = settings[["tPriorDf"]],
    type        = settings[["testType"]],
    alternative = settings[["alternative"]],
    dpm         = settings[["designPriorMean"]],
    dpsd        = settings[["designPriorSd"]],
    lower.tail  = settings[["lowerTail"]],
    nrange      = nrange
  ))
}

.evFindSampleSizeBySearch <- function(settings, minimumN, maximumN) {
  lowerProbability <- .evEvidenceProbability(settings, n1 = minimumN)
  if (is.finite(lowerProbability) && lowerProbability >= settings[["targetProbability"]])
    return(minimumN)

  upperProbability <- .evEvidenceProbability(settings, n1 = maximumN)
  if (!is.finite(upperProbability) || upperProbability < settings[["targetProbability"]])
    stop(gettext("Target evidence probability is not reached within the selected sample-size range."))

  lower <- minimumN
  upper <- maximumN

  while ((upper - lower) > 1) {
    midpoint            <- floor((lower + upper) / 2)
    midpointProbability <- .evEvidenceProbability(settings, n1 = midpoint)

    if (is.finite(midpointProbability) && midpointProbability >= settings[["targetProbability"]]) {
      upper <- midpoint
    } else {
      lower <- midpoint
    }
  }

  return(upper)
}

.evAdjustSampleSize <- function(settings, n, minimumN, maximumN) {
  while (n <= maximumN && .evEvidenceProbability(settings, n1 = n) < settings[["targetProbability"]])
    n <- n + 1

  if (n > maximumN)
    stop(gettext("Target evidence probability is not reached within the selected sample-size range."))

  while (n > minimumN && .evEvidenceProbability(settings, n1 = n - 1) >= settings[["targetProbability"]])
    n <- n - 1

  return(n)
}

.evResultsTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceResults"]]))
    return()

  table <- createJaspTable(title = gettext("Evidence Analysis"))
  table$dependOn(.evDependencies)
  table$position <- 1
  jaspResults[["evidenceResults"]] <- table

  .evAddResultsTableColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute evidence results: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evResultsRow(settings, result))

  if (settings[["calculation"]] == "sampleSize")
    table$addFootnote(gettext("Due to rounding of the sample size, the actual evidence probability can deviate from the target probability."))

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested sample-size ratio."))
}

.evAddResultsTableColumns <- function(table, settings) {
  computed <- gettext("Computed")
  userDefined <- gettext("User Defined")

  if (settings[["calculation"]] == "sampleSize") {
    if (settings[["isIndependentSamples"]]) {
      table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer", overtitle = computed)
      table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer", overtitle = computed)
    } else {
      table$addColumnInfo(name = "n", title = "N", type = "integer", overtitle = computed)
    }
    table$addColumnInfo(name = "probability", title = .evProbabilityColumnTitle(settings), type = "number", overtitle = computed)
    table$addColumnInfo(name = "targetProbability", title = gettext("Target probability"), type = "number", overtitle = userDefined)
  } else {
    table$addColumnInfo(name = "probability", title = .evProbabilityColumnTitle(settings), type = "number", overtitle = computed)
    if (settings[["isIndependentSamples"]]) {
      table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer", overtitle = userDefined)
      table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer", overtitle = userDefined)
    } else {
      table$addColumnInfo(name = "n", title = "N", type = "integer", overtitle = userDefined)
    }
  }

  table$addColumnInfo(name = "threshold", title = .evThresholdColumnTitle(settings), type = "number", overtitle = userDefined)
  table$addColumnInfo(name = "target", title = gettext("Evidence for"), type = "string", overtitle = userDefined)
}

.evResultsRow <- function(settings, result) {
  row <- data.frame(
    probability       = result[["probability"]],
    targetProbability = settings[["targetProbability"]],
    threshold         = settings[["bfThreshold"]],
    target            = .evTargetLabel(settings[["evidenceTarget"]]),
    stringsAsFactors  = FALSE
  )

  if (settings[["isIndependentSamples"]]) {
    row[["n1"]] <- result[["n1"]]
    row[["n2"]] <- result[["n2"]]
  } else {
    row[["n"]] <- result[["n1"]]
  }

  if (settings[["calculation"]] != "sampleSize")
    row[["targetProbability"]] <- NULL

  return(row)
}

.evDesignOutcomeTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceDesignOutcome"]]))
    return()

  table <- createJaspTable(title = gettext("Design Outcome"))
  table$dependOn(.evDependencies)
  table$position <- 2
  jaspResults[["evidenceDesignOutcome"]] <- table

  table$addColumnInfo(name = "under",       title = gettext("Under"),       type = "string")
  table$addColumnInfo(name = "null",        title = gettext("Null"),        type = "number", overtitle = gettext("Evidence"))
  table$addColumnInfo(name = "undecided",   title = gettext("Undecided"),   type = "number", overtitle = gettext("Evidence"))
  table$addColumnInfo(name = "alternative", title = gettext("Alternative"), type = "number", overtitle = gettext("Evidence"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute design outcome: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evDesignOutcomeRows(settings, result))
  table$addFootnote(gettext("Probabilities are evaluated at the resulting sample size. The H\u2081 row uses the design prior; the H\u2080 row uses the null prior."))
}

.evDesignOutcomeRows <- function(settings, result) {
  h1Outcome <- .evDesignOutcomeProbabilities(settings, result[["n1"]], under = "h1")
  h0Outcome <- .evDesignOutcomeProbabilities(settings, result[["n1"]], under = "h0")

  return(data.frame(
    under       = c(gettext("H\u2081"), gettext("H\u2080")),
    null        = c(h1Outcome[["null"]],        h0Outcome[["null"]]),
    undecided   = c(h1Outcome[["undecided"]],   h0Outcome[["undecided"]]),
    alternative = c(h1Outcome[["alternative"]], h0Outcome[["alternative"]]),
    stringsAsFactors = FALSE
  ))
}

.evDesignOutcomeProbabilities <- function(settings, n1, under) {
  alternative <- .evDesignOutcomeEvidenceProbability(settings, n1, target = "h1", under = under)
  null        <- .evDesignOutcomeEvidenceProbability(settings, n1, target = "h0", under = under)
  undecided   <- 1 - alternative - null

  return(c(
    null        = .evClampProbability(null),
    undecided   = .evClampProbability(undecided),
    alternative = .evClampProbability(alternative)
  ))
}

.evDesignOutcomeEvidenceProbability <- function(settings, n1, target, under) {
  if (under == "h1")
    return(.evEvidenceProbability(settings, n1 = n1, target = target))

  if (!settings[["isBinomial"]]) {
    return(.evEvidenceProbability(
      settings,
      n1              = n1,
      target          = target,
      designPriorMean = settings[["nullValue"]],
      designPriorSd   = 0
    ))
  }

  k               <- if (target == "h1") 1 / settings[["bfThreshold"]] else settings[["bfThreshold"]]
  lowerTail       <- target == "h1"
  designArguments <- .evBinomialNullDesignArguments(settings)

  return(.evEvidenceProbabilityBinomial(settings, n1, k, lowerTail, designArguments))
}

.evPriorsTable <- function(jaspResults, settings) {
  if (!is.null(jaspResults[["evidencePriors"]]))
    return()

  table <- createJaspTable(title = gettext("Priors"))
  table$dependOn(.evDependencies)
  table$position <- 3
  jaspResults[["evidencePriors"]] <- table

  table$addColumnInfo(name = "section", title = gettext("Section"), type = "string")
  table$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
  table$addColumnInfo(name = "parameters", title = gettext("Parameters"), type = "string")

  table$setData(data.frame(
    section      = c(gettext("Prior Under H\u2080"), gettext("Prior Under H\u2081"), gettext("Design Prior")),
    distribution = c(.evNullPriorLabel(settings), .evAnalysisPriorLabel(settings), .evDesignPriorLabel(settings)),
    parameters   = c(.evNullPriorParameters(settings), .evAnalysisPriorParameters(settings), .evDesignPriorParameters(settings)),
    stringsAsFactors = FALSE
  ))
}

.evText <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceText"]]))
    return()

  html <- createJaspHtml(title = gettext("Explanation"))
  html$dependOn(c(.evDependencies, "text"))
  html$position <- 4
  jaspResults[["evidenceText"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested evidence calculation could not be completed with the current settings.")
    return()
  }

  probabilityText <- .evFormatNumber(result[["probability"]])
  thresholdText   <- .evThresholdText(settings)

  if (settings[["calculation"]] == "sampleSize") {
    sampleText <- if (settings[["isIndependentSamples"]]) {
      gettextf("The smallest sample size found is N\u2081 = %1$s and N\u2082 = %2$s.", result[["n1"]], result[["n2"]])
    } else {
      gettextf("The smallest sample size found is N = %1$s.", result[["n1"]])
    }
    calculationText <- gettextf(
      "%1$s At this sample size, the evidence probability is %2$s.",
      sampleText,
      probabilityText
    )
  } else {
    sampleText <- if (settings[["isIndependentSamples"]]) {
      gettextf("With N\u2081 = %1$s and N\u2082 = %2$s", result[["n1"]], result[["n2"]])
    } else {
      gettextf("With N = %1$s", result[["n1"]])
    }
    calculationText <- gettextf("%1$s, the evidence probability is %2$s.", sampleText, probabilityText)
  }

  html[["text"]] <- paste0(
    "<p>",
    gettextf(
      "This fixed-N design computes the probability of obtaining %1$s for %2$s, averaging over the selected design prior.",
      thresholdText,
      settings[["testLabel"]]
    ),
    "</p><p>",
    calculationText,
    "</p>"
  )
}

.evSampleSizePlot <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceBySampleSizePlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Evidence Curve by N"), width = 735, height = 350)
  plot$dependOn(c(.evDependencies, "evidenceBySampleSize", "showBothEvidenceTargets", "plotPoints"))
  plot$position <- 6
  jaspResults[["evidenceBySampleSizePlot"]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute evidence curve: %1$s", .evCleanError(result)))
    return()
  }

  plotResult <- try(.evBuildSampleSizePlot(settings, result), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute evidence curve: %1$s", .evCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.evBuildSampleSizePlot <- function(settings, result) {
  minimumN <- .evMinimumSampleSize(settings)
  maximumN <- max(result[["n1"]] * 2, settings[["sampleSize"]] * 2, minimumN + 20, 50)

  if (settings[["calculation"]] == "sampleSize")
    maximumN <- min(settings[["rangeMax"]], maximumN)

  nValues <- unique(ceiling(exp(seq(log(minimumN), log(maximumN), length.out = settings[["plotPoints"]]))))
  targets <- if (isTRUE(settings[["showBothTargets"]])) c("h1", "h0") else settings[["evidenceTarget"]]
  data    <- .evCurveBySampleSize(settings, nValues, targets)

  xLabel <- if (settings[["isIndependentSamples"]]) gettext("Sample size (group 1)") else gettext("Sample size")

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = target)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(x = xLabel, y = gettext("Evidence probability"), color = gettext("Target")) +
    .evSegment(x = result[["n1"]], xend = result[["n1"]], y = 0, yend = result[["probability"]]) +
    .evSegment(x = min(nValues), xend = result[["n1"]], y = result[["probability"]], yend = result[["probability"]])

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      ggplot2::geom_hline(yintercept = settings[["targetProbability"]], linetype = "dotted", color = "#555555")
  }

  return(.pwrApplyPlotTheme(plot))
}

.evEffectSizePlot <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceByEffectSizePlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Evidence Curve by Effect Size"), width = 735, height = 350)
  plot$dependOn(c(.evDependencies, "evidenceByEffectSize", "showBothEvidenceTargets", "plotPoints"))
  plot$position <- 5
  jaspResults[["evidenceByEffectSizePlot"]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute evidence curve: %1$s", .evCleanError(result)))
    return()
  }

  plotResult <- try(.evBuildEffectSizePlot(settings, result), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute evidence curve: %1$s", .evCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.evBuildEffectSizePlot <- function(settings, result) {
  effectRange <- .evEffectRange(settings)
  effect      <- seq(effectRange[1], effectRange[2], length.out = settings[["plotPoints"]])
  targets     <- if (isTRUE(settings[["showBothTargets"]])) c("h1", "h0") else settings[["evidenceTarget"]]
  data        <- .evCurveByEffectSize(settings, effect, targets, result[["n1"]])

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = effect, y = probability, color = target)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(x = gettext("Design prior mean"), y = gettext("Evidence probability"), color = gettext("Target")) +
    .evSegment(x = settings[["designPriorMean"]], xend = settings[["designPriorMean"]], y = 0, yend = result[["probability"]]) +
    .evSegment(x = min(effect), xend = settings[["designPriorMean"]], y = result[["probability"]], yend = result[["probability"]])

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      ggplot2::geom_hline(yintercept = settings[["targetProbability"]], linetype = "dotted", color = "#555555")
  }

  return(.pwrApplyPlotTheme(plot))
}

.evPriorPlot <- function(jaspResults, settings) {
  if (!is.null(jaspResults[["evidencePriorPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Prior Distribution"), width = 735, height = 350)
  plot$dependOn(c(.evDependencies, "priorDistribution", "plotPoints"))
  plot$position <- 7
  jaspResults[["evidencePriorPlot"]] <- plot

  plotResult <- try(.evBuildPriorPlot(settings), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute prior distribution plot: %1$s", .evCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.evBuildPriorPlot <- function(settings) {
  if (settings[["isBinomial"]])
    return(.evBuildBinomialPriorPlot(settings))

  return(.evBuildContinuousPriorPlot(settings))
}

.evCurveBySampleSize <- function(settings, nValues, targets) {
  rows <- lapply(targets, function(target) {
    data.frame(
      n           = nValues,
      probability = .evEvidenceProbability(settings, n1 = nValues, target = target),
      target      = .evTargetLabel(target),
      stringsAsFactors = FALSE
    )
  })

  return(do.call(rbind, rows))
}

.evCurveByEffectSize <- function(settings, effect, targets, n1) {
  rows <- lapply(targets, function(target) {
    data.frame(
      effect      = effect,
      probability = .evEvidenceProbability(settings, n1 = n1, target = target, designPriorMean = effect),
      target      = .evTargetLabel(target),
      stringsAsFactors = FALSE
    )
  })

  return(do.call(rbind, rows))
}

.evBuildContinuousPriorPlot <- function(settings) {
  priorAxis   <- .evPriorAxis(settings)
  x           <- seq(priorAxis[["range"]][1], priorAxis[["range"]][2], length.out = settings[["plotPoints"]])
  densityData <- .evContinuousPriorDensityData(settings, x)

  plot <- ggplot2::ggplot(densityData, ggplot2::aes(x = x, y = density, color = prior)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::labs(y = gettext("Density"), color = gettext("Prior")) +
    jaspGraphs::scale_x_continuous(
      name   = gettext("Parameter value"),
      breaks = priorAxis[["breaks"]],
      limits = priorAxis[["range"]]
    )

  plot <- plot + ggplot2::geom_vline(xintercept = settings[["nullValue"]], linetype = "dashed", color = "#444444")

  if (settings[["designPrior"]] == "point")
    plot <- plot + ggplot2::geom_vline(xintercept = settings[["designPriorMean"]], linetype = "dotted", color = "#0072B2")

  return(.pwrApplyPlotTheme(plot))
}

.evContinuousPriorDensityData <- function(settings, x) {
  h1Density <- if (settings[["isZTest"]]) {
    if (settings[["analysisPriorDistribution"]] == "normal") {
      stats::dnorm(x, mean = settings[["analysisPriorMean"]], sd = settings[["analysisPriorSd"]])
    } else {
      ((x - settings[["nullValue"]])^2 / settings[["momentPriorSpread"]]^2) *
        stats::dnorm(x, mean = settings[["nullValue"]], sd = settings[["momentPriorSpread"]])
    }
  } else {
    .evTAnalysisPriorDensity(settings, x)
  }

  rows <- list(data.frame(
    x       = x,
    density = h1Density,
    prior   = gettext("Prior Under H\u2081"),
    stringsAsFactors = FALSE
  ))

  if (settings[["designPrior"]] == "normal") {
    rows[[length(rows) + 1]] <- data.frame(
      x       = x,
      density = stats::dnorm(x, mean = settings[["designPriorMean"]], sd = settings[["designPriorSd"]]),
      prior   = gettext("Design Prior"),
      stringsAsFactors = FALSE
    )
  }

  return(do.call(rbind, rows))
}

.evTAnalysisPriorDensity <- function(settings, x) {
  rawDensity <- stats::dt((x - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]]) / settings[["tPriorScale"]]

  if (settings[["alternative"]] == "two.sided")
    return(rawDensity)

  if (settings[["alternative"]] == "greater") {
    normalizer <- 1 - stats::pt((0 - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]])
    rawDensity[x <= 0] <- 0
  } else {
    normalizer <- stats::pt((0 - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]])
    rawDensity[x >= 0] <- 0
  }

  return(rawDensity / normalizer)
}

.evBuildBinomialPriorPlot <- function(settings) {
  priorAxis   <- .evPriorAxis(settings)
  xRange      <- c(max(priorAxis[["range"]][1], .Machine$double.eps), min(priorAxis[["range"]][2], 1 - .Machine$double.eps))
  x           <- seq(xRange[1], xRange[2], length.out = settings[["plotPoints"]])
  densityData <- .evBinomialPriorDensityData(settings, x)

  plot <- ggplot2::ggplot(densityData, ggplot2::aes(x = x, y = density, color = prior)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::labs(y = gettext("Density"), color = gettext("Prior")) +
    jaspGraphs::scale_x_continuous(
      name   = gettext("Proportion"),
      breaks = priorAxis[["breaks"]],
      limits = priorAxis[["range"]]
    ) +
    ggplot2::geom_vline(xintercept = settings[["nullProportion"]], linetype = "dashed", color = "#444444")

  if (settings[["binomialDesignPrior"]] == "point")
    plot <- plot + ggplot2::geom_vline(xintercept = settings[["designProportion"]], linetype = "dotted", color = "#0072B2")

  return(.pwrApplyPlotTheme(plot))
}

.evBinomialPriorDensityData <- function(settings, x) {
  h1Density <- stats::dbeta(x, settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])

  if (settings[["nullPriorDistribution"]] == "direction") {
    h1Normalizer <- stats::pbeta(settings[["nullProportion"]], settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]], lower.tail = FALSE)
    h1Density[x <= settings[["nullProportion"]]] <- 0
    h1Density <- h1Density / h1Normalizer

    h0Density <- stats::dbeta(x, settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])
    h0Normalizer <- stats::pbeta(settings[["nullProportion"]], settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])
    h0Density[x > settings[["nullProportion"]]] <- 0
    h0Density <- h0Density / h0Normalizer
  } else {
    h0Density <- NULL
  }

  rows <- list(data.frame(
    x       = x,
    density = h1Density,
    prior   = gettext("Prior Under H\u2081"),
    stringsAsFactors = FALSE
  ))

  if (!is.null(h0Density)) {
    rows[[length(rows) + 1]] <- data.frame(
      x       = x,
      density = h0Density,
      prior   = gettext("Prior Under H\u2080"),
      stringsAsFactors = FALSE
    )
  }

  if (settings[["binomialDesignPrior"]] == "beta") {
    designDensity <- stats::dbeta(x, settings[["designPriorSuccesses"]], settings[["designPriorFailures"]])
    designDensity[x < settings[["designPriorLower"]] | x > settings[["designPriorUpper"]]] <- 0
    designNormalizer <- diff(stats::pbeta(c(settings[["designPriorLower"]], settings[["designPriorUpper"]]),
                                          settings[["designPriorSuccesses"]],
                                          settings[["designPriorFailures"]]))
    rows[[length(rows) + 1]] <- data.frame(
      x       = x,
      density = designDensity / designNormalizer,
      prior   = gettext("Design Prior"),
      stringsAsFactors = FALSE
    )
  }

  return(do.call(rbind, rows))
}

.evZUnitStandardDeviation <- function(settings, n1) {
  if (!settings[["isIndependentSamples"]])
    return(settings[["standardDeviation"]])

  n2 <- .evSampleSizeSecondGroup(settings, n1)
  return(settings[["standardDeviation"]] * sqrt(1 + n1 / n2))
}

.evSampleSizeSecondGroup <- function(settings, n1) {
  if (!settings[["isIndependentSamples"]])
    return(n1)

  return(ceiling(n1 * settings[["sampleSizeRatio"]]))
}

.evMinimumSampleSize <- function(settings) {
  minimumN <- max(ceiling(settings[["rangeMin"]]), if (settings[["isBinomial"]]) 1 else 2)

  if (settings[["isIndependentSamples"]] && settings[["isTTest"]]) {
    while (.evSampleSizeSecondGroup(settings, minimumN) <= 1)
      minimumN <- minimumN + 1
  }

  return(minimumN)
}

.evBinomialDesignArguments <- function(settings) {
  if (settings[["binomialDesignPrior"]] == "point")
    return(list(dp = settings[["designProportion"]]))

  return(list(
    dp = NA_real_,
    da = settings[["designPriorSuccesses"]],
    db = settings[["designPriorFailures"]],
    dl = settings[["designPriorLower"]],
    du = settings[["designPriorUpper"]]
  ))
}

.evBinomialNullDesignArguments <- function(settings) {
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

.evPriorAxis <- function(settings) {
  requestedRange <- if (settings[["isBinomial"]]) .evBinomialPriorRange(settings) else .evContinuousPriorRange(settings)
  breaks         <- .evPrettyPriorBreaks(requestedRange, isBinomial = settings[["isBinomial"]])

  return(list(
    breaks = breaks,
    range  = range(breaks)
  ))
}

.evContinuousPriorRange <- function(settings) {
  rangeValues <- c(settings[["nullValue"]])

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "normal") {
    rangeValues <- c(rangeValues, .evPriorInterval(settings[["analysisPriorMean"]], settings[["analysisPriorSd"]]))
  } else if (settings[["isZTest"]]) {
    rangeValues <- c(rangeValues, .evPriorInterval(settings[["nullValue"]], sqrt(3) * settings[["momentPriorSpread"]]))
  } else {
    rangeValues <- c(rangeValues, .evPriorInterval(settings[["tPriorLocation"]], .evStudentTPriorSpread(settings)))

    if (settings[["alternative"]] != "two.sided")
      rangeValues <- c(rangeValues, 0)
  }

  if (settings[["designPrior"]] == "normal") {
    rangeValues <- c(rangeValues, .evPriorInterval(settings[["designPriorMean"]], settings[["designPriorSd"]]))
  } else {
    rangeValues <- c(rangeValues, settings[["designPriorMean"]])
  }

  return(.evFiniteRange(rangeValues))
}

.evBinomialPriorRange <- function(settings) {
  rangeValues <- c(
    settings[["nullProportion"]],
    .evBetaPriorInterval(settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])
  )

  if (settings[["binomialDesignPrior"]] == "point") {
    rangeValues <- c(rangeValues, settings[["designProportion"]])
  } else {
    rangeValues <- c(rangeValues, .evBetaPriorInterval(
      settings[["designPriorSuccesses"]],
      settings[["designPriorFailures"]],
      lower = settings[["designPriorLower"]],
      upper = settings[["designPriorUpper"]]
    ))
  }

  rangeValues <- pmin(1, pmax(0, rangeValues))

  return(.evFiniteRange(rangeValues, fallback = c(0, 1)))
}

.evPriorInterval <- function(center, sd) {
  if (!is.finite(sd) || sd <= 0)
    return(c(center, center))

  return(center + c(-1, 1) * 2.5 * sd)
}

.evBetaPriorInterval <- function(a, b, lower = 0, upper = 1) {
  mean <- a / (a + b)
  sd   <- sqrt(a * b / ((a + b)^2 * (a + b + 1)))

  interval <- .evPriorInterval(mean, sd)
  interval <- c(max(lower, interval[1]), min(upper, interval[2]))

  if (interval[1] >= interval[2])
    return(c(lower, upper))

  return(interval)
}

.evStudentTPriorSpread <- function(settings) {
  if (settings[["tPriorDf"]] > 2)
    return(settings[["tPriorScale"]] * sqrt(settings[["tPriorDf"]] / (settings[["tPriorDf"]] - 2)))

  return(settings[["tPriorScale"]])
}

.evPrettyPriorBreaks <- function(x, isBinomial) {
  x <- .evFiniteRange(x, fallback = if (isBinomial) c(0, 1) else c(-1, 1))

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

.evFiniteRange <- function(x, fallback = c(-1, 1)) {
  x <- x[is.finite(x)]

  if (length(x) == 0)
    return(fallback)

  return(range(x))
}

.evEffectRange <- function(settings) {
  anchors <- c(settings[["nullValue"]], settings[["designPriorMean"]])

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "normal") {
    anchors <- c(anchors, settings[["analysisPriorMean"]])
    spread  <- max(settings[["analysisPriorSd"]], settings[["designPriorSd"]], abs(diff(range(anchors))), 0.25)
  } else if (settings[["isZTest"]]) {
    anchors <- c(anchors, settings[["nullValue"]] + c(-1, 1) * settings[["momentPriorMode"]])
    spread  <- max(settings[["momentPriorSpread"]], settings[["designPriorSd"]], abs(diff(range(anchors))), 0.25)
  } else {
    anchors <- c(anchors, settings[["tPriorLocation"]])
    spread  <- max(settings[["tPriorScale"]], settings[["designPriorSd"]], abs(diff(range(anchors))), 0.25)
  }

  lower <- min(anchors) - 1.5 * spread
  upper <- max(anchors) + 1.5 * spread

  return(c(lower, upper))
}

.evTestType <- function(test) {
  switch(test,
    independentSamplesTTest = "two.sample",
    independentSamplesZTest = "two.sample",
    pairedSamplesTTest      = "paired",
    pairedSamplesZTest      = "paired",
    oneSampleTTest          = "one.sample",
    oneSampleZTest          = "one.sample",
    oneSampleProportion     = "binomial"
  )
}

.evTestLabel <- function(test) {
  switch(test,
    independentSamplesTTest = gettext("independent samples t-test"),
    pairedSamplesTTest      = gettext("paired samples t-test"),
    oneSampleTTest          = gettext("one sample t-test"),
    independentSamplesZTest = gettext("independent samples z-test"),
    pairedSamplesZTest      = gettext("paired samples z-test"),
    oneSampleZTest          = gettext("one sample z-test"),
    oneSampleProportion     = gettext("one sample proportion test")
  )
}

.evTargetLabel <- function(target) {
  if (target == "h1")
    return(gettext("H\u2081 (BF\u2081\u2080)"))

  return(gettext("H\u2080 (BF\u2080\u2081)"))
}

.evProbabilityColumnTitle <- function(settings) {
  if (settings[["evidenceTarget"]] == "h1")
    return("Pr(BF\u2081\u2080 \u2265 k)")

  return("Pr(BF\u2080\u2081 \u2265 k)")
}

.evThresholdColumnTitle <- function(settings) {
  if (settings[["evidenceTarget"]] == "h1")
    return("BF\u2081\u2080")

  return("BF\u2080\u2081")
}

.evThresholdText <- function(settings) {
  if (settings[["evidenceTarget"]] == "h1")
    return(gettextf("BF\u2081\u2080 \u2265 %1$s", .evFormatNumber(settings[["bfThreshold"]])))

  return(gettextf("BF\u2080\u2081 \u2265 %1$s", .evFormatNumber(settings[["bfThreshold"]])))
}

.evNullPriorLabel <- function(settings) {
  if (!settings[["isBinomial"]])
    return(gettext("Point null"))

  if (settings[["nullPriorDistribution"]] == "direction")
    return(gettext("Directional"))

  return(gettext("Point null"))
}

.evNullPriorParameters <- function(settings) {
  if (!settings[["isBinomial"]])
    return(gettextf("\u03B8\u2080 = %1$s", .evFormatNumber(settings[["nullValue"]])))

  if (settings[["nullPriorDistribution"]] == "direction")
    return(gettextf("p <= p\u2080, p\u2080 = %1$s", .evFormatNumber(settings[["nullProportion"]])))

  return(gettextf("p\u2080 = %1$s", .evFormatNumber(settings[["nullProportion"]])))
}

.evAnalysisPriorLabel <- function(settings) {
  if (settings[["isBinomial"]])
    return(gettext("Beta"))

  if (settings[["isTTest"]])
    return(gettext("Student-t"))

  if (settings[["analysisPriorDistribution"]] == "normal")
    return(gettext("Normal"))

  return(gettext("Normal-moment"))
}

.evAnalysisPriorParameters <- function(settings) {
  if (settings[["isBinomial"]]) {
    return(gettextf(
      "a = %1$s, b = %2$s",
      .evFormatNumber(settings[["analysisPriorSuccesses"]]),
      .evFormatNumber(settings[["analysisPriorFailures"]])
    ))
  }

  if (settings[["isTTest"]]) {
    return(gettextf(
      "location = %1$s, scale = %2$s, df = %3$s",
      .evFormatNumber(settings[["tPriorLocation"]]),
      .evFormatNumber(settings[["tPriorScale"]]),
      .evFormatNumber(settings[["tPriorDf"]])
    ))
  }

  if (settings[["analysisPriorDistribution"]] == "normal") {
    return(gettextf(
      "mean = %1$s, scale = %2$s",
      .evFormatNumber(settings[["analysisPriorMean"]]),
      .evFormatNumber(settings[["analysisPriorSd"]])
    ))
  }

  return(gettextf(
    "spread = %1$s, modes = +/- %2$s",
    .evFormatNumber(settings[["momentPriorSpread"]]),
    .evFormatNumber(settings[["momentPriorMode"]])
  ))
}

.evDesignPriorLabel <- function(settings) {
  if (settings[["isBinomial"]]) {
    if (settings[["binomialDesignPrior"]] == "point")
      return(gettext("Point proportion"))
    return(gettext("Beta"))
  }

  if (settings[["designPrior"]] == "point")
    return(gettext("Point"))

  return(gettext("Normal"))
}

.evDesignPriorParameters <- function(settings) {
  if (settings[["isBinomial"]]) {
    if (settings[["binomialDesignPrior"]] == "point")
      return(gettextf("p = %1$s", .evFormatNumber(settings[["designProportion"]])))

    return(gettextf(
      "a = %1$s, b = %2$s, lower = %3$s, upper = %4$s",
      .evFormatNumber(settings[["designPriorSuccesses"]]),
      .evFormatNumber(settings[["designPriorFailures"]]),
      .evFormatNumber(settings[["designPriorLower"]]),
      .evFormatNumber(settings[["designPriorUpper"]])
    ))
  }

  if (settings[["designPrior"]] == "point")
    return(gettextf("mean = %1$s", .evFormatNumber(settings[["designPriorMean"]])))

  return(gettextf(
    "mean = %1$s, sd = %2$s",
    .evFormatNumber(settings[["designPriorMean"]]),
    .evFormatNumber(settings[["designPriorSd"]])
  ))
}

.evSegment <- function(...) {
  ggplot2::annotate(
    geom     = "segment",
    linetype = "dashed",
    color    = "#333333",
    ...
  )
}

.evCleanError <- function(error) {
  message <- as.character(error)
  message <- gsub("\n", " ", message, fixed = TRUE)
  message <- sub("^Error[^:]*: ", "", message)

  return(message)
}

.evClampProbability <- function(x) {
  if (!is.finite(x))
    return(NA_real_)

  return(max(0, min(1, x)))
}

.evFormatNumber <- function(x) {
  format(signif(x, 4), trim = TRUE)
}
