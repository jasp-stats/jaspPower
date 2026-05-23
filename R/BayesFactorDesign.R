BayesFactorDesign <- function(jaspResults, dataset, options) {
  settings   <- .evPrepareSettings(options)
  validation <- try(.evValidateSettings(settings), silent = TRUE)
  result     <- if (jaspBase::isTryError(validation)) validation else try(.evComputeResult(settings), silent = TRUE)

  .evResultsTable(jaspResults, settings, result)
  .evDesignOutcomeTable(jaspResults, settings, result)
  .evPriorsTable(jaspResults, settings, validation)

  if (.evObservedAnalysisReady(dataset, options, settings))
    .evObservedAnalysisTable(jaspResults, dataset, options, settings, key = "evidenceObservedAnalysis", position = 4)

  if (isTRUE(options[["text"]]))
    .evText(jaspResults, settings, result)

  if (isTRUE(options[["generateReport"]]))
    .evReport(jaspResults, settings, result)

  if (isTRUE(options[["generateRCode"]]))
    .evRCode(jaspResults, settings, result, validation)

  if (isTRUE(options[["evidenceByEffectSize"]]))
    .evEffectSizePlot(jaspResults, settings, result)

  if (isTRUE(options[["evidenceBySampleSize"]]))
    .evSampleSizePlot(jaspResults, settings, result)

  if (isTRUE(options[["priorDistribution"]]))
    .evPriorPlot(jaspResults, settings, validation)

  return()
}

.evDependencies <- c(
  "test", "calculation", "evidenceTarget", "bfThreshold", "bf10Threshold", "bf01Threshold",
  "evidenceProbability", "targetPowerH1", "targetPowerH0",
  "sampleSize", "sampleSizeRatio", "standardDeviation",
  "generalZParameterization", "unitInformationSd",
  "alternative", "nullPriorDistribution", "nullValue", "nullProportion",
  "analysisPriorDistribution", "analysisPriorPoint", "analysisPriorMean",
  "analysisPriorSd", "momentPriorSpread", "momentPriorMode", "tPriorLocation",
  "tPriorScale", "tPriorDf", "analysisPriorSuccesses", "analysisPriorFailures",
  "designNullPrior", "designNullPriorMean", "designNullPriorSd",
  "binomialDesignNullPrior", "designNullProportion",
  "designNullPriorSuccesses", "designNullPriorFailures", "designNullPriorLower",
  "designNullPriorUpper", "designPrior", "designPriorMean", "designPriorSd", "binomialDesignPrior",
  "designProportion", "designPriorSuccesses", "designPriorFailures",
  "designPriorLower", "designPriorUpper", "sampleSizeRangeMin",
  "sampleSizeRangeMax", "drangeMode", "drangeLower", "drangeUpper"
)

.evObservedDependencies <- c(
  "observedAnalysisInput", "observedInputType",
  "observedN", "observedN1", "observedN2", "observedMean", "observedMean1",
  "observedMean2", "observedSd", "observedSd1", "observedSd2",
  "observedMeanDifference", "observedSdDifference", "observedEstimate",
  "observedStandardError", "observedTStatistic", "observedCohensD",
  "observedSuccesses", "observedFailures", "observedTrials",
  "observedVariable", "observedFirstVariable", "observedSecondVariable",
  "observedDependentVariable", "observedGroupingVariable",
  "observedProportionVariable", "observedSuccessValue"
)

.evOption <- function(options, name, default = NULL) {
  value <- options[[name]]
  if (is.null(value))
    return(default)

  return(value)
}

.evThreshold <- function(settings, target) {
  if (target == "h1")
    return(settings[["bf10Threshold"]])

  return(settings[["bf01Threshold"]])
}

.evEventK <- function(settings, target) {
  if (target == "h1")
    return(1 / settings[["bf10Threshold"]])

  return(settings[["bf01Threshold"]])
}

.evLowerTail <- function(target) {
  return(target == "h1")
}

.evPrepareSettings <- function(options) {
  test <- options[["test"]]
  legacyThreshold <- .evOption(options, "bfThreshold", 10)

  settings <- list(
    test                 = test,
    testLabel            = .evTestLabel(test),
    testType             = .evTestType(test),
    isIndependentSamples = grepl("independentSamples", test, fixed = TRUE),
    isTTest              = grepl("TTest", test, fixed = TRUE),
    isGeneralZ           = identical(test, "generalZApproximation"),
    isZTest              = grepl("ZTest", test, fixed = TRUE) || identical(test, "generalZApproximation"),
    isBinomial           = identical(test, "oneSampleProportion"),
    calculation          = options[["calculation"]],
    evidenceTarget       = .evOption(options, "evidenceTarget", "h1"),
    bf10Threshold        = .evOption(options, "bf10Threshold", legacyThreshold),
    bf01Threshold        = .evOption(options, "bf01Threshold", legacyThreshold),
    targetPowerH1        = .evOption(options, "targetPowerH1", .evOption(options, "evidenceProbability", 0.9)),
    targetPowerH0        = .evOption(options, "targetPowerH0", .evOption(options, "evidenceProbability", 0.9)),
    planningTargets      = .evPlanningTargets(options, .evOption(options, "evidenceTarget", NULL)),
    sampleSize           = options[["sampleSize"]],
    sampleSizeRatio      = options[["sampleSizeRatio"]],
    rangeMin             = options[["sampleSizeRangeMin"]],
    rangeMax             = options[["sampleSizeRangeMax"]],
    plotPoints           = options[["plotPoints"]],
    logSampleSize        = .evOption(options, "logSampleSize", TRUE),
    legendPosition       = .evOption(options, "legendPosition", "right"),
    colorPalette         = .evOption(options, "colorPalette", "colorblind"),
    mergeH1H0Figures     = .evOption(options, "mergeH1H0Figures", FALSE),
    priorPlotDesign      = .evOption(options, "priorDistributionDesign", TRUE),
    priorPlotAnalysis    = .evOption(options, "priorDistributionAnalysis", TRUE),
    priorPlotMerge       = .evOption(options, "priorDistributionMerge", FALSE),
    reportLatex          = .evOption(options, "generateReportLatex", FALSE)
  )

  settings[["bfThreshold"]] <- .evThreshold(settings, settings[["evidenceTarget"]])
  settings[["eventK"]]      <- .evEventK(settings, settings[["evidenceTarget"]])
  settings[["lowerTail"]]   <- .evLowerTail(settings[["evidenceTarget"]])

  if (settings[["isBinomial"]]) {
    settings <- .evAddBinomialSettings(settings, options)
  } else {
    settings <- .evAddContinuousSettings(settings, options)
  }

  return(settings)
}

.evPlanningTargets <- function(options, legacyTarget = NULL) {
  hasPowerTargets <- !is.null(options[["targetPowerH1"]]) || !is.null(options[["targetPowerH0"]])
  if (!hasPowerTargets && !is.null(legacyTarget))
    return(legacyTarget)

  return(c("h1", "h0"))
}

.evResultTargets <- function(settings) {
  if (identical(settings[["calculation"]], "sampleSize"))
    return(settings[["planningTargets"]])

  return(c("h1", "h0"))
}

.evPlanningTargetText <- function(settings) {
  labels <- vapply(settings[["planningTargets"]], .evTargetLabel, character(1))
  return(paste(labels, collapse = ", "))
}

.evTargetPower <- function(settings, target) {
  if (target == "h0")
    return(settings[["targetPowerH0"]])

  return(settings[["targetPowerH1"]])
}

.evValidateTargetPowers <- function(settings) {
  for (target in c("h1", "h0")) {
    power <- .evTargetPower(settings, target)
    if (!is.finite(power) || power <= 0 || power >= 1)
      stop(gettext("Conclusive evidence targets must be between 0 and 1."))
  }
}

.evAddContinuousSettings <- function(settings, options) {
  settings[["nullValue"]]         <- options[["nullValue"]]
  settings[["standardDeviation"]] <- options[["standardDeviation"]]
  settings[["generalZParameterization"]] <- options[["generalZParameterization"]]
  settings[["unitInformationSd"]]        <- options[["unitInformationSd"]]
  settings[["alternative"]]       <- switch(options[["alternative"]], twoSided = "two.sided", options[["alternative"]])
  settings[["n1"]]                <- options[["sampleSize"]]
  settings[["n2"]]                <- if (settings[["isIndependentSamples"]]) ceiling(options[["sampleSize"]] * options[["sampleSizeRatio"]]) else options[["sampleSize"]]

  if (settings[["isZTest"]]) {
    settings[["analysisPriorIsCauchy"]]     <- FALSE
    analysisPriorDistribution               <- .evOption(options, "analysisPriorDistribution", "normal")
    if (length(analysisPriorDistribution) != 1 || is.na(analysisPriorDistribution) || analysisPriorDistribution == "")
      analysisPriorDistribution <- "normal"
    settings[["analysisPriorDistribution"]] <- analysisPriorDistribution
    settings[["analysisPriorMean"]]         <- if (analysisPriorDistribution == "point") .evOption(options, "analysisPriorPoint", 0) else .evOption(options, "analysisPriorMean", 0)
    settings[["analysisPriorSd"]]           <- if (analysisPriorDistribution == "point") 0 else .evOption(options, "analysisPriorSd", 1)
    settings[["momentPriorSpread"]]         <- if (analysisPriorDistribution == "normalMomentMode") {
      .evOption(options, "momentPriorMode", sqrt(2)) / sqrt(2)
    } else {
      .evOption(options, "momentPriorSpread", 1)
    }
    settings[["momentPriorMode"]] <- sqrt(2) * settings[["momentPriorSpread"]]
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
    settings[["drangeMode"]]                <- options[["drangeMode"]]
    settings[["drangeLower"]]               <- options[["drangeLower"]]
    settings[["drangeUpper"]]               <- options[["drangeUpper"]]
  }

  settings <- .evAddContinuousDesignPriors(settings, options)

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

  settings[["binomialDesignPrior"]]  <- options[["binomialDesignPrior"]]
  settings[["designProportion"]]     <- options[["designProportion"]]
  settings[["designPriorSuccesses"]] <- options[["designPriorSuccesses"]]
  settings[["designPriorFailures"]]  <- options[["designPriorFailures"]]
  settings[["designPriorLower"]]     <- options[["designPriorLower"]]
  settings[["designPriorUpper"]]     <- options[["designPriorUpper"]]

  settings <- .evAddBinomialDesignPriors(settings, options)

  return(settings)
}

.evAddContinuousDesignPriors <- function(settings, options) {
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

  return(settings)
}

.evContinuousDesignParameters <- function(designPrior) {
  if (designPrior[["distribution"]] == "point")
    return(gettextf("location = %1$s", .evFormatNumber(designPrior[["mean"]])))

  return(gettextf(
    "mean = %1$s, sd = %2$s",
    .evFormatNumber(designPrior[["mean"]]),
    .evFormatNumber(designPrior[["sd"]])
  ))
}

.evAddBinomialDesignPriors <- function(settings, options) {
  designH0 <- .evExplicitBinomialDesignPrior(settings, options, "h0")
  designH1 <- .evExplicitBinomialDesignPrior(settings, options, "h1")

  settings[["designPriorUnderH0"]] <- designH0
  settings[["designPriorUnderH1"]] <- designH1

  return(settings)
}

.evExplicitBinomialDesignPrior <- function(settings, options, under) {
  if (under == "h0") {
    distribution <- .evOption(options, "binomialDesignNullPrior", "point")
    if (distribution == "point")
      return(.evBinomialPointDesignPrior(.evOption(options, "designNullProportion", settings[["nullProportion"]])))

    return(.evBinomialBetaDesignPrior(
      a     = .evOption(options, "designNullPriorSuccesses", 1),
      b     = .evOption(options, "designNullPriorFailures", 1),
      lower = .evOption(options, "designNullPriorLower", 0),
      upper = .evOption(options, "designNullPriorUpper", settings[["nullProportion"]])
    ))
  }

  if (settings[["binomialDesignPrior"]] == "point")
    return(.evBinomialPointDesignPrior(settings[["designProportion"]]))

  return(.evBinomialBetaDesignPrior(
    a     = settings[["designPriorSuccesses"]],
    b     = settings[["designPriorFailures"]],
    lower = settings[["designPriorLower"]],
    upper = settings[["designPriorUpper"]]
  ))
}

.evBinomialPointDesignPrior <- function(proportion, label = gettext("Point proportion"), parameters = NULL) {
  if (is.null(parameters))
    parameters <- gettextf("p = %1$s", .evFormatNumber(proportion))

  return(list(
    distribution = "point",
    proportion   = proportion,
    label        = label,
    parameters   = parameters
  ))
}

.evBinomialBetaDesignPrior <- function(a, b, lower, upper, label = gettext("Beta"), parameters = NULL) {
  if (is.null(parameters)) {
    parameters <- gettextf(
      "a = %1$s, b = %2$s, lower = %3$s, upper = %4$s",
      .evFormatNumber(a),
      .evFormatNumber(b),
      .evFormatNumber(lower),
      .evFormatNumber(upper)
    )
  }

  return(list(
    distribution = "beta",
    a            = a,
    b            = b,
    lower        = lower,
    upper        = upper,
    label        = label,
    parameters   = parameters
  ))
}

.evComputeResult <- function(settings) {
  .evValidateSettings(settings)

  targetResults <- lapply(.evResultTargets(settings), function(target) {
    under <- target
    n1 <- if (settings[["calculation"]] == "sampleSize") {
      .evFindSampleSize(settings, target = target, under = under)
    } else {
      settings[["sampleSize"]]
    }

    .evValidateSampleSize(settings, n1)

    data.frame(
      under       = under,
      target      = target,
      n1          = n1,
      n2          = .evSampleSizeSecondGroup(settings, n1),
      probability = .evEvidenceProbability(settings, n1 = n1, target = target, under = under),
      stringsAsFactors = FALSE
    )
  })

  targetResults <- do.call(rbind, targetResults)
  n2Values <- targetResults[["n2"]]
  probabilityValues <- targetResults[["probability"]]

  return(list(
    n1            = max(targetResults[["n1"]], na.rm = TRUE),
    n2            = if (all(is.na(n2Values))) NA_integer_ else max(n2Values, na.rm = TRUE),
    probability   = if (all(is.na(probabilityValues))) NA_real_ else min(probabilityValues, na.rm = TRUE),
    targetResults = targetResults
  ))
}

.evEvidenceProbability <- function(settings, n1 = settings[["n1"]], target = settings[["evidenceTarget"]],
                                   under = "h1", designPriorMean = NULL, designPriorSd = NULL) {
  k         <- .evEventK(settings, target)
  lowerTail <- .evLowerTail(target)

  if (settings[["isBinomial"]]) {
    designArguments <- if (is.null(designPriorMean)) .evBinomialDesignArguments(settings, under = under) else list(dp = designPriorMean)
    return(.evEvidenceProbabilityBinomial(settings, n1, k, lowerTail, designArguments))
  }

  if (is.null(designPriorMean)) {
    designPrior     <- .evContinuousDesignPriorForUnder(settings, under)
    designPriorMean <- designPrior[["mean"]]
    designPriorSd   <- designPrior[["sd"]]
  } else if (is.null(designPriorSd)) {
    designPriorSd <- .evContinuousDesignPriorForUnder(settings, under)[["sd"]]
  }

  if (settings[["isZTest"]])
    return(.evEvidenceProbabilityZ(settings, n1, k, lowerTail, designPriorMean, designPriorSd))

  return(.evEvidenceProbabilityT(settings, n1, k, lowerTail, designPriorMean, designPriorSd))
}

.evContinuousDesignPriorForUnder <- function(settings, under) {
  if (under == "h0")
    return(settings[["designPriorUnderH0"]])

  return(settings[["designPriorUnderH1"]])
}

.evEvidenceProbabilityZ <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  unitSd <- .evZUnitStandardDeviation(settings, n1)

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

.evEvidenceProbabilityT <- function(settings, n1, k, lowerTail, designPriorMean, designPriorSd) {
  n2 <- .evSampleSizeSecondGroup(settings, n1)

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
    drange      = .evTSearchRange(settings, n1, k)
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

.evFindSampleSize <- function(settings, target, under) {
  minimumN <- .evMinimumSampleSize(settings)
  maximumN <- ceiling(settings[["rangeMax"]])

  if (minimumN >= maximumN)
    stop(gettext("The minimum sample size must be smaller than the maximum sample size."))

  packageN <- try(.evFindSampleSizeWithBfpwr(settings, minimumN, maximumN, target = target, under = under), silent = TRUE)

  if (!jaspBase::isTryError(packageN) && length(packageN) == 1 && is.finite(packageN)) {
    n <- ceiling(packageN)
  } else {
    n <- .evFindSampleSizeBySearch(settings, minimumN, maximumN, target = target, under = under)
  }

  n <- max(minimumN, min(maximumN, n))
  n <- .evAdjustSampleSize(settings, n, minimumN, maximumN, target = target, under = under)

  return(n)
}

.evFindSampleSizeWithBfpwr <- function(settings, minimumN, maximumN, target, under) {
  nrange <- c(minimumN, maximumN)
  k      <- .evEventK(settings, target)
  lowerTail <- .evLowerTail(target)

  if (settings[["isBinomial"]]) {
    return(do.call(
      what = bfpwr::nbinbf01,
      args = c(
        list(
          k          = k,
          power      = .evTargetPower(settings, target),
          p0         = settings[["nullProportion"]],
          type       = settings[["nullPriorDistribution"]],
          a          = settings[["analysisPriorSuccesses"]],
          b          = settings[["analysisPriorFailures"]],
          lower.tail = lowerTail,
          nrange     = nrange
        ),
        .evBinomialDesignArguments(settings, under = under)
      )
    ))
  }

  designPrior <- .evContinuousDesignPriorForUnder(settings, under)

  if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("point", "normal")) {
    return(bfpwr::nbf01(
      k          = k,
      power      = .evTargetPower(settings, target),
      usd        = .evZUnitStandardDeviation(settings, minimumN),
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
      power      = .evTargetPower(settings, target),
      usd        = .evZUnitStandardDeviation(settings, minimumN),
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
    power       = .evTargetPower(settings, target),
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

.evFindSampleSizeBySearch <- function(settings, minimumN, maximumN, target, under) {
  targetPower <- .evTargetPower(settings, target)
  lowerProbability <- .evEvidenceProbability(settings, n1 = minimumN, target = target, under = under)
  if (is.finite(lowerProbability) && lowerProbability >= targetPower)
    return(minimumN)

  upperProbability <- .evEvidenceProbability(settings, n1 = maximumN, target = target, under = under)
  if (!is.finite(upperProbability) || upperProbability < targetPower)
    stop(gettext("Target conclusive evidence probability is not reached within the selected sample-size range."))

  lower <- minimumN
  upper <- maximumN

  while ((upper - lower) > 1) {
    midpoint            <- floor((lower + upper) / 2)
    midpointProbability <- .evEvidenceProbability(settings, n1 = midpoint, target = target, under = under)

    if (is.finite(midpointProbability) && midpointProbability >= targetPower) {
      upper <- midpoint
    } else {
      lower <- midpoint
    }
  }

  return(upper)
}

.evAdjustSampleSize <- function(settings, n, minimumN, maximumN, target, under) {
  targetPower <- .evTargetPower(settings, target)
  while (n <= maximumN && .evEvidenceProbability(settings, n1 = n, target = target, under = under) < targetPower)
    n <- n + 1

  if (n > maximumN)
    stop(gettext("Target conclusive evidence probability is not reached within the selected sample-size range."))

  while (n > minimumN && .evEvidenceProbability(settings, n1 = n - 1, target = target, under = under) >= targetPower)
    n <- n - 1

  return(n)
}

.evResultsTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceResults"]]))
    return()

  table <- createJaspTable(title = gettext("Bayes Factor Design"))
  table$dependOn(.evDependencies)
  table$position <- 1
  jaspResults[["evidenceResults"]] <- table

  .evAddResultsTableColumns(table, settings)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute Bayes factor design: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evResultsRows(settings, result))

  generalZUisdFootnote <- .evGeneralZKnownUisdFootnote(settings)
  if (!is.null(generalZUisdFootnote))
    table$addFootnote(generalZUisdFootnote)

  if (settings[["calculation"]] == "sampleSize")
    table$addFootnote(gettext("Due to rounding of the sample size, Pr(Conclusive Evidence) can deviate from the target probability."))

  if (settings[["isIndependentSamples"]])
    table$addFootnote(gettext("For independent samples, N\u2081 is the sample size in group 1 and N\u2082 is rounded up from the requested N\u2082/N\u2081 sample-size ratio."))
}

.evAddResultsTableColumns <- function(table, settings) {
  computed <- gettext("Computed")
  userDefined <- gettext("User Defined")

  table$addColumnInfo(name = "under", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "target", title = gettext("Planned Target"), type = "string")

  if (settings[["calculation"]] == "sampleSize") {
    if (settings[["isIndependentSamples"]]) {
      table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer", overtitle = computed)
      table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer", overtitle = computed)
    } else {
      table$addColumnInfo(name = "n", title = "N", type = "integer", overtitle = computed)
    }
    table$addColumnInfo(name = "probability", title = gettext("Pr(Conclusive Evidence)"), type = "number", overtitle = computed)
    table$addColumnInfo(name = "targetProbability", title = gettext("Target Pr(Conclusive Evidence)"), type = "number", overtitle = userDefined)
  } else {
    table$addColumnInfo(name = "probability", title = gettext("Pr(Conclusive Evidence)"), type = "number", overtitle = computed)
    if (settings[["isIndependentSamples"]]) {
      table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer", overtitle = userDefined)
      table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer", overtitle = userDefined)
    } else {
      table$addColumnInfo(name = "n", title = "N", type = "integer", overtitle = userDefined)
    }
  }

  table$addColumnInfo(name = "threshold", title = gettext("Bayes factor threshold"), type = "number", overtitle = userDefined)
}

.evResultsRows <- function(settings, result) {
  rows <- result[["targetResults"]]
  out <- data.frame(
    under             = vapply(rows[["under"]], .evUnderLabel, character(1)),
    target            = vapply(rows[["target"]], .evTargetLabel, character(1)),
    probability       = rows[["probability"]],
    targetProbability = vapply(rows[["target"]], function(target) .evTargetPower(settings, target), numeric(1)),
    threshold         = vapply(rows[["target"]], function(target) .evThreshold(settings, target), numeric(1)),
    stringsAsFactors  = FALSE
  )

  if (settings[["isIndependentSamples"]]) {
    out[["n1"]] <- rows[["n1"]]
    out[["n2"]] <- rows[["n2"]]
  } else {
    out[["n"]] <- rows[["n1"]]
  }

  if (settings[["calculation"]] != "sampleSize")
    out[["targetProbability"]] <- NULL

  return(out)
}

.evDesignOutcomeTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceDesignOutcome"]]))
    return()

  table <- createJaspTable(title = gettext("Design Evidence"))
  table$dependOn(.evDependencies)
  table$position <- 2
  jaspResults[["evidenceDesignOutcome"]] <- table

  table$addColumnInfo(name = "under",       title = gettext("Under"),       type = "string")
  table$addColumnInfo(name = "null",        title = gettext("Null"),         type = "number", overtitle = gettext("Bayes Factor Evidence"))
  table$addColumnInfo(name = "undecided",   title = gettext("Inconclusive"), type = "number", overtitle = gettext("Bayes Factor Evidence"))
  table$addColumnInfo(name = "alternative", title = gettext("Alternative"),  type = "number", overtitle = gettext("Bayes Factor Evidence"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute design evidence: %1$s", .evCleanError(result)))
    return()
  }

  table$setData(.evDesignOutcomeRows(settings, result))
  table$addFootnote(gettext("Probabilities are evaluated at the sample sizes in the Bayes Factor Design table. Rows use the corresponding design prior under H\u2081 or H\u2080."))
}

.evDesignOutcomeRows <- function(settings, result) {
  h1N <- .evResultN1ForUnder(result, "h1")
  h0N <- .evResultN1ForUnder(result, "h0")
  h1Outcome <- .evDesignOutcomeProbabilities(settings, h1N, under = "h1")
  h0Outcome <- .evDesignOutcomeProbabilities(settings, h0N, under = "h0")

  out <- data.frame(
    under       = c(gettext("H\u2081"), gettext("H\u2080")),
    null        = c(h1Outcome[["null"]],        h0Outcome[["null"]]),
    undecided   = c(h1Outcome[["undecided"]],   h0Outcome[["undecided"]]),
    alternative = c(h1Outcome[["alternative"]], h0Outcome[["alternative"]]),
    stringsAsFactors = FALSE
  )

  return(out)
}

.evResultN1ForUnder <- function(result, under) {
  rows <- result[["targetResults"]]
  index <- which(rows[["under"]] == under)
  if (length(index) == 0)
    return(result[["n1"]])

  return(rows[["n1"]][index[1]])
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
  return(.evEvidenceProbability(settings, n1 = n1, target = target, under = under))
}

.evPriorsTable <- function(jaspResults, settings, validation) {
  if (!is.null(jaspResults[["evidencePriors"]]))
    return()

  table <- createJaspTable(title = gettext("Design Specification"))
  table$dependOn(.evDependencies)
  table$position <- 3
  jaspResults[["evidencePriors"]] <- table

  table$addColumnInfo(name = "hypothesis", title = gettext("Hypothesis"), type = "string")
  table$addColumnInfo(name = "designPrior", title = gettext("Design Prior"), type = "string")
  table$addColumnInfo(name = "analysisPrior", title = gettext("Analysis Prior"), type = "string")

  if (jaspBase::isTryError(validation)) {
    table$setError(gettextf("Unable to describe priors: %1$s", .evCleanError(validation)))
    return()
  }

  table$setData(data.frame(
    hypothesis    = c(gettext("H\u2081"), gettext("H\u2080")),
    designPrior   = c(.evDesignPriorString(settings, "h1"), .evDesignPriorString(settings, "h0")),
    analysisPrior = c(.evAnalysisPriorString(settings), .evNullPriorString(settings)),
    stringsAsFactors = FALSE
  ))
}

.evObservedAnalysisTable <- function(jaspResults, dataset, options, settings, key, position, sequential = FALSE,
                                     dependencies = .evDependencies) {
  if (!is.null(jaspResults[[key]]))
    return()

  container <- createJaspContainer(title = gettext("Analysis"))
  container$dependOn(c(dependencies, .evObservedDependencies))
  container$position <- position
  jaspResults[[key]] <- container

  resultTable <- createJaspTable(title = gettext("Bayes Factor"))
  resultTable$position <- 1
  resultTable$showSpecifiedColumnsOnly <- TRUE
  container[["result"]] <- resultTable
  .evObservedAddResultColumns(resultTable, settings)

  summaryTable <- createJaspTable(title = gettext("Summary Statistics"))
  summaryTable$position <- 2
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  container[["summaryStatistics"]] <- summaryTable

  summary <- try(.evObservedSummary(dataset, options, settings), silent = TRUE)
  if (jaspBase::isTryError(summary)) {
    message <- gettextf("Unable to summarize observed data: %1$s", .evCleanError(summary))
    resultTable$setError(message)
    summaryTable$setError(message)
    return()
  }

  .evObservedFillSummaryTable(summaryTable, settings, summary)

  bayesFactor <- try(.evObservedBayesFactor(settings, summary), silent = TRUE)
  if (jaspBase::isTryError(bayesFactor)) {
    resultTable$setError(gettextf("Unable to compute observed Bayes factor: %1$s", .evCleanError(bayesFactor)))
    return()
  }

  resultTable$setData(.evObservedResultRow(settings, summary, bayesFactor, sequential))

  if (settings[["isTTest"]])
    resultTable$addFootnote(gettext("For t-tests, the estimate is the standardized mean difference on the analysis scale."))

  if (isTRUE(sequential))
    resultTable$addFootnote(gettext("Sequential decisions use the configured BF thresholds for the current observed data."))
}

.evObservedAddResultColumns <- function(table, settings) {
  table$addColumnInfo(name = "test", title = gettext("Statistical Test"), type = "string")
  table$addColumnInfo(
    name  = "statistic",
    title = .evObservedStatisticTitle(settings),
    type  = if (settings[["isBinomial"]]) "integer" else "number"
  )
  table$addColumnInfo(name = "bf10", title = "BF\u2081\u2080", type = "number")
  table$addColumnInfo(name = "decision", title = gettext("Decision"), type = "string")
}

.evObservedResultRow <- function(settings, summary, bayesFactor, sequential) {
  return(data.frame(
    test      = .evObservedTestLabel(settings),
    statistic = .evObservedStatisticValue(settings, summary),
    bf10      = bayesFactor[["bf10"]],
    decision  = .evObservedDecision(settings, bayesFactor, sequential),
    stringsAsFactors = FALSE
  ))
}

.evObservedTestLabel <- function(settings) {
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

.evObservedStatisticValue <- function(settings, summary) {
  if (settings[["isBinomial"]])
    return(summary[["successes"]])

  return(.evObservedValue(summary, "testStatistic"))
}

.evObservedFillSummaryTable <- function(table, settings, summary) {
  .evObservedAddSummaryColumns(table, settings, summary)
  table$setData(.evObservedSummaryRows(settings, summary))
}

.evObservedAddSummaryColumns <- function(table, settings, summary) {
  if (settings[["isIndependentSamples"]]) {
    table$addColumnInfo(name = "group", title = gettext("Group"), type = "string")
    table$addColumnInfo(name = "n", title = "N", type = "integer")
    if (.evObservedSummaryHasAny(summary, c("mean1", "mean2")))
      table$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    if (.evObservedSummaryHasAny(summary, c("sd1", "sd2")))
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

  if (.evObservedSummaryHasAny(summary, "n"))
    table$addColumnInfo(name = "n", title = "N", type = "integer")

  if (settings[["testType"]] == "paired") {
    if (.evObservedSummaryHasAny(summary, "meanDifference"))
      table$addColumnInfo(name = "meanDifference", title = gettext("Mean Difference"), type = "number")
    if (.evObservedSummaryHasAny(summary, "sdDifference"))
      table$addColumnInfo(name = "sdDifference", title = gettext("SD Difference"), type = "number")
  } else {
    if (.evObservedSummaryHasAny(summary, "mean"))
      table$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    if (.evObservedSummaryHasAny(summary, "sd"))
      table$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
  }

  table$addColumnInfo(name = "estimate", title = .evObservedEstimateTitle(settings), type = "number")
  table$addColumnInfo(name = "standardError", title = gettext("SE"), type = "number")
}

.evObservedSummaryRows <- function(settings, summary) {
  if (settings[["isBinomial"]])
    return(.evObservedBinomialSummaryRows(summary))

  if (settings[["isIndependentSamples"]])
    return(.evObservedIndependentSummaryRows(summary))

  if (settings[["testType"]] == "paired")
    return(.evObservedPairedSummaryRows(summary))

  return(.evObservedOneSampleSummaryRows(settings, summary))
}

.evObservedBinomialSummaryRows <- function(summary) {
  return(data.frame(
    variable   = summary[["variable"]],
    successes  = summary[["successes"]],
    trials     = summary[["trials"]],
    proportion = summary[["proportion"]],
    stringsAsFactors = FALSE
  ))
}

.evObservedIndependentSummaryRows <- function(summary) {
  groups <- c(.evObservedValue(summary, "group1"), .evObservedValue(summary, "group2"))
  groups[is.na(groups)] <- c(gettext("Group 1"), gettext("Group 2"))[is.na(groups)]

  rows <- data.frame(
    group = groups,
    n     = c(summary[["n1"]], summary[["n2"]]),
    stringsAsFactors = FALSE
  )

  if (.evObservedSummaryHasAny(summary, c("mean1", "mean2")))
    rows[["mean"]] <- c(.evObservedValue(summary, "mean1"), .evObservedValue(summary, "mean2"))

  if (.evObservedSummaryHasAny(summary, c("sd1", "sd2")))
    rows[["sd"]] <- c(.evObservedValue(summary, "sd1"), .evObservedValue(summary, "sd2"))

  return(rows)
}

.evObservedPairedSummaryRows <- function(summary) {
  row <- data.frame(variable = summary[["variable"]], stringsAsFactors = FALSE)

  if (.evObservedSummaryHasAny(summary, "n"))
    row[["n"]] <- summary[["n"]]

  if (.evObservedSummaryHasAny(summary, "meanDifference"))
    row[["meanDifference"]] <- summary[["meanDifference"]]

  if (.evObservedSummaryHasAny(summary, "sdDifference"))
    row[["sdDifference"]] <- summary[["sdDifference"]]

  row[["estimate"]]      <- summary[["estimate"]]
  row[["standardError"]] <- summary[["standardError"]]
  return(row)
}

.evObservedOneSampleSummaryRows <- function(settings, summary) {
  row <- data.frame(variable = summary[["variable"]], stringsAsFactors = FALSE)

  if (.evObservedSummaryHasAny(summary, "n"))
    row[["n"]] <- summary[["n"]]

  if (.evObservedSummaryHasAny(summary, "mean"))
    row[["mean"]] <- .evObservedValue(summary, "mean")
  if (.evObservedSummaryHasAny(summary, "sd"))
    row[["sd"]] <- summary[["sd"]]

  row[["estimate"]]      <- summary[["estimate"]]
  row[["standardError"]] <- summary[["standardError"]]
  return(row)
}

.evObservedSummaryHasAny <- function(summary, names) {
  values <- vapply(names, function(name) {
    value <- .evObservedValue(summary, name)
    length(value) == 1 && !is.na(value)
  }, logical(1))

  return(any(values))
}

.evObservedSummary <- function(dataset, options, settings) {
  input <- .evOption(options, "observedAnalysisInput", "summaryStatistics")
  if (input == "columns")
    return(.evObservedSummaryFromData(dataset, options, settings))

  return(.evObservedSummaryFromOptions(options, settings))
}

.evObservedAnalysisReady <- function(dataset, options, settings) {
  input <- .evOption(options, "observedAnalysisInput", "summaryStatistics")
  if (input == "columns")
    return(.evObservedColumnInputStarted(options, settings))

  return(.evObservedSummaryInputStarted(options, settings))
}

.evObservedSummaryInputStarted <- function(options, settings) {
  if (settings[["isBinomial"]]) {
    trials    <- .evObservedOptionNumber(options, "observedTrials")
    failures  <- .evObservedOptionNumber(options, "observedFailures")
    successes <- .evObservedOptionNumber(options, "observedSuccesses")
    return(trials > 0 || failures > 0 || successes > 0)
  }

  if (settings[["isGeneralZ"]])
    return(.evObservedOptionNumber(options, "observedStandardError") > 0)

  if (settings[["isTTest"]])
    return(.evObservedTSampleSizeStarted(options, settings))

  if (settings[["isIndependentSamples"]]) {
    n1 <- .evObservedOptionNumber(options, "observedN1")
    n2 <- .evObservedOptionNumber(options, "observedN2")
    return(n1 > 0 || n2 > 0)
  }

  return(.evObservedOptionNumber(options, "observedN") > 0)
}

.evObservedTSampleSizeStarted <- function(options, settings) {
  if (settings[["isIndependentSamples"]]) {
    n1 <- .evObservedOptionNumber(options, "observedN1")
    n2 <- .evObservedOptionNumber(options, "observedN2")
    return(n1 > 0 || n2 > 0)
  }

  return(.evObservedOptionNumber(options, "observedN") > 0)
}

.evObservedColumnInputStarted <- function(options, settings) {
  if (settings[["isBinomial"]])
    return(.evObservedHasOption(options, "observedProportionVariable"))

  if (settings[["isIndependentSamples"]]) {
    return(any(vapply(c("observedDependentVariable", "observedGroupingVariable"), function(name) {
      .evObservedHasOption(options, name)
    }, logical(1))))
  }

  if (settings[["testType"]] == "paired") {
    return(any(vapply(c("observedFirstVariable", "observedSecondVariable"), function(name) {
      .evObservedHasOption(options, name)
    }, logical(1))))
  }

  return(.evObservedHasOption(options, "observedVariable"))
}

.evObservedOptionNumber <- function(options, name) {
  value <- .evOption(options, name, 0)
  if (length(value) != 1 || !is.numeric(value) || !is.finite(value))
    return(0)

  return(value)
}

.evObservedHasOption <- function(options, name) {
  value <- .evOption(options, name, "")
  if (length(value) < 1)
    return(FALSE)

  value <- value[[1]]
  return(length(value) == 1 && !is.na(value) && nzchar(value))
}

.evObservedSummaryFromOptions <- function(options, settings) {
  source <- gettext("Summary statistics")

  if (settings[["isTTest"]])
    return(.evObservedTSummaryFromOptions(options, settings, source))

  if (settings[["isBinomial"]]) {
    counts <- .evObservedBinomialCounts(options)

    return(list(
      source     = source,
      variable   = gettext("Summary statistics"),
      successes  = counts[["successes"]],
      trials     = counts[["trials"]],
      proportion = counts[["successes"]] / counts[["trials"]]
    ))
  }

  if (settings[["isGeneralZ"]]) {
    estimate <- .evObservedNumber(options, "observedEstimate", gettext("Estimate"))
    se       <- .evObservedPositive(options, "observedStandardError", gettext("SE"))
    summary  <- .evObservedZSummary(source, gettext("Summary statistics"), estimate, se)
    summary[["testStatistic"]] <- (estimate - settings[["nullValue"]]) / se
    return(summary)
  }

  if (settings[["isIndependentSamples"]]) {
    n1    <- .evObservedInteger(options, "observedN1", "N1", minimum = if (settings[["isTTest"]]) 2 else 1)
    n2    <- .evObservedInteger(options, "observedN2", "N2", minimum = if (settings[["isTTest"]]) 2 else 1)
    mean1 <- .evObservedNumber(options, "observedMean1", gettext("Mean 1"))
    mean2 <- .evObservedNumber(options, "observedMean2", gettext("Mean 2"))

    if (settings[["isTTest"]]) {
      sd1 <- .evObservedPositive(options, "observedSd1", gettext("SD 1"))
      sd2 <- .evObservedPositive(options, "observedSd2", gettext("SD 2"))
      return(.evObservedIndependentTSummary(source, gettext("Summary statistics"), n1, n2, mean1, mean2, sd1, sd2, settings))
    }

    return(.evObservedIndependentZSummary(source, gettext("Summary statistics"), n1, n2, mean1, mean2, settings))
  }

  n <- .evObservedInteger(options, "observedN", "N", minimum = if (settings[["isTTest"]]) 2 else 1)
  if (settings[["testType"]] == "paired") {
    meanDifference <- .evObservedNumber(options, "observedMeanDifference", gettext("Mean difference"))
    if (settings[["isTTest"]]) {
      sdDifference <- .evObservedPositive(options, "observedSdDifference", gettext("SD difference"))
      return(.evObservedPairedTSummary(source, gettext("Summary statistics"), n, meanDifference, sdDifference, settings))
    }

    return(.evObservedPairedZSummary(source, gettext("Summary statistics"), n, meanDifference, settings))
  }

  mean <- .evObservedNumber(options, "observedMean", gettext("Mean"))
  if (settings[["isTTest"]]) {
    sd <- .evObservedPositive(options, "observedSd", gettext("SD"))
    return(.evObservedOneSampleTSummary(source, gettext("Summary statistics"), n, mean, sd, settings))
  }

  return(.evObservedOneSampleZSummary(source, gettext("Summary statistics"), n, mean, settings))
}

.evObservedTSummaryFromOptions <- function(options, settings, source) {
  inputType <- .evObservedTInputType(options, settings)

  if (settings[["isIndependentSamples"]])
    return(.evObservedIndependentTSummaryFromOptions(options, settings, source, inputType))

  return(.evObservedDependentTSummaryFromOptions(options, settings, source, inputType))
}

.evObservedTInputType <- function(options, settings) {
  inputType <- .evOption(options, "observedInputType", NULL)
  if (is.null(inputType))
    return(.evObservedLegacyTInputType(settings))

  valid <- if (settings[["isIndependentSamples"]]) {
    c("tAndN", "cohensD", "meansAndSDs")
  } else if (settings[["testType"]] == "paired") {
    c("tAndN", "cohensD", "meanDiffAndSD")
  } else {
    c("tAndN", "cohensD", "meanAndSD")
  }

  if (length(inputType) != 1 || is.na(inputType) || !inputType %in% valid)
    return("tAndN")

  return(inputType)
}

.evObservedLegacyTInputType <- function(settings) {
  if (settings[["isIndependentSamples"]])
    return("meansAndSDs")

  if (settings[["testType"]] == "paired")
    return("meanDiffAndSD")

  return("meanAndSD")
}

.evObservedIndependentTSummaryFromOptions <- function(options, settings, source, inputType) {
  n1 <- .evObservedInteger(options, "observedN1", "N1", minimum = 2)
  n2 <- .evObservedInteger(options, "observedN2", "N2", minimum = 2)

  if (inputType == "meansAndSDs") {
    mean1 <- .evObservedNumber(options, "observedMean1", gettext("Mean 1"))
    mean2 <- .evObservedNumber(options, "observedMean2", gettext("Mean 2"))
    sd1   <- .evObservedPositive(options, "observedSd1", gettext("SD 1"))
    sd2   <- .evObservedPositive(options, "observedSd2", gettext("SD 2"))
    return(.evObservedIndependentTSummary(source, gettext("Summary statistics"), n1, n2, mean1, mean2, sd1, sd2, settings))
  }

  standardError <- sqrt(1 / n1 + 1 / n2)
  if (inputType == "cohensD") {
    estimate <- .evObservedNumber(options, "observedCohensD", gettext("Cohen's d"))
    t        <- (estimate - settings[["nullValue"]]) / standardError
  } else {
    t        <- .evObservedNumber(options, "observedTStatistic", "t")
    estimate <- settings[["nullValue"]] + t * standardError
  }

  return(.evObservedTStatisticSummary(
    source        = source,
    variable      = gettext("Summary statistics"),
    estimate      = estimate,
    standardError = standardError,
    t             = t,
    n1            = n1,
    n2            = n2
  ))
}

.evObservedDependentTSummaryFromOptions <- function(options, settings, source, inputType) {
  n <- .evObservedInteger(options, "observedN", "N", minimum = 2)

  if (settings[["testType"]] == "paired" && inputType == "meanDiffAndSD") {
    meanDifference <- .evObservedNumber(options, "observedMeanDifference", gettext("Mean difference"))
    sdDifference   <- .evObservedPositive(options, "observedSdDifference", gettext("SD difference"))
    return(.evObservedPairedTSummary(source, gettext("Summary statistics"), n, meanDifference, sdDifference, settings))
  }

  if (settings[["testType"]] != "paired" && inputType == "meanAndSD") {
    mean <- .evObservedNumber(options, "observedMean", gettext("Mean"))
    sd   <- .evObservedPositive(options, "observedSd", gettext("SD"))
    return(.evObservedOneSampleTSummary(source, gettext("Summary statistics"), n, mean, sd, settings))
  }

  standardError <- 1 / sqrt(n)
  if (inputType == "cohensD") {
    estimate <- .evObservedNumber(options, "observedCohensD", gettext("Cohen's d"))
    t        <- (estimate - settings[["nullValue"]]) / standardError
  } else {
    t        <- .evObservedNumber(options, "observedTStatistic", "t")
    estimate <- settings[["nullValue"]] + t * standardError
  }

  return(.evObservedTStatisticSummary(
    source        = source,
    variable      = gettext("Summary statistics"),
    estimate      = estimate,
    standardError = standardError,
    t             = t,
    n             = n
  ))
}

.evObservedBinomialCounts <- function(options) {
  successes <- .evObservedInteger(options, "observedSuccesses", gettext("Successes"), minimum = 0)

  legacyTrials <- .evObservedOptionNumber(options, "observedTrials")
  failures     <- .evObservedOptionNumber(options, "observedFailures")
  useLegacy    <- legacyTrials > 0 && failures == 0 && legacyTrials >= successes

  if (is.null(options[["observedFailures"]]) || useLegacy) {
    trials <- .evObservedInteger(options, "observedTrials", gettext("Trials"), minimum = 1)
    if (successes > trials)
      stop(gettext("The number of successes cannot exceed the number of trials."))

    return(list(successes = successes, trials = trials))
  }

  failures <- .evObservedInteger(options, "observedFailures", gettext("Failures"), minimum = 0)
  trials   <- successes + failures
  if (trials < 1)
    stop(gettext("The total number of successes and failures must be at least 1."))

  return(list(successes = successes, trials = trials))
}

.evObservedTStatisticSummary <- function(source, variable, estimate, standardError, t, n = NA_integer_,
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

.evObservedSummaryFromData <- function(dataset, options, settings) {
  if (is.null(dataset))
    stop(gettext("No dataset is available for column input."))

  source <- gettext("Column data")

  if (settings[["isBinomial"]])
    return(.evObservedBinomialSummaryFromData(dataset, options, source))

  if (settings[["isIndependentSamples"]])
    return(.evObservedIndependentSummaryFromData(dataset, options, settings, source))

  if (settings[["testType"]] == "paired")
    return(.evObservedPairedSummaryFromData(dataset, options, settings, source))

  return(.evObservedOneSampleSummaryFromData(dataset, options, settings, source))
}

.evObservedBinomialSummaryFromData <- function(dataset, options, source) {
  variable <- .evObservedVariableName(options, "observedProportionVariable", gettext("Variable"))
  values   <- .evObservedColumn(dataset, variable, gettext("Variable"))
  values   <- values[!is.na(values)]
  if (length(values) < 1)
    stop(gettext("The selected variable contains no observed values."))

  successValue <- .evOption(options, "observedSuccessValue", "1")
  successes    <- sum(as.character(values) == successValue)
  trials       <- length(values)

  return(list(
    source     = source,
    variable   = .evObservedDecodeVariable(variable),
    successes  = successes,
    trials     = trials,
    proportion = successes / trials
  ))
}

.evObservedIndependentSummaryFromData <- function(dataset, options, settings, source) {
  dependentVariable <- .evObservedVariableName(options, "observedDependentVariable", gettext("Dependent variable"))
  groupingVariable  <- .evObservedVariableName(options, "observedGroupingVariable", gettext("Grouping variable"))
  y                 <- .evObservedColumn(dataset, dependentVariable, gettext("Dependent variable"))
  group             <- .evObservedColumn(dataset, groupingVariable, gettext("Grouping variable"))
  complete          <- stats::complete.cases(y, group)
  y                 <- y[complete]
  group             <- group[complete]

  y <- .evObservedFiniteNumeric(y, gettext("Dependent variable"))
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
    .evObservedDecodeVariable(dependentVariable),
    .evObservedDecodeVariable(groupingVariable)
  )

  if (settings[["isTTest"]]) {
    .evObservedCheckSampleSize(n1, "N1", minimum = 2)
    .evObservedCheckSampleSize(n2, "N2", minimum = 2)
    summary <- .evObservedIndependentTSummary(source, variable, n1, n2, mean1, mean2, stats::sd(values1), stats::sd(values2), settings)
    summary[["group1"]] <- levels[1]
    summary[["group2"]] <- levels[2]
    return(summary)
  }

  .evObservedCheckSampleSize(n1, "N1", minimum = 1)
  .evObservedCheckSampleSize(n2, "N2", minimum = 1)
  summary <- .evObservedIndependentZSummary(source, variable, n1, n2, mean1, mean2, settings, stats::sd(values1), stats::sd(values2))
  summary[["group1"]] <- levels[1]
  summary[["group2"]] <- levels[2]
  return(summary)
}

.evObservedPairedSummaryFromData <- function(dataset, options, settings, source) {
  firstVariable  <- .evObservedVariableName(options, "observedFirstVariable", gettext("First variable"))
  secondVariable <- .evObservedVariableName(options, "observedSecondVariable", gettext("Second variable"))
  first          <- .evObservedColumn(dataset, firstVariable, gettext("First variable"))
  second         <- .evObservedColumn(dataset, secondVariable, gettext("Second variable"))
  complete       <- stats::complete.cases(first, second)
  first          <- .evObservedFiniteNumeric(first[complete], gettext("First variable"))
  second         <- .evObservedFiniteNumeric(second[complete], gettext("Second variable"))
  difference     <- first - second
  n              <- length(difference)
  meanDifference <- mean(difference)
  variable       <- gettextf("%1$s - %2$s", .evObservedDecodeVariable(firstVariable), .evObservedDecodeVariable(secondVariable))

  if (settings[["isTTest"]]) {
    .evObservedCheckSampleSize(n, "N", minimum = 2)
    return(.evObservedPairedTSummary(source, variable, n, meanDifference, stats::sd(difference), settings))
  }

  .evObservedCheckSampleSize(n, "N", minimum = 1)
  return(.evObservedPairedZSummary(source, variable, n, meanDifference, settings, sdDifference = if (n > 1) stats::sd(difference) else NA_real_))
}

.evObservedOneSampleSummaryFromData <- function(dataset, options, settings, source) {
  variable <- .evObservedVariableName(options, "observedVariable", gettext("Variable"))
  values   <- .evObservedFiniteNumeric(.evObservedColumn(dataset, variable, gettext("Variable")), gettext("Variable"))
  n        <- length(values)
  mean     <- mean(values)
  sd       <- if (n > 1) stats::sd(values) else NA_real_

  if (settings[["isGeneralZ"]]) {
    .evObservedCheckSampleSize(n, "N", minimum = 2)
    return(.evObservedGeneralZSummary(source, .evObservedDecodeVariable(variable), n, mean, sd, settings))
  }

  if (settings[["isTTest"]]) {
    .evObservedCheckSampleSize(n, "N", minimum = 2)
    return(.evObservedOneSampleTSummary(source, .evObservedDecodeVariable(variable), n, mean, sd, settings))
  }

  .evObservedCheckSampleSize(n, "N", minimum = 1)
  return(.evObservedOneSampleZSummary(source, .evObservedDecodeVariable(variable), n, mean, settings, sd = sd))
}

.evObservedIndependentTSummary <- function(source, variable, n1, n2, mean1, mean2, sd1, sd2, settings) {
  .evObservedCheckPositive(sd1, gettext("SD 1"))
  .evObservedCheckPositive(sd2, gettext("SD 2"))

  pooledSd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  .evObservedCheckPositive(pooledSd, gettext("Pooled SD"))

  standardError <- sqrt(1 / n1 + 1 / n2)
  estimate      <- (mean1 - mean2) / pooledSd
  t             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n1 = n1, n2 = n2, mean1 = mean1,
    mean2 = mean2, sd1 = sd1, sd2 = sd2, estimate = estimate,
    standardError = standardError, testStatistic = t
  ))
}

.evObservedIndependentZSummary <- function(source, variable, n1, n2, mean1, mean2, settings, sd1 = NA_real_, sd2 = NA_real_) {
  standardError <- settings[["standardDeviation"]] * sqrt(1 / n1 + 1 / n2)
  estimate      <- mean1 - mean2
  z             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n1 = n1, n2 = n2, mean1 = mean1,
    mean2 = mean2, sd1 = sd1, sd2 = sd2, estimate = estimate,
    standardError = standardError, testStatistic = z
  ))
}

.evObservedPairedTSummary <- function(source, variable, n, meanDifference, sdDifference, settings) {
  .evObservedCheckPositive(sdDifference, gettext("SD difference"))
  standardError <- 1 / sqrt(n)
  estimate      <- meanDifference / sdDifference
  t             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, meanDifference = meanDifference,
    sdDifference = sdDifference, estimate = estimate, standardError = standardError,
    testStatistic = t
  ))
}

.evObservedPairedZSummary <- function(source, variable, n, meanDifference, settings, sdDifference = NA_real_) {
  standardError <- settings[["standardDeviation"]] / sqrt(n)
  z             <- (meanDifference - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, meanDifference = meanDifference,
    sdDifference = sdDifference, estimate = meanDifference,
    standardError = standardError, testStatistic = z
  ))
}

.evObservedOneSampleTSummary <- function(source, variable, n, mean, sd, settings) {
  .evObservedCheckPositive(sd, gettext("SD"))
  standardError <- 1 / sqrt(n)
  estimate      <- mean / sd
  t             <- (estimate - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, mean = mean, sd = sd,
    estimate = estimate, standardError = standardError, testStatistic = t
  ))
}

.evObservedOneSampleZSummary <- function(source, variable, n, mean, settings, sd = NA_real_) {
  standardError <- settings[["standardDeviation"]] / sqrt(n)
  z             <- (mean - settings[["nullValue"]]) / standardError

  return(list(
    source = source, variable = variable, n = n, mean = mean, sd = sd,
    estimate = mean, standardError = standardError, testStatistic = z
  ))
}

.evObservedGeneralZSummary <- function(source, variable, n, mean, sd, settings) {
  .evObservedCheckPositive(sd, gettext("SD"))
  standardError <- sd / sqrt(n)
  summary <- .evObservedZSummary(source, variable, mean, standardError)
  summary[["n"]]             <- n
  summary[["mean"]]          <- mean
  summary[["sd"]]            <- sd
  summary[["testStatistic"]] <- (mean - settings[["nullValue"]]) / standardError

  return(summary)
}

.evObservedZSummary <- function(source, variable, estimate, standardError) {
  return(list(
    source = source, variable = variable, n = NA_integer_, mean = NA_real_,
    sd = NA_real_, estimate = estimate, standardError = standardError,
    testStatistic = NA_real_
  ))
}

.evObservedBayesFactor <- function(settings, summary) {
  bf01 <- if (settings[["isBinomial"]]) {
    .evObservedBinomialBayesFactor(settings, summary)
  } else if (settings[["isTTest"]]) {
    .evObservedTBayesFactor(settings, summary)
  } else {
    .evObservedZBayesFactor(settings, summary)
  }

  if (!is.finite(bf01) || bf01 <= 0)
    stop(gettext("Observed Bayes factor is not finite."))

  return(list(bf01 = bf01, bf10 = 1 / bf01))
}

.evObservedZBayesFactor <- function(settings, summary) {
  estimate <- summary[["estimate"]]
  se       <- summary[["standardError"]]

  if (is.na(summary[["testStatistic"]]))
    summary[["testStatistic"]] <- (estimate - settings[["nullValue"]]) / se

  if (isTRUE(.evUsesDirectionalZTest(settings))) {
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

.evObservedTBayesFactor <- function(settings, summary) {
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

.evObservedBinomialBayesFactor <- function(settings, summary) {
  return(bfpwr::binbf01(
    x    = summary[["successes"]],
    n    = summary[["trials"]],
    p0   = settings[["nullProportion"]],
    type = settings[["nullPriorDistribution"]],
    a    = settings[["analysisPriorSuccesses"]],
    b    = settings[["analysisPriorFailures"]]
  ))
}

.evObservedDecision <- function(settings, bayesFactor, sequential) {
  if (bayesFactor[["bf10"]] >= settings[["bf10Threshold"]])
    return(if (isTRUE(sequential)) gettext("Stop for H\u2081") else gettext("BF supports H\u2081"))

  if (bayesFactor[["bf01"]] >= settings[["bf01Threshold"]])
    return(if (isTRUE(sequential)) gettext("Stop for H\u2080") else gettext("BF supports H\u2080"))

  if (isTRUE(sequential))
    return(gettext("Continue"))

    return(gettext("Inconclusive"))
}

.evObservedVariableName <- function(options, name, label) {
  variable <- .evOption(options, name, "")
  if (length(variable) != 1 || is.na(variable) || variable == "")
    stop(gettextf("%1$s must be selected.", label))

  return(variable)
}

.evObservedColumn <- function(dataset, variable, label) {
  if (!variable %in% names(dataset))
    stop(gettextf("%1$s was not found in the dataset.", label))

  return(dataset[[variable]])
}

.evObservedFiniteNumeric <- function(values, label) {
  values <- values[!is.na(values)]
  if (length(values) < 1)
    stop(gettextf("%1$s contains no observed values.", label))

  if (!is.numeric(values))
    stop(gettextf("%1$s must be numeric.", label))

  if (any(!is.finite(values)))
    stop(gettextf("%1$s contains infinite values.", label))

  return(values)
}

.evObservedNumber <- function(options, name, label) {
  value <- .evOption(options, name)
  if (length(value) != 1 || !is.numeric(value) || !is.finite(value))
    stop(gettextf("%1$s must be a finite number.", label))

  return(value)
}

.evObservedPositive <- function(options, name, label) {
  value <- .evObservedNumber(options, name, label)
  .evObservedCheckPositive(value, label)
  return(value)
}

.evObservedInteger <- function(options, name, label, minimum) {
  value <- .evObservedNumber(options, name, label)
  if (value < minimum || abs(value - round(value)) > sqrt(.Machine$double.eps))
    stop(gettextf("%1$s must be an integer of at least %2$s.", label, minimum))

  return(as.integer(round(value)))
}

.evObservedCheckSampleSize <- function(n, label, minimum) {
  if (n < minimum)
    stop(gettextf("%1$s must be at least %2$s.", label, minimum))
}

.evObservedCheckPositive <- function(value, label) {
  if (!is.finite(value) || value <= 0)
    stop(gettextf("%1$s must be positive.", label))
}

.evObservedValue <- function(summary, name) {
  value <- summary[[name]]
  if (is.null(value))
    return(NA)

  return(value)
}

.evObservedEstimateTitle <- function(settings) {
  if (settings[["isTTest"]])
    return("SMD")

  return(gettext("Estimate"))
}

.evObservedStatisticTitle <- function(settings) {
  if (settings[["isBinomial"]])
    return("x")

  if (settings[["isTTest"]])
    return("t")

  return("z")
}

.evObservedDecodeVariable <- function(variable) {
  decoded <- try(jaspBase::decodeColNames(variable), silent = TRUE)
  if (jaspBase::isTryError(decoded))
    return(variable)

  return(decoded)
}

.evText <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceText"]]))
    return()

  html <- createJaspHtml(title = gettext("Explanation"))
  html$dependOn(c(.evDependencies, "text"))
  html$position <- 4
  jaspResults[["evidenceText"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested Bayes factor design could not be completed with the current settings.")
    return()
  }

  if (settings[["calculation"]] == "sampleSize") {
    sampleText <- gettextf("The smallest sample size satisfying the selected planning targets is %1$s.", .evSampleSizeText(settings, result[["n1"]]))
    calculationText <- gettextf(
      "%1$s The corresponding conclusive evidence probabilities are %2$s.",
      sampleText,
      .evTargetProbabilityText(result)
    )
  } else {
    h1Result <- .evTargetResult(result, "h1")
    h0Result <- .evTargetResult(result, "h0")
    sampleText <- gettextf("With %1$s", .evSampleSizeText(settings, result[["n1"]]))
    calculationText <- gettextf(
      "%1$s, Pr(Conclusive Evidence) is %2$s for H\u2081 and %3$s for H\u2080.",
      sampleText,
      .evFormatNumber(h1Result[["probability"]]),
      .evFormatNumber(h0Result[["probability"]])
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

.evTargetProbabilityText <- function(result) {
  rows <- result[["targetResults"]]
  parts <- vapply(seq_len(nrow(rows)), function(i) {
    gettextf("%1$s for %2$s", .evFormatNumber(rows[["probability"]][i]), .evTargetLabel(rows[["target"]][i]))
  }, character(1))

  return(paste(parts, collapse = ", "))
}

.evTargetResult <- function(result, target) {
  rows <- result[["targetResults"]]
  index <- which(rows[["target"]] == target)
  if (length(index) == 0)
    return(rows[1, , drop = FALSE])

  return(rows[index[1], , drop = FALSE])
}

.evSampleSizeText <- function(settings, n1) {
  if (settings[["isIndependentSamples"]]) {
    return(gettextf(
      "N\u2081 = %1$s and N\u2082 = %2$s",
      n1,
      .evSampleSizeSecondGroup(settings, n1)
    ))
  }

  return(gettextf("N = %1$s", n1))
}

.evReport <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["evidenceReport"]]))
    return()

  html <- createJaspHtml(title = gettext("Report"))
  html$dependOn(c(.evDependencies, "generateReport", "generateReportLatex"))
  html$position <- 15
  jaspResults[["evidenceReport"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The report could not be generated because the Bayes factor design could not be completed with the current settings.")
    return()
  }

  paragraph <- paste(
    .evReportOpeningSentence(settings, designType = gettext("fixed-sample")),
    .evReportFixedPlanningSentence(settings, result),
    .evReportThresholdSentence(settings),
    .evReportPriorSentence(settings),
    .evReportProbabilitySentence(
      settings,
      h1Outcome = .evDesignOutcomeProbabilities(settings, .evResultN1ForUnder(result, "h1"), under = "h1"),
      h0Outcome = .evDesignOutcomeProbabilities(settings, .evResultN1ForUnder(result, "h0"), under = "h0")
    ),
    .evReportSoftwareSentence()
  )

  html[["text"]] <- .evReportHtml(paragraph, settings)
}

.evReportOpeningSentence <- function(settings, designType) {
  gettextf(
    "A %1$s Bayes factor design %2$s a %3$s %4$s %5$s.",
    designType,
    .evReportDesignAction(settings),
    .evReportAlternativeLabel(settings),
    .evReportTestLabel(settings),
    .evReportHypothesisClause(settings)
  )
}

.evReportDesignAction <- function(settings) {
  if (identical(settings[["calculation"]], "sampleSize"))
    return(gettext("specified"))

  return(gettext("evaluated"))
}

.evReportAlternativeLabel <- function(settings) {
  if (.evReportIsOneSided(settings))
    return(gettext("one-sided"))

  return(gettext("two-sided"))
}

.evReportIsOneSided <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return(identical(settings[["nullPriorDistribution"]], "direction"))

  if (.evUsesDirectionalZTest(settings))
    return(TRUE)

  alternative <- settings[["alternative"]]
  return(!is.null(alternative) && !identical(alternative, "two.sided"))
}

.evReportTestLabel <- function(settings) {
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

.evReportHypothesisClause <- function(settings) {
  gettextf(
    "(H\u2081: %1$s %2$s %3$s)",
    .evReportParameterSymbol(settings),
    .evReportH1Relation(settings),
    .evReportNumber(.evReportNullValue(settings))
  )
}

.evReportH1Relation <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]) && identical(settings[["nullPriorDistribution"]], "direction"))
    return(">")

  alternative <- settings[["alternative"]]
  if (identical(alternative, "less"))
    return("<")
  if (identical(alternative, "greater"))
    return(">")

  return("\u2260")
}

.evReportParameterSymbol <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return("p")

  return("\u03B4")
}

.evReportNullValue <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return(settings[["nullProportion"]])

  return(settings[["nullValue"]])
}

.evReportPriorSentence <- function(settings) {
  paste(
    gettextf(
      "The analysis prior for H\u2080 %1$s, and the analysis prior for H\u2081 %2$s.",
      .evReportAnalysisPriorPhrase(settings, "h0"),
      .evReportAnalysisPriorPhrase(settings, "h1")
    ),
    gettextf(
      "The design prior for H\u2080 %1$s, and the design prior for H\u2081 %2$s.",
      .evReportDesignPriorPhrase(settings, "h0"),
      .evReportDesignPriorPhrase(settings, "h1")
    )
  )
}

.evReportAnalysisPriorPhrase <- function(settings, under) {
  if (isTRUE(settings[["isBinomial"]]))
    return(.evReportBinomialAnalysisPriorPhrase(settings, under))

  if (under == "h0")
    return(.evReportContinuousNullPriorPhrase(settings))

  return(.evReportContinuousAnalysisPriorPhrase(settings))
}

.evReportContinuousNullPriorPhrase <- function(settings) {
  if (.evUsesDirectionalZTest(settings)) {
    return(gettextf(
      "used %1$s, truncated to %2$s",
      .evReportContinuousZPriorDistribution(settings),
      .evReportHypothesisRegion(settings, "h0")
    ))
  }

  return(.evReportPointMassPhrase(.evReportParameterSymbol(settings), settings[["nullValue"]]))
}

.evReportContinuousAnalysisPriorPhrase <- function(settings) {
  if (settings[["isTTest"]]) {
    prior <- if (isTRUE(settings[["analysisPriorIsCauchy"]])) {
      gettextf(
        "%1$s ~ Cauchy(location = %2$s, scale = %3$s)",
        .evReportParameterSymbol(settings),
        .evReportNumber(settings[["tPriorLocation"]]),
        .evReportNumber(settings[["tPriorScale"]])
      )
    } else {
      gettextf(
        "%1$s ~ Student-t(location = %2$s, scale = %3$s, df = %4$s)",
        .evReportParameterSymbol(settings),
        .evReportNumber(settings[["tPriorLocation"]]),
        .evReportNumber(settings[["tPriorScale"]]),
        .evReportNumber(settings[["tPriorDf"]])
      )
    }
  } else if (settings[["analysisPriorDistribution"]] == "point") {
    return(.evReportPointMassPhrase(.evReportParameterSymbol(settings), settings[["analysisPriorMean"]]))
  } else if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    prior <- .evReportContinuousZPriorDistribution(settings)
  } else {
    prior <- gettextf(
      "%1$s ~ Normal-moment(spread = %2$s, modes = \u00B1%3$s)",
      .evReportParameterSymbol(settings),
      .evReportNumber(settings[["momentPriorSpread"]]),
      .evReportNumber(settings[["momentPriorMode"]])
    )
  }

  if (.evReportIsOneSided(settings))
    return(gettextf("used %1$s, truncated to %2$s", prior, .evReportHypothesisRegion(settings, "h1")))

  return(gettextf("used %1$s", prior))
}

.evReportContinuousZPriorDistribution <- function(settings) {
  gettextf(
    "%1$s ~ Normal(mean = %2$s, SD = %3$s)",
    .evReportParameterSymbol(settings),
    .evReportNumber(settings[["analysisPriorMean"]]),
    .evReportNumber(settings[["analysisPriorSd"]])
  )
}

.evReportBinomialAnalysisPriorPhrase <- function(settings, under) {
  if (under == "h0" && !identical(settings[["nullPriorDistribution"]], "direction"))
    return(.evReportPointMassPhrase("p", settings[["nullProportion"]]))

  prior <- .evReportBetaPriorDistribution(
    symbol = "p",
    a      = settings[["analysisPriorSuccesses"]],
    b      = settings[["analysisPriorFailures"]]
  )

  if (identical(settings[["nullPriorDistribution"]], "direction"))
    return(gettextf("used %1$s, truncated to %2$s", prior, .evReportHypothesisRegion(settings, under)))

  return(gettextf("used %1$s", prior))
}

.evReportDesignPriorPhrase <- function(settings, under) {
  if (isTRUE(settings[["isBinomial"]]))
    return(.evReportBinomialDesignPriorPhrase(settings, under))

  prior <- .evContinuousDesignPriorForUnder(settings, under)
  if (identical(prior[["distribution"]], "point"))
    return(.evReportPointMassPhrase(.evReportParameterSymbol(settings), prior[["mean"]]))

  return(gettextf(
    "used %1$s ~ Normal(mean = %2$s, SD = %3$s)",
    .evReportParameterSymbol(settings),
    .evReportNumber(prior[["mean"]]),
    .evReportNumber(prior[["sd"]])
  ))
}

.evReportBinomialDesignPriorPhrase <- function(settings, under) {
  prior <- .evBinomialDesignPriorForUnder(settings, under)
  if (identical(prior[["distribution"]], "point"))
    return(.evReportPointMassPhrase("p", prior[["proportion"]]))

  return(gettextf(
    "used %1$s, truncated to %2$s \u2264 p \u2264 %3$s",
    .evReportBetaPriorDistribution("p", prior[["a"]], prior[["b"]]),
    .evReportNumber(prior[["lower"]]),
    .evReportNumber(prior[["upper"]])
  ))
}

.evReportPointMassPhrase <- function(symbol, value) {
  gettextf(
    "placed all mass at %1$s = %2$s",
    symbol,
    .evReportNumber(value)
  )
}

.evReportBetaPriorDistribution <- function(symbol, a, b) {
  gettextf(
    "%1$s ~ Beta(a = %2$s, b = %3$s)",
    symbol,
    .evReportNumber(a),
    .evReportNumber(b)
  )
}

.evReportHypothesisRegion <- function(settings, under) {
  h1Greater <- TRUE
  if (!isTRUE(settings[["isBinomial"]]))
    h1Greater <- identical(settings[["alternative"]], "greater")

  symbol <- .evReportParameterSymbol(settings)
  null   <- .evReportNumber(.evReportNullValue(settings))

  if (h1Greater && under == "h1")
    return(gettextf("%1$s > %2$s", symbol, null))
  if (h1Greater)
    return(gettextf("%1$s \u2264 %2$s", symbol, null))
  if (under == "h1")
    return(gettextf("%1$s < %2$s", symbol, null))

  return(gettextf("%1$s \u2265 %2$s", symbol, null))
}

.evReportThresholdSentence <- function(settings) {
  gettextf(
    "The design used evidence thresholds of BF\u2081\u2080 \u2265 %1$s for H\u2081 and BF\u2080\u2081 \u2265 %2$s for H\u2080 (equivalently, BF\u2081\u2080 \u2264 %3$s).",
    .evReportNumber(settings[["bf10Threshold"]]),
    .evReportNumber(settings[["bf01Threshold"]]),
    .evReportReciprocalText(settings[["bf01Threshold"]])
  )
}

.evReportFixedPlanningSentence <- function(settings, result) {
  h1N <- .evResultN1ForUnder(result, "h1")
  h0N <- .evResultN1ForUnder(result, "h0")

  if (!identical(settings[["calculation"]], "sampleSize"))
    return(gettextf("The design evaluated %1$s.", .evSampleSizeText(settings, result[["n1"]])))

  if (identical(h1N, h0N))
    return(gettextf(
      "The design selected %1$s to target conclusive evidence probabilities of %2$s under H\u2081 and %3$s under H\u2080.",
      .evSampleSizeText(settings, h1N),
      .evReportPercent(.evTargetPower(settings, "h1")),
      .evReportPercent(.evTargetPower(settings, "h0"))
    ))

  return(gettextf(
    "The design selected %1$s for the H\u2081 planning target and %2$s for the H\u2080 planning target, with target conclusive evidence probabilities of %3$s and %4$s, respectively.",
    .evSampleSizeText(settings, h1N),
    .evSampleSizeText(settings, h0N),
    .evReportPercent(.evTargetPower(settings, "h1")),
    .evReportPercent(.evTargetPower(settings, "h0"))
  ))
}

.evReportProbabilitySentence <- function(settings, h1Outcome, h0Outcome) {
  gettextf(
    "The achieved probability of conclusive evidence was %1$s under H\u2081 and %2$s under H\u2080; the probability of misleading evidence was %3$s under H\u2081 and %4$s under H\u2080.",
    .evReportPercent(h1Outcome[["alternative"]]),
    .evReportPercent(h0Outcome[["null"]]),
    .evReportPercent(h1Outcome[["null"]]),
    .evReportPercent(h0Outcome[["alternative"]])
  )
}

.evReportSoftwareSentence <- function() {
  gettextf(
    "The design analysis used JASP with the jaspPower module version %1$s and the bfpwr R package version %2$s.",
    .evPackageVersion("jaspPower"),
    .evPackageVersion("bfpwr")
  )
}

.evPackageVersion <- function(package) {
  version <- try(as.character(utils::packageVersion(package)), silent = TRUE)
  if (jaspBase::isTryError(version) || length(version) == 0)
    return(gettext("unknown"))

  return(version)
}

.evReportHtml <- function(paragraph, settings = NULL) {
  if (!is.null(settings) && isTRUE(settings[["reportLatex"]]))
    paragraph <- .evReportLatexText(paragraph)

  paste0("<p>", .evEscapeHtml(paragraph), "</p>")
}

.evReportLatexText <- function(paragraph) {
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

.evReportNumber <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(.evFormatNumber(x[1]))
}

.evReportPercent <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(paste0(formatC(100 * x[1], format = "f", digits = 1), "%"))
}

.evReportReciprocalText <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(gettextf("1/%1$s", .evReportNumber(x[1])))
}

.evReportMomentNumber <- function(x) {
  if (length(x) == 0 || !isTRUE(is.finite(x[1])))
    return(gettext("not available"))

  return(formatC(x[1], format = "f", digits = 1))
}

.evRCode <- function(jaspResults, settings, result, validation) {
  if (!is.null(jaspResults[["evidenceRCode"]]))
    return()

  html <- createJaspHtml(title = gettext("R Code"))
  html$dependOn(c(.evDependencies, "generateRCode"))
  html$position <- 14
  jaspResults[["evidenceRCode"]] <- html

  if (jaspBase::isTryError(validation)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .evCleanError(validation))
    return()
  }

  code <- try(.evBfpwrCall(settings, result), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .evCleanError(code))
    return()
  }

  html[["text"]] <- .evCodeHtml(code)
}

.evBfpwrCall <- function(settings, result = NULL) {
  calls <- vapply(.evResultTargets(settings), function(target) {
    targetSettings <- .evSettingsForTargetCall(settings, target)
    prefix <- if (target == "h1") "# Plan for evidence for H1" else "# Plan for evidence for H0"

    call <- if (targetSettings[["isBinomial"]]) {
      .evBinomialBfpwrCall(targetSettings)
    } else if (targetSettings[["isZTest"]]) {
      .evZBfpwrCall(targetSettings)
    } else {
      .evTBfpwrCall(targetSettings)
    }

    paste(prefix, call, sep = "\n")
  }, character(1))

  return(paste(calls, collapse = "\n\n"))
}

.evSettingsForTargetCall <- function(settings, target) {
  targetSettings <- settings
  designPrior <- if (target == "h0") {
    settings[["designPriorUnderH0"]]
  } else {
    settings[["designPriorUnderH1"]]
  }

  targetSettings[["evidenceTarget"]] <- target
  targetSettings[["bfThreshold"]]    <- .evThreshold(settings, target)
  targetSettings[["eventK"]]         <- .evEventK(settings, target)
  targetSettings[["lowerTail"]]      <- .evLowerTail(target)

  if (settings[["isBinomial"]]) {
    targetSettings[["designPriorUnderH1"]] <- designPrior
  } else {
    targetSettings[["designPriorMean"]] <- designPrior[["mean"]]
    targetSettings[["designPriorSd"]]   <- designPrior[["sd"]]
  }

  return(targetSettings)
}

.evBinomialBfpwrCall <- function(settings) {
  if (settings[["calculation"]] == "sampleSize") {
    args <- c(
      list(
        k          = settings[["eventK"]],
        power      = .evTargetPower(settings, settings[["evidenceTarget"]]),
        p0         = settings[["nullProportion"]],
        type       = settings[["nullPriorDistribution"]],
        a          = settings[["analysisPriorSuccesses"]],
        b          = settings[["analysisPriorFailures"]],
        lower.tail = settings[["lowerTail"]],
        nrange     = c(.evMinimumSampleSize(settings), ceiling(settings[["rangeMax"]]))
      ),
      .evBinomialDesignArguments(settings)
    )

    return(.evFormatRCall("bfpwr::nbinbf01", args))
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
    .evBinomialDesignArguments(settings)
  )

  return(.evFormatRCall("bfpwr::pbinbf01", args))
}

.evZBfpwrCall <- function(settings) {
  normalPrior <- settings[["analysisPriorDistribution"]] %in% c("point", "normal")

  if (settings[["calculation"]] == "sampleSize") {
    n <- .evMinimumSampleSize(settings)
    if (normalPrior) {
      args <- list(
        k          = settings[["eventK"]],
        power      = .evTargetPower(settings, settings[["evidenceTarget"]]),
        usd        = .evZUnitStandardDeviation(settings, n),
        null       = settings[["nullValue"]],
        pm         = settings[["analysisPriorMean"]],
        psd        = settings[["analysisPriorSd"]],
        dpm        = settings[["designPriorMean"]],
        dpsd       = settings[["designPriorSd"]],
        nrange     = c(n, ceiling(settings[["rangeMax"]])),
        lower.tail = settings[["lowerTail"]]
      )

      return(.evFormatRCall("bfpwr::nbf01", args))
    }

    args <- list(
      k          = settings[["eventK"]],
      power      = .evTargetPower(settings, settings[["evidenceTarget"]]),
      usd        = .evZUnitStandardDeviation(settings, n),
      null       = settings[["nullValue"]],
      psd        = settings[["momentPriorSpread"]],
      dpm        = settings[["designPriorMean"]],
      dpsd       = settings[["designPriorSd"]],
      nrange     = c(n, ceiling(settings[["rangeMax"]])),
      lower.tail = settings[["lowerTail"]]
    )

    return(.evFormatRCall("bfpwr::nnmbf01", args))
  }

  k         <- if (settings[["evidenceTarget"]] == "h1") 1 / settings[["bfThreshold"]] else settings[["bfThreshold"]]
  lowerTail <- settings[["evidenceTarget"]] == "h1"
  n         <- settings[["sampleSize"]]

  if (normalPrior) {
    args <- list(
      k          = k,
      n          = n,
      usd        = .evZUnitStandardDeviation(settings, n),
      null       = settings[["nullValue"]],
      pm         = settings[["analysisPriorMean"]],
      psd        = settings[["analysisPriorSd"]],
      dpm        = settings[["designPriorMean"]],
      dpsd       = settings[["designPriorSd"]],
      lower.tail = lowerTail
    )

    return(.evFormatRCall("bfpwr::pbf01", args))
  }

  args <- list(
    k          = k,
    n          = n,
    usd        = .evZUnitStandardDeviation(settings, n),
    null       = settings[["nullValue"]],
    psd        = settings[["momentPriorSpread"]],
    dpm        = settings[["designPriorMean"]],
    dpsd       = settings[["designPriorSd"]],
    lower.tail = lowerTail
  )

  return(.evFormatRCall("bfpwr::pnmbf01", args))
}

.evTBfpwrCall <- function(settings) {
  if (settings[["calculation"]] == "sampleSize") {
    if (settings[["isIndependentSamples"]] && settings[["sampleSizeRatio"]] != 1)
      stop(gettext("R code generation for t-test sample-size search with unequal group sizes is not available."))

    if (settings[["drangeMode"]] == "custom")
      stop(gettext("R code generation for t-test sample-size search with a custom t search range is not available."))

    if (!isTRUE(all.equal(settings[["nullValue"]], 0)))
      stop(gettext("R code generation for t-test sample-size search with a nonzero null value is not available."))

    args <- list(
      k           = settings[["eventK"]],
      power       = .evTargetPower(settings, settings[["evidenceTarget"]]),
      null        = settings[["nullValue"]],
      plocation   = settings[["tPriorLocationRelative"]],
      pscale      = settings[["tPriorScale"]],
      pdf         = settings[["tPriorDf"]],
      type        = settings[["testType"]],
      alternative = settings[["alternative"]],
      dpm         = settings[["designPriorMean"]],
      dpsd        = settings[["designPriorSd"]],
      lower.tail  = settings[["lowerTail"]],
      nrange      = c(.evMinimumSampleSize(settings), ceiling(settings[["rangeMax"]]))
    )

    return(.evFormatRCall("bfpwr::ntbf01", args))
  }

  n1        <- settings[["sampleSize"]]
  n2        <- .evSampleSizeSecondGroup(settings, n1)
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
    drange      = .evTSearchRange(settings, n1, k)
  )

  return(.evFormatRCall("bfpwr::ptbf01", args))
}

.evSampleSizePlot <- function(jaspResults, settings, result) {
  for (spec in .evUnderPlotSpecs(settings, "evidenceBySampleSize", gettext("Conclusive Evidence and Misleading Evidence by N"), 9)) {
    .evSampleSizeOutcomePlot(
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

.evSampleSizeOutcomePlot <- function(jaspResults, settings, result, key, title, position, under) {
  if (!is.null(jaspResults[[key]]))
    return()

  plot <- createJaspPlot(title = title, width = .evPlotWidth(settings), height = 350)
  plot$dependOn(c(.evDependencies, "evidenceBySampleSize", "mergeH1H0Figures", "plotPoints", "logSampleSize", "legendPosition", "colorPalette"))
  plot$position <- position
  jaspResults[[key]] <- plot

  if (jaspBase::isTryError(result)) {
    .evSetOutcomePlotError(plot, result)
    return()
  }

  plotResult <- try(.evBuildSampleSizePlot(settings, result, under), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    .evSetOutcomePlotError(plot, plotResult)
    return()
  }

  plot$plotObject <- plotResult
}

.evUnderPlotSpecs <- function(settings, keyPrefix, title, position) {
  if (isTRUE(settings[["mergeH1H0Figures"]])) {
    return(list(list(
      key      = paste0(keyPrefix, "Plot"),
      title    = title,
      position = position,
      under    = NULL
    )))
  }

  unders <- .evCurveUnders(settings)
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

.evUnderKeySuffix <- function(under) {
  if (under == "h1")
    return("UnderH1")

  return("UnderH0")
}

.evPlotTitleUnder <- function(title, under) {
  gettextf("%1$s: %2$s", title, .evUnderLabel(under))
}

.evSetOutcomePlotError <- function(plot, error) {
  plot$setError(gettextf("Unable to compute conclusive evidence and misleading evidence curves: %1$s", .evCleanError(error)))
}

.evBuildSampleSizePlot <- function(settings, result, under) {
  minimumN <- .evMinimumSampleSize(settings)
  maximumN <- max(result[["n1"]] * 2, settings[["sampleSize"]] * 2, minimumN + 20, 50)

  if (settings[["calculation"]] == "sampleSize")
    maximumN <- min(settings[["rangeMax"]], maximumN)

  nValues <- if (isTRUE(settings[["logSampleSize"]])) {
    unique(ceiling(exp(seq(log(minimumN), log(maximumN), length.out = settings[["plotPoints"]]))))
  } else {
    unique(ceiling(seq(minimumN, maximumN, length.out = settings[["plotPoints"]])))
  }
  unders <- .evPlotUnders(settings, under)
  data   <- .evCurveBySampleSize(settings, nValues, unders)

  xLabel <- if (settings[["isIndependentSamples"]]) gettext("Sample size (group 1)") else gettext("Sample size")

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
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Evidence"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = n, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.2) +
      xScale +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Evidence"))
  }

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      ggplot2::geom_hline(yintercept = unique(vapply(unders, function(under) .evTargetPower(settings, under), numeric(1))), linetype = "dotted", color = "#555555")
  }

  return(.evApplyPlotTheme(plot, settings))
}

.evEffectSizePlot <- function(jaspResults, settings, result) {
  for (spec in .evUnderPlotSpecs(settings, "evidenceByEffectSize", .evEffectSizePlotTitle(settings), 5)) {
    .evEffectSizeOutcomePlot(
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

.evEffectSizeOutcomePlot <- function(jaspResults, settings, result, key, title, position, under) {
  if (!is.null(jaspResults[[key]]))
    return()

  plot <- createJaspPlot(title = title, width = .evPlotWidth(settings), height = 350)
  plot$dependOn(c(.evDependencies, "evidenceByEffectSize", "mergeH1H0Figures", "plotPoints", "legendPosition", "colorPalette"))
  plot$position <- position
  jaspResults[[key]] <- plot

  if (jaspBase::isTryError(result)) {
    .evSetOutcomePlotError(plot, result)
    return()
  }

  plotResult <- try(.evBuildEffectSizePlot(settings, result, under), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    .evSetOutcomePlotError(plot, plotResult)
    return()
  }

  plot$plotObject <- plotResult
}

.evEffectSizePlotTitle <- function(settings) {
  if (settings[["isBinomial"]])
    return(gettext("Conclusive Evidence and Misleading Evidence by Proportion"))

  return(gettext("Conclusive Evidence and Misleading Evidence by Effect Size"))
}

.evBuildEffectSizePlot <- function(settings, result, under) {
  if (settings[["isBinomial"]])
    return(.evBuildBinomialProportionPlot(settings, result, under))

  effectRange <- .evEffectRange(settings)
  effect      <- seq(effectRange[1], effectRange[2], length.out = settings[["plotPoints"]])
  unders      <- .evPlotUnders(settings, under)
  data        <- .evCurveByEffectSize(settings, effect, unders, result[["n1"]])
  xLabel      <- .evDesignPriorEffectAxisLabel(settings)

  showUnder <- length(unique(data[["under"]])) > 1
  if (showUnder) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = effect, y = probability, color = outcome, linetype = under)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Evidence"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = effect, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::labs(x = xLabel, y = gettext("Probability"), color = gettext("Evidence"))
  }

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      ggplot2::geom_hline(yintercept = unique(vapply(unders, function(under) .evTargetPower(settings, under), numeric(1))), linetype = "dotted", color = "#555555")
  }

  return(.evApplyPlotTheme(plot, settings))
}

.evDesignPriorEffectAxisLabel <- function(settings) {
  designPrior <- .evContinuousDesignPriorForUnder(settings, "h1")
  if (identical(designPrior[["distribution"]], "point"))
    return(gettext("Design prior location"))

  return(gettext("Design prior mean"))
}

.evBuildBinomialProportionPlot <- function(settings, result, under) {
  proportionAxis <- .evBinomialProportionAxis(settings)
  xRange         <- c(max(proportionAxis[["range"]][1], .Machine$double.eps), min(proportionAxis[["range"]][2], 1 - .Machine$double.eps))
  proportion     <- seq(xRange[1], xRange[2], length.out = settings[["plotPoints"]])
  unders         <- .evPlotUnders(settings, under)
  data           <- .evCurveByProportion(settings, proportion, unders, result[["n1"]])

  showUnder <- length(unique(data[["under"]])) > 1
  if (showUnder) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = proportion, y = probability, color = outcome, linetype = under)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      jaspGraphs::scale_x_continuous(
        name   = gettext("Design proportion"),
        breaks = proportionAxis[["breaks"]],
        limits = proportionAxis[["range"]]
      ) +
      ggplot2::labs(y = gettext("Probability"), color = gettext("Evidence"), linetype = gettext("Design Prior"))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = proportion, y = probability, color = outcome)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
      jaspGraphs::scale_x_continuous(
        name   = gettext("Design proportion"),
        breaks = proportionAxis[["breaks"]],
        limits = proportionAxis[["range"]]
      ) +
      ggplot2::labs(y = gettext("Probability"), color = gettext("Evidence"))
  }

  if (settings[["calculation"]] == "sampleSize") {
    plot <- plot +
      ggplot2::geom_hline(yintercept = unique(vapply(unders, function(under) .evTargetPower(settings, under), numeric(1))), linetype = "dotted", color = "#555555")
  }

  return(.evApplyPlotTheme(plot, settings))
}

.evPriorPlot <- function(jaspResults, settings, validation) {
  .evPriorPlotContainer(
    jaspResults  = jaspResults,
    settings     = settings,
    validation   = validation,
    key          = "evidencePriorPlot",
    position     = 13,
    dependencies = .evDependencies
  )
}

.evPriorPlotContainer <- function(jaspResults, settings, validation = NULL, key, position, dependencies) {
  if (!is.null(jaspResults[[key]]))
    return()

  container <- createJaspContainer(title = gettext("Prior Distribution"))
  container$dependOn(c(
    dependencies, "priorDistribution", "priorDistributionDesign",
    "priorDistributionAnalysis", "priorDistributionMerge", "plotPoints",
    "legendPosition", "colorPalette"
  ))
  container$position <- position
  jaspResults[[key]] <- container

  specs <- .evPriorPlotSpecs(settings)
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    plot <- createJaspPlot(title = spec[["title"]], width = .evPlotWidth(settings), height = 350)
    plot$position <- i
    container[[spec[["key"]]]] <- plot

    if (!is.null(validation) && jaspBase::isTryError(validation)) {
      plot$setError(gettextf("Unable to compute prior distribution plot: %1$s", .evCleanError(validation)))
      next
    }

    plotResult <- try(.evBuildPriorPlot(settings, spec[["priorSet"]]), silent = TRUE)
    if (jaspBase::isTryError(plotResult)) {
      plot$setError(gettextf("Unable to compute prior distribution plot: %1$s", .evCleanError(plotResult)))
      next
    }

    plot$plotObject <- plotResult
  }
}

.evPriorPlotSpecs <- function(settings) {
  priorSets <- .evSelectedPriorSets(settings)
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
  if ("analysis" %in% priorSets) {
    specs[[length(specs) + 1]] <- list(
      key      = "analysis",
      title    = gettext("Analysis Prior"),
      priorSet = "analysis"
    )
  }
  if ("design" %in% priorSets) {
    specs[[length(specs) + 1]] <- list(
      key      = "design",
      title    = gettext("Design Prior"),
      priorSet = "design"
    )
  }

  return(specs)
}

.evSelectedPriorSets <- function(settings) {
  priorSets <- character(0)
  if (isTRUE(settings[["priorPlotAnalysis"]]))
    priorSets <- c(priorSets, "analysis")
  if (isTRUE(settings[["priorPlotDesign"]]))
    priorSets <- c(priorSets, "design")

  return(priorSets)
}

.evPriorSetIncludes <- function(priorSet, value) {
  value %in% priorSet
}

.evPriorLegendTitle <- function(priorSet) {
  if (identical(priorSet, "analysis"))
    return(gettext("Analysis Prior"))

  if (identical(priorSet, "design"))
    return(gettext("Design Prior"))

  return(gettext("Prior"))
}

.evPriorRelabelData <- function(data, priorSet) {
  if (nrow(data) == 0 || length(priorSet) != 1)
    return(data)

  data[["prior"]] <- .evPriorLegendLabels(data[["prior"]], priorSet)
  return(data)
}

.evPriorLegendLabels <- function(labels, priorSet) {
  if (identical(priorSet, "analysis")) {
    labels[labels == .evAnalysisPriorPlotLabel("h0")] <- .evUnderLabel("h0")
    labels[labels == .evAnalysisPriorPlotLabel("h1")] <- .evUnderLabel("h1")
  }

  if (identical(priorSet, "design")) {
    labels[labels == .evDesignPriorPlotLabel("h0")] <- .evUnderLabel("h0")
    labels[labels == .evDesignPriorPlotLabel("h1")] <- .evUnderLabel("h1")
  }

  return(labels)
}

.evBuildPriorPlot <- function(settings, priorSet = c("analysis", "design")) {
  if (settings[["isBinomial"]])
    return(.evBuildBinomialPriorPlot(settings, priorSet))

  return(.evBuildContinuousPriorPlot(settings, priorSet))
}

.evCurveUnders <- function(settings) {
  return(c("h1", "h0"))
}

.evCurveOutcomes <- function() {
  return(c("power", "misleading"))
}

.evPlotUnders <- function(settings, under = NULL) {
  if (!is.null(under))
    return(under)

  return(.evCurveUnders(settings))
}

.evOutcomeLabel <- function(outcome) {
  if (outcome == "power")
    return(gettext("Conclusive"))

  return(gettext("Misleading"))
}

.evCurveLabel <- function(outcome, under, nUnders) {
  outcomeLabel <- .evOutcomeLabel(outcome)
  if (nUnders == 1)
    return(outcomeLabel)

  return(gettextf("%1$s: %2$s", outcomeLabel, .evUnderLabel(under)))
}

.evCurveBySampleSize <- function(settings, nValues, unders) {
  rows <- lapply(unders, function(under) {
    lapply(.evCurveOutcomes(), function(outcome) {
      target <- .evCurveTarget(under, outcome)
      data.frame(
        n           = nValues,
        probability = .evEvidenceProbability(settings, n1 = nValues, target = target, under = under),
        under       = .evUnderLabel(under),
        outcome     = .evOutcomeLabel(outcome),
        curve       = .evCurveLabel(outcome, under, length(unders)),
        stringsAsFactors = FALSE
      )
    })
  })

  return(do.call(rbind, unlist(rows, recursive = FALSE)))
}

.evCurveByEffectSize <- function(settings, effect, unders, n1) {
  rows <- lapply(unders, function(under) {
    lapply(.evCurveOutcomes(), function(outcome) {
      target          <- .evCurveTarget(under, outcome)
      designPriorMean <- if (under == "h1" || length(unders) == 1) effect else NULL
      data.frame(
        effect      = effect,
        probability = .evEvidenceProbability(settings, n1 = n1, target = target, under = under, designPriorMean = designPriorMean),
        under       = .evUnderLabel(under),
        outcome     = .evOutcomeLabel(outcome),
        curve       = .evCurveLabel(outcome, under, length(unders)),
        stringsAsFactors = FALSE
      )
    })
  })

  return(do.call(rbind, unlist(rows, recursive = FALSE)))
}

.evCurveByProportion <- function(settings, proportion, unders, n1) {
  rows <- lapply(unders, function(under) {
    lapply(.evCurveOutcomes(), function(outcome) {
      target          <- .evCurveTarget(under, outcome)
      designPriorMean <- if (under == "h1" || length(unders) == 1) proportion else NULL
      data.frame(
        proportion  = proportion,
        probability = .evEvidenceProbability(settings, n1 = n1, target = target, under = under, designPriorMean = designPriorMean),
        under       = .evUnderLabel(under),
        outcome     = .evOutcomeLabel(outcome),
        curve       = .evCurveLabel(outcome, under, length(unders)),
        stringsAsFactors = FALSE
      )
    })
  })

  return(do.call(rbind, unlist(rows, recursive = FALSE)))
}

.evCurveTarget <- function(under, outcome) {
  if (outcome == "power")
    return(under)

  return(.evOppositeTarget(under))
}

.evOppositeTarget <- function(target) {
  if (target == "h1")
    return("h0")

  return("h1")
}

.evBuildContinuousPriorPlot <- function(settings, priorSet) {
  priorAxis   <- .evPriorAxis(settings, priorSet)
  x           <- seq(priorAxis[["range"]][1], priorAxis[["range"]][2], length.out = settings[["plotPoints"]])
  densityData <- .evContinuousPriorDensityData(settings, x, priorSet)
  spikeData   <- .evContinuousPriorSpikeData(settings, priorSet, densityData)
  yAxis       <- .evPriorYAxis(densityData, spikeData)
  legendTitle <- .evPriorLegendTitle(priorSet)

  densityData <- .evPriorRelabelData(densityData, priorSet)
  spikeData   <- .evPriorRelabelData(spikeData, priorSet)

  plot <- ggplot2::ggplot() +
    jaspGraphs::scale_x_continuous(
      name   = gettext("Parameter value"),
      breaks = priorAxis[["breaks"]],
      limits = priorAxis[["range"]]
    )

  if (nrow(densityData) > 0) {
    plot <- plot + ggplot2::geom_line(
      data = densityData,
      ggplot2::aes(x = x, y = density, color = prior),
      linewidth = 1.1
    )
  }

  if (nrow(spikeData) > 0) {
    plot <- plot + ggplot2::geom_segment(
      data = spikeData,
      ggplot2::aes(x = x, xend = x, y = 0, yend = height, color = prior),
      arrow       = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      linewidth   = 1.1,
      inherit.aes = FALSE
    )
  }

  plot <- plot +
    ggplot2::scale_y_continuous(
      breaks = yAxis[["breaks"]],
      labels = yAxis[["labels"]],
      limits = yAxis[["range"]],
      expand = .evPriorYAxisExpansion()
    ) +
    ggplot2::labs(y = gettext("Density"), color = legendTitle)

  return(.evApplyPlotTheme(plot, settings))
}

.evContinuousPriorDensityData <- function(settings, x, priorSet) {
  rows <- list()

  if (.evPriorSetIncludes(priorSet, "analysis"))
    rows <- c(rows, .evContinuousAnalysisPriorDensityRows(settings, x))

  if (.evPriorSetIncludes(priorSet, "design"))
    rows <- c(rows, .evContinuousDesignPriorDensityRows(settings, x))

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

.evContinuousAnalysisPriorDensityRows <- function(settings, x) {
  if (settings[["isZTest"]])
    return(.evZAnalysisPriorDensityRows(settings, x))

  return(.evTAnalysisPriorDensityRows(settings, x))
}

.evZAnalysisPriorDensityRows <- function(settings, x) {
  if (settings[["analysisPriorDistribution"]] == "point")
    return(list())

  if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    if (.evUsesDirectionalZTest(settings))
      return(.evDirectionalNormalPriorDensityRows(settings, x))

    return(list(.evPriorDensityRows(
      x       = x,
      density = stats::dnorm(x, mean = settings[["analysisPriorMean"]], sd = settings[["analysisPriorSd"]]),
      prior   = .evAnalysisPriorPlotLabel("h1")
    )))
  }

  density <- ((x - settings[["nullValue"]])^2 / settings[["momentPriorSpread"]]^2) *
    stats::dnorm(x, mean = settings[["nullValue"]], sd = settings[["momentPriorSpread"]])

  return(list(.evPriorDensityRows(
    x       = x,
    density = density,
    prior   = .evAnalysisPriorPlotLabel("h1")
  )))
}

.evDirectionalNormalPriorDensityRows <- function(settings, x) {
  rows <- list()
  h1   <- .evDirectionalNormalPriorDensity(settings, x, "h1")
  h0   <- .evDirectionalNormalPriorDensity(settings, x, "h0")

  rows[[length(rows) + 1]] <- .evPriorDensityRows(
    x            = x,
    density      = h1[["density"]],
    prior        = .evAnalysisPriorPlotLabel("h1"),
    lower        = h1[["lower"]],
    upper        = h1[["upper"]],
    lowerDensity = h1[["lowerDensity"]],
    upperDensity = h1[["upperDensity"]]
  )
  rows[[length(rows) + 1]] <- .evPriorDensityRows(
    x            = x,
    density      = h0[["density"]],
    prior        = .evAnalysisPriorPlotLabel("h0"),
    lower        = h0[["lower"]],
    upper        = h0[["upper"]],
    lowerDensity = h0[["lowerDensity"]],
    upperDensity = h0[["upperDensity"]]
  )

  return(rows)
}

.evDirectionalNormalPriorDensity <- function(settings, x, under) {
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

.evTAnalysisPriorDensityRows <- function(settings, x) {
  rawDensity <- stats::dt((x - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]]) / settings[["tPriorScale"]]
  nullValue  <- settings[["nullValue"]]

  if (settings[["alternative"]] == "two.sided") {
    return(list(.evPriorDensityRows(
      x       = x,
      density = rawDensity,
      prior   = .evAnalysisPriorPlotLabel("h1")
    )))
  }

  if (settings[["alternative"]] == "greater") {
    normalizer <- 1 - stats::pt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]])
    density    <- rawDensity / normalizer
    atNull     <- stats::dt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]]) / settings[["tPriorScale"]] / normalizer
    return(list(.evPriorDensityRows(
      x            = x,
      density      = density,
      prior        = .evAnalysisPriorPlotLabel("h1"),
      lower        = nullValue,
      lowerDensity = atNull
    )))
  } else {
    normalizer <- stats::pt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]])
    density    <- rawDensity / normalizer
    atNull     <- stats::dt((nullValue - settings[["tPriorLocation"]]) / settings[["tPriorScale"]], df = settings[["tPriorDf"]]) / settings[["tPriorScale"]] / normalizer
    return(list(.evPriorDensityRows(
      x            = x,
      density      = density,
      prior        = .evAnalysisPriorPlotLabel("h1"),
      upper        = nullValue,
      upperDensity = atNull
    )))
  }
}

.evContinuousDesignPriorDensityRows <- function(settings, x) {
  rows <- list()
  for (under in c("h0", "h1")) {
    designPrior <- .evContinuousDesignPriorForUnder(settings, under)
    if (designPrior[["distribution"]] == "normal") {
      rows[[length(rows) + 1]] <- .evPriorDensityRows(
        x       = x,
        density = stats::dnorm(x, mean = designPrior[["mean"]], sd = designPrior[["sd"]]),
        prior   = .evDesignPriorPlotLabel(under)
      )
    }
  }

  return(rows)
}

.evBuildBinomialPriorPlot <- function(settings, priorSet) {
  priorAxis   <- .evPriorAxis(settings, priorSet)
  xRange      <- c(max(priorAxis[["range"]][1], .Machine$double.eps), min(priorAxis[["range"]][2], 1 - .Machine$double.eps))
  x           <- seq(xRange[1], xRange[2], length.out = settings[["plotPoints"]])
  densityData <- .evBinomialPriorDensityData(settings, x, priorSet)
  spikeData   <- .evBinomialPriorSpikeData(settings, priorSet, densityData)
  yAxis       <- .evPriorYAxis(densityData, spikeData)
  legendTitle <- .evPriorLegendTitle(priorSet)

  densityData <- .evPriorRelabelData(densityData, priorSet)
  spikeData   <- .evPriorRelabelData(spikeData, priorSet)

  plot <- ggplot2::ggplot() +
    jaspGraphs::scale_x_continuous(
      name   = gettext("Proportion"),
      breaks = priorAxis[["breaks"]],
      limits = priorAxis[["range"]]
    )

  if (nrow(densityData) > 0) {
    plot <- plot + ggplot2::geom_line(
      data = densityData,
      ggplot2::aes(x = x, y = density, color = prior),
      linewidth = 1.1
    )
  }

  if (nrow(spikeData) > 0) {
    plot <- plot + ggplot2::geom_segment(
      data = spikeData,
      ggplot2::aes(x = x, xend = x, y = 0, yend = height, color = prior),
      arrow       = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      linewidth   = 1.1,
      inherit.aes = FALSE
    )
  }

  plot <- plot +
    ggplot2::scale_y_continuous(
      breaks = yAxis[["breaks"]],
      labels = yAxis[["labels"]],
      limits = yAxis[["range"]],
      expand = .evPriorYAxisExpansion()
    ) +
    ggplot2::labs(y = gettext("Density"), color = legendTitle)

  return(.evApplyPlotTheme(plot, settings))
}

.evBinomialPriorDensityData <- function(settings, x, priorSet) {
  rows <- list()

  if (.evPriorSetIncludes(priorSet, "analysis"))
    rows <- c(rows, .evBinomialAnalysisPriorDensityRows(settings, x))

  if (.evPriorSetIncludes(priorSet, "design"))
    rows <- c(rows, .evBinomialDesignPriorDensityRows(settings, x))

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

.evBinomialAnalysisPriorDensityRows <- function(settings, x) {
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

  rows <- list(.evPriorDensityRows(
    x       = x,
    density = h1Density,
    prior   = .evAnalysisPriorPlotLabel("h1"),
    lower   = if (settings[["nullPriorDistribution"]] == "direction") settings[["nullProportion"]] else -Inf,
    lowerDensity = if (settings[["nullPriorDistribution"]] == "direction") h1AtNull else NULL
  ))

  if (!is.null(h0Density)) {
    rows[[length(rows) + 1]] <- .evPriorDensityRows(
      x            = x,
      density      = h0Density,
      prior        = .evAnalysisPriorPlotLabel("h0"),
      upper        = settings[["nullProportion"]],
      upperDensity = h0AtNull
    )
  }

  return(rows)
}

.evBinomialDesignPriorDensityRows <- function(settings, x) {
  rows <- list()
  for (under in c("h0", "h1")) {
    designPrior <- .evBinomialDesignPriorForUnder(settings, under)
    if (designPrior[["distribution"]] == "beta") {
      designDensity <- stats::dbeta(x, designPrior[["a"]], designPrior[["b"]])
      designNormalizer <- diff(stats::pbeta(c(designPrior[["lower"]], designPrior[["upper"]]),
                                            designPrior[["a"]],
                                            designPrior[["b"]]))
      lowerDensity <- stats::dbeta(designPrior[["lower"]], designPrior[["a"]], designPrior[["b"]]) / designNormalizer
      upperDensity <- stats::dbeta(designPrior[["upper"]], designPrior[["a"]], designPrior[["b"]]) / designNormalizer
      rows[[length(rows) + 1]] <- .evPriorDensityRows(
        x            = x,
        density      = designDensity / designNormalizer,
        prior        = .evDesignPriorPlotLabel(under),
        lower        = designPrior[["lower"]],
        upper        = designPrior[["upper"]],
        lowerDensity = lowerDensity,
        upperDensity = upperDensity
      )
    }
  }

  return(rows)
}

.evContinuousPriorSpikeData <- function(settings, priorSet, densityData) {
  height <- .evPriorSpikeHeight(densityData)
  rows   <- list()

  if (.evPriorSetIncludes(priorSet, "analysis")) {
    if (!.evUsesDirectionalZTest(settings)) {
      rows[[length(rows) + 1]] <- .evPriorSpikeRow(settings[["nullValue"]], .evAnalysisPriorPlotLabel("h0"), height)
    }
    if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "point") {
      rows[[length(rows) + 1]] <- .evPriorSpikeRow(settings[["analysisPriorMean"]], .evAnalysisPriorPlotLabel("h1"), height)
    }
  }

  if (.evPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .evContinuousDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "point")
        rows[[length(rows) + 1]] <- .evPriorSpikeRow(designPrior[["mean"]], .evDesignPriorPlotLabel(under), height)
    }
  }

  return(.evPriorSpikeRows(rows))
}

.evBinomialPriorSpikeData <- function(settings, priorSet, densityData) {
  height <- .evPriorSpikeHeight(densityData)
  rows   <- list()

  if (.evPriorSetIncludes(priorSet, "analysis") && settings[["nullPriorDistribution"]] != "direction")
    rows[[length(rows) + 1]] <- .evPriorSpikeRow(settings[["nullProportion"]], .evAnalysisPriorPlotLabel("h0"), height)

  if (.evPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .evBinomialDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "point")
        rows[[length(rows) + 1]] <- .evPriorSpikeRow(designPrior[["proportion"]], .evDesignPriorPlotLabel(under), height)
    }
  }

  return(.evPriorSpikeRows(rows))
}

.evPriorSpikeHeight <- function(densityData) {
  if (nrow(densityData) == 0)
    return(1)

  density <- densityData[["density"]]
  density <- density[is.finite(density)]
  if (length(density) == 0 || max(density) <= 0)
    return(1)

  return(1.1 * max(density))
}

.evPriorSpikeRow <- function(x, prior, height) {
  return(data.frame(
    x       = x,
    height  = height,
    prior   = prior,
    stringsAsFactors = FALSE
  ))
}

.evPriorSpikeRows <- function(rows) {
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

.evPriorYAxis <- function(densityData, spikeData) {
  if (nrow(densityData) == 0 && nrow(spikeData) > 0) {
    return(list(
      breaks = c(0, 1),
      labels = c("0", "\u221E"),
      range  = c(0, 1)
    ))
  }

  y <- c(densityData[["density"]], spikeData[["height"]])
  y <- y[is.finite(y)]
  yMaximum <- if (length(y) == 0) 1 else max(y, 1)
  breaks   <- .evPriorPrettyBreaks(c(0, yMaximum))

  if (max(breaks) < yMaximum)
    breaks <- .evPriorPrettyBreaks(c(0, yMaximum * 1.05))

  return(list(
    breaks = breaks,
    labels = ggplot2::waiver(),
    range  = range(breaks)
  ))
}

.evPriorYAxisExpansion <- function() {
  return(ggplot2::expansion(mult = c(0, 0.04)))
}

.evPriorPrettyBreaks <- function(x) {
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

.evPriorDensityRows <- function(x, density, prior, lower = -Inf, upper = Inf, lowerDensity = NULL, upperDensity = NULL) {
  if (!is.finite(lower) && !is.finite(upper))
    return(.evPriorDensityFrame(x, density, prior))

  parts <- list()
  if (is.finite(lower)) {
    parts[[length(parts) + 1]] <- .evPriorDensityFrame(x[x < lower], rep(0, sum(x < lower)), prior)
    parts[[length(parts) + 1]] <- .evPriorDensityFrame(lower, 0, prior)
    if (!is.null(lowerDensity) && is.finite(lowerDensity))
      parts[[length(parts) + 1]] <- .evPriorDensityFrame(lower, lowerDensity, prior)
  }

  support <- x > lower & x < upper
  parts[[length(parts) + 1]] <- .evPriorDensityFrame(x[support], density[support], prior)

  if (is.finite(upper)) {
    if (!is.null(upperDensity) && is.finite(upperDensity))
      parts[[length(parts) + 1]] <- .evPriorDensityFrame(upper, upperDensity, prior)
    parts[[length(parts) + 1]] <- .evPriorDensityFrame(upper, 0, prior)
    parts[[length(parts) + 1]] <- .evPriorDensityFrame(x[x > upper], rep(0, sum(x > upper)), prior)
  }

  return(do.call(rbind, parts))
}

.evPriorDensityFrame <- function(x, density, prior) {
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

.evApplyPlotTheme <- function(plot, settings) {
  palette        <- .evOption(settings, "colorPalette", "colorblind")
  legendPosition <- .evOption(settings, "legendPosition", "right")

  plot <- .pwrApplyPlotTheme(plot, legendPosition = .evLegendPosition(legendPosition)) +
    jaspGraphs::scale_JASPcolor_discrete(palette, labels = .evPlotmathLabels) +
    ggplot2::scale_linetype_discrete(labels = .evPlotmathLabels)

  if (legendPosition == "rightInside") {
    plot <- plot +
      ggplot2::theme(
        legend.position.inside = c(0.98, 0.5),
        legend.justification   = c(1, 0.5)
      )
  }

  return(plot)
}

.evLegendPosition <- function(legendPosition) {
  if (identical(legendPosition, "rightInside"))
    return("inside")

  return(legendPosition)
}

.evPlotWidth <- function(settings) {
  width <- 735
  if (.evOption(settings, "legendPosition", "right") %in% c("top", "bottom", "rightInside"))
    return(round(width * 0.8))

  return(width)
}

.evPlotmathLabels <- function(labels) {
  labels <- as.character(labels)
  parsed <- vapply(labels, .evPlotmathLabel, character(1), USE.NAMES = FALSE)
  parse(text = parsed)
}

.evPlotmathLabel <- function(label) {
  tokenMap <- setNames(
    c(
      'BF["10"]', 'BF["01"]', "H[1]", "H[0]",
      "N[1]", "N[2]", "n[1]", "n[2]",
      "p[0]", "p[1]", "p[2]",
      "theta[0]", "lambda[0]", "lambda[1]", "lambda[2]",
      "mu[0]", "sigma[0]", "s[1]", "s[2]"
    ),
    c(
      paste0("BF", "\u2081", "\u2080"), paste0("BF", "\u2080", "\u2081"),
      paste0("H", "\u2081"), paste0("H", "\u2080"),
      paste0("N", "\u2081"), paste0("N", "\u2082"),
      paste0("n", "\u2081"), paste0("n", "\u2082"),
      paste0("p", "\u2080"), paste0("p", "\u2081"), paste0("p", "\u2082"),
      paste0("\u03B8", "\u2080"),
      paste0("\u03BB", "\u2080"), paste0("\u03BB", "\u2081"), paste0("\u03BB", "\u2082"),
      paste0("\u03BC", "\u2080"), paste0("\u03C3", "\u2080"),
      paste0("s", "\u2081"), paste0("s", "\u2082")
    )
  )
  pattern <- paste(names(tokenMap), collapse = "|")
  matches <- gregexpr(pattern, label, perl = TRUE)[[1]]

  if (matches[1] == -1)
    return(.evPlotmathText(label))

  matchLengths <- attr(matches, "match.length")
  parts        <- character()
  cursor       <- 1

  for (i in seq_along(matches)) {
    start <- matches[i]
    end   <- start + matchLengths[i] - 1

    if (start > cursor)
      parts <- c(parts, .evPlotmathText(substr(label, cursor, start - 1)))

    token <- substr(label, start, end)
    parts <- c(parts, unname(tokenMap[[token]]))
    cursor <- end + 1
  }

  if (cursor <= nchar(label))
    parts <- c(parts, .evPlotmathText(substr(label, cursor, nchar(label))))

  if (length(parts) == 1)
    return(parts)

  paste0("paste(", paste(parts, collapse = ", "), ")")
}

.evPlotmathText <- function(text) {
  text <- gsub("\\", "\\\\", text, fixed = TRUE)
  text <- gsub("\"", "\\\"", text, fixed = TRUE)
  paste0("\"", text, "\"")
}

.evZUnitStandardDeviation <- function(settings, n1) {
  if (isTRUE(settings[["isGeneralZ"]])) {
    return(.evGeneralZUnitInformationSd(settings))
  }

  if (!settings[["isIndependentSamples"]])
    return(settings[["standardDeviation"]])

  n2 <- .evSampleSizeSecondGroup(settings, n1)
  return(settings[["standardDeviation"]] * sqrt(1 + n1 / n2))
}

.evGeneralZUnitInformationSd <- function(settings) {
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

.evGeneralZKnownUisdFootnote <- function(settings) {
  if (!isTRUE(settings[["isGeneralZ"]]))
    return(NULL)

  if (!.evGeneralZUsesKnownUisd(settings))
    return(NULL)

  gettextf(
    "The %1$s parameterization assumes a unit information standard deviation (UISD) of %2$s.",
    .evGeneralZParameterizationLabel(settings[["generalZParameterization"]]),
    .evFormatNumber(.evGeneralZUnitInformationSd(settings))
  )
}

.evGeneralZUsesKnownUisd <- function(settings) {
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

.evGeneralZParameterizationLabel <- function(parameterization) {
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

.evSampleSizeSecondGroup <- function(settings, n1) {
  if (!settings[["isIndependentSamples"]])
    return(n1)

  return(ceiling(n1 * settings[["sampleSizeRatio"]]))
}

.evValidateSampleSize <- function(settings, n1) {
  if (settings[["isIndependentSamples"]] && settings[["isTTest"]] && .evSampleSizeSecondGroup(settings, n1) <= 1)
    stop(gettext("The sample-size ratio leads to a second-group sample size smaller than 2."))
}

.evMinimumSampleSize <- function(settings) {
  minimumN <- max(ceiling(settings[["rangeMin"]]), if (settings[["isBinomial"]]) 1 else 2)

  if (settings[["isIndependentSamples"]] && settings[["isTTest"]]) {
    while (.evSampleSizeSecondGroup(settings, minimumN) <= 1)
      minimumN <- minimumN + 1
  }

  return(minimumN)
}

.evBinomialDesignPriorForUnder <- function(settings, under) {
  if (under == "h0")
    return(settings[["designPriorUnderH0"]])

  return(settings[["designPriorUnderH1"]])
}

.evBinomialDesignArguments <- function(settings, under = "h1") {
  designPrior <- .evBinomialDesignPriorForUnder(settings, under)
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

.evValidateSettings <- function(settings) {
  .evValidateTargetPowers(settings)

  if (identical(settings[["calculation"]], "sampleSize") && length(settings[["planningTargets"]]) == 0)
    stop(gettext("Select at least one Bayes factor planning target."))

  if (settings[["isBinomial"]]) {
    for (under in c("h0", "h1")) {
      designPrior <- .evBinomialDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "beta" && designPrior[["lower"]] >= designPrior[["upper"]])
        stop(gettext("The lower beta design-prior truncation must be smaller than the upper truncation."))
    }
  }
}

.evDrange <- function(settings) {
  if (settings[["drangeMode"]] != "custom")
    return("adaptive")

  if (settings[["drangeLower"]] >= settings[["drangeUpper"]])
    stop(gettext("The lower t search bound must be smaller than the upper bound."))

  return(c(settings[["drangeLower"]], settings[["drangeUpper"]]))
}

.evTSearchRange <- function(settings, n1, k) {
  if (settings[["drangeMode"]] == "custom")
    return(.evDrange(settings))

  if (isTRUE(all.equal(settings[["nullValue"]], 0)))
    return("adaptive")

  if (settings[["alternative"]] != "two.sided")
    return("adaptive")

  n2    <- .evSampleSizeSecondGroup(settings, n1)
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

  return(range(c(searchRange, .evEffectRange(settings))))
}

.evPriorAxis <- function(settings, priorSet = c("analysis", "design")) {
  requestedRange <- if (settings[["isBinomial"]]) .evBinomialPriorRange(settings, priorSet) else .evContinuousPriorRange(settings, priorSet)
  breaks         <- .evPrettyPriorBreaks(requestedRange, isBinomial = settings[["isBinomial"]])

  return(list(
    breaks = breaks,
    range  = range(breaks)
  ))
}

.evContinuousPriorRange <- function(settings, priorSet = c("analysis", "design")) {
  rangeValues <- numeric(0)

  if (.evPriorSetIncludes(priorSet, "analysis")) {
    rangeValues <- c(rangeValues, settings[["nullValue"]])

    if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] == "point") {
      rangeValues <- c(rangeValues, settings[["analysisPriorMean"]])
    } else if (settings[["isZTest"]] && settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
      rangeValues <- c(rangeValues, .evPriorInterval(settings[["analysisPriorMean"]], settings[["analysisPriorSd"]]))
    } else if (settings[["isZTest"]]) {
      rangeValues <- c(rangeValues, .evPriorInterval(settings[["nullValue"]], sqrt(3) * settings[["momentPriorSpread"]]))
    } else {
      rangeValues <- c(rangeValues, .evPriorInterval(settings[["tPriorLocation"]], .evStudentTPriorSpread(settings)))

      if (settings[["alternative"]] != "two.sided")
        rangeValues <- c(rangeValues, settings[["nullValue"]])
    }
  }

  if (.evPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .evContinuousDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "normal") {
        rangeValues <- c(rangeValues, .evPriorInterval(designPrior[["mean"]], designPrior[["sd"]]))
      } else {
        rangeValues <- c(rangeValues, designPrior[["mean"]])
      }
    }
  }

  return(.evFiniteRange(rangeValues))
}

.evBinomialPriorRange <- function(settings, priorSet = c("analysis", "design")) {
  rangeValues <- numeric(0)

  if (.evPriorSetIncludes(priorSet, "analysis")) {
    rangeValues <- c(
      rangeValues,
      settings[["nullProportion"]],
      .evBetaPriorInterval(settings[["analysisPriorSuccesses"]], settings[["analysisPriorFailures"]])
    )
  }

  if (.evPriorSetIncludes(priorSet, "design")) {
    for (under in c("h0", "h1")) {
      designPrior <- .evBinomialDesignPriorForUnder(settings, under)
      if (designPrior[["distribution"]] == "point") {
        rangeValues <- c(rangeValues, designPrior[["proportion"]])
      } else {
        rangeValues <- c(rangeValues, .evBetaPriorInterval(
          designPrior[["a"]],
          designPrior[["b"]],
          lower = designPrior[["lower"]],
          upper = designPrior[["upper"]]
        ))
      }
    }
  }

  rangeValues <- pmin(1, pmax(0, rangeValues))

  return(.evFiniteRange(rangeValues, fallback = c(0, 1)))
}

.evBinomialProportionAxis <- function(settings) {
  requestedRange <- .evBinomialPriorRange(settings)
  breaks         <- .evPrettyPriorBreaks(requestedRange, isBinomial = TRUE)

  return(list(
    breaks = breaks,
    range  = range(breaks)
  ))
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
  designH0 <- .evContinuousDesignPriorForUnder(settings, "h0")
  designH1 <- .evContinuousDesignPriorForUnder(settings, "h1")
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

.evTestType <- function(test) {
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

.evTestLabel <- function(test) {
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

.evTargetLabel <- function(target) {
  if (target == "h1")
    return(gettext("H\u2081 (BF\u2081\u2080)"))

  return(gettext("H\u2080 (BF\u2080\u2081)"))
}

.evUnderLabel <- function(under) {
  if (under == "h1")
    return(gettext("Under H\u2081"))

  return(gettext("Under H\u2080"))
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
  if (.evUsesDirectionalZTest(settings))
    return(gettext("Directional"))

  if (!settings[["isBinomial"]])
    return(gettext("Point null"))

  if (settings[["nullPriorDistribution"]] == "direction")
    return(gettext("Directional"))

  return(gettext("Point null"))
}

.evNullPriorParameters <- function(settings) {
  if (.evUsesDirectionalZTest(settings)) {
    if (settings[["alternative"]] == "less")
      return(gettextf("\u03B8 >= \u03B8\u2080, \u03B8\u2080 = %1$s", .evFormatNumber(settings[["nullValue"]])))

    return(gettextf("\u03B8 <= \u03B8\u2080, \u03B8\u2080 = %1$s", .evFormatNumber(settings[["nullValue"]])))
  }

  if (!settings[["isBinomial"]])
    return(gettextf("\u03B8\u2080 = %1$s", .evFormatNumber(settings[["nullValue"]])))

  if (settings[["nullPriorDistribution"]] == "direction")
    return(gettextf("p <= p\u2080, p\u2080 = %1$s", .evFormatNumber(settings[["nullProportion"]])))

  return(gettextf("p\u2080 = %1$s", .evFormatNumber(settings[["nullProportion"]])))
}

.evUsesDirectionalZTest <- function(settings) {
  if (isTRUE(settings[["isBinomial"]]))
    return(FALSE)

  return(isTRUE(settings[["isDirectionalZTest"]]) || identical(settings[["analysisPriorDistribution"]], "directional"))
}

.evAnalysisPriorLabel <- function(settings) {
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

.evAnalysisPriorParameters <- function(settings) {
  if (settings[["isBinomial"]]) {
    return(gettextf(
      "a = %1$s, b = %2$s",
      .evFormatNumber(settings[["analysisPriorSuccesses"]]),
      .evFormatNumber(settings[["analysisPriorFailures"]])
    ))
  }

  if (settings[["isTTest"]]) {
    if (isTRUE(settings[["analysisPriorIsCauchy"]])) {
      return(gettextf(
        "location = %1$s, scale = %2$s",
        .evFormatNumber(settings[["tPriorLocation"]]),
        .evFormatNumber(settings[["tPriorScale"]])
      ))
    }

    return(gettextf(
      "location = %1$s, scale = %2$s, df = %3$s",
      .evFormatNumber(settings[["tPriorLocation"]]),
      .evFormatNumber(settings[["tPriorScale"]]),
      .evFormatNumber(settings[["tPriorDf"]])
    ))
  }

  if (settings[["analysisPriorDistribution"]] == "point") {
    return(gettextf(
      "location = %1$s",
      .evFormatNumber(settings[["analysisPriorMean"]])
    ))
  }

  if (settings[["analysisPriorDistribution"]] %in% c("normal", "directional")) {
    return(gettextf(
      "mean = %1$s, sd = %2$s",
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

.evDesignPriorLabel <- function(settings, under = "h1") {
  if (settings[["isBinomial"]])
    return(.evBinomialDesignPriorForUnder(settings, under)[["label"]])

  return(.evContinuousDesignPriorForUnder(settings, under)[["label"]])
}

.evDesignPriorParameters <- function(settings, under = "h1") {
  if (settings[["isBinomial"]])
    return(.evBinomialDesignPriorForUnder(settings, under)[["parameters"]])

  return(.evContinuousDesignPriorForUnder(settings, under)[["parameters"]])
}

.evNullPriorString <- function(settings) {
  if (!settings[["isBinomial"]] && !.evUsesDirectionalZTest(settings)) {
    return(.evPriorString(
      gettext("Point"),
      gettextf("location = %1$s", .evFormatNumber(settings[["nullValue"]]))
    ))
  }

  if (settings[["isBinomial"]] && settings[["nullPriorDistribution"]] != "direction") {
    return(.evPriorString(
      gettext("Point"),
      gettextf("p = %1$s", .evFormatNumber(settings[["nullProportion"]]))
    ))
  }

  return(.evPriorString(.evNullPriorLabel(settings), .evNullPriorParameters(settings)))
}

.evAnalysisPriorString <- function(settings) {
  return(.evPriorString(.evAnalysisPriorTableLabel(settings), .evAnalysisPriorParameters(settings)))
}

.evAnalysisPriorTableLabel <- function(settings) {
  label <- .evAnalysisPriorLabel(settings)

  if (identical(settings[["alternative"]], "greater"))
    return(paste0(label, "\u208A"))

  if (identical(settings[["alternative"]], "less"))
    return(paste0(label, "\u208B"))

  return(label)
}

.evDesignPriorString <- function(settings, under = "h1") {
  return(.evPriorString(.evDesignPriorLabel(settings, under), .evDesignPriorParameters(settings, under)))
}

.evPriorString <- function(distribution, parameters) {
  return(paste0(distribution, "(", parameters, ")"))
}

.evDesignPriorPlotLabel <- function(under) {
  if (under == "h0")
    return(gettext("Design Prior Under H\u2080"))

  return(gettext("Design Prior Under H\u2081"))
}

.evAnalysisPriorPlotLabel <- function(under) {
  if (under == "h0")
    return(gettext("Analysis Prior Under H\u2080"))

  return(gettext("Analysis Prior Under H\u2081"))
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

.evCodeHtml <- function(code) {
  paste0("<pre><code>", .evEscapeHtml(code), "</code></pre>")
}

.evEscapeHtml <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)

  return(text)
}

.evFormatRCall <- function(functionName, args) {
  argNames <- names(args)
  values   <- vapply(args, .evFormatRValue, character(1))
  width    <- max(nchar(argNames))
  lines    <- paste0("  ", sprintf(paste0("%-", width, "s"), argNames), " = ", values)

  return(paste0(
    functionName,
    "(\n",
    paste(lines, collapse = ",\n"),
    "\n)"
  ))
}

.evFormatRValue <- function(x) {
  if (is.null(x))
    return("NULL")

  if (length(x) != 1) {
    values <- vapply(as.list(x), .evFormatRValue, character(1))
    return(paste0("c(", paste(values, collapse = ", "), ")"))
  }

  if (is.character(x))
    return(encodeString(x, quote = "\""))

  if (is.logical(x))
    return(if (isTRUE(x)) "TRUE" else "FALSE")

  if (is.numeric(x)) {
    if (is.na(x))
      return(if (is.integer(x)) "NA_integer_" else "NA_real_")

    return(format(signif(x, 12), scientific = FALSE, trim = TRUE))
  }

  return(encodeString(as.character(x), quote = "\""))
}

.evClampProbability <- function(x) {
  if (!is.finite(x))
    return(NA_real_)

  return(max(0, min(1, x)))
}

.evFormatNumber <- function(x) {
  format(signif(x, 4), trim = TRUE)
}
