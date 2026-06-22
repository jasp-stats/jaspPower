.bfdApplyCurrentSettings <- function(computation, currentSettings, settingNames) {
  settings <- computation[["settings"]]
  for (name in settingNames)
    settings[name] <- currentSettings[name]

  computation[["settings"]] <- settings
  return(computation)
}

.bfdPriorPlotRequested <- function(options) {
  return(
    options[["designPriorDistributionFigure"]] ||
      options[["analysisPriorDistributionFigure"]]
  )
}

.bfdAnalysisPriorAlternative <- function(direction) {
  switch(direction,
    twoSided = "two.sided",
    less     = "less",
    greater  = "greater"
  )
}

.bfdSampleSizeBasisTarget <- function(basis, under) {
  switch(basis,
    eachDesignHypothesis = under,
    bothDesignHypotheses = NULL,
    alternativeHypothesis = "h1",
    nullHypothesis        = "h0"
  )
}

.bfdCommonSampleSizeBasisTarget <- function(basis) {
  switch(basis,
    eachDesignHypothesis = NULL,
    bothDesignHypotheses = NULL,
    alternativeHypothesis = "h1",
    nullHypothesis        = "h0"
  )
}

.bfdEventK <- function(settings, target) {
  switch(
    target,
    "h1" = 1 / settings[["conclusiveEvidenceThresholdH1"]],
    "h0" = settings[["conclusiveEvidenceThresholdH0"]],
    stop(gettextf("Invalid target '%1$s'.", target))
  )
}

.bfdLowerTail <- function(target) {
  return(target == "h1")
}

.bfdTargetPower <- function(settings, target) {
  switch(
    target,
    "h1" = settings[["probabilityOfConclusiveEvidenceUnderH1"]],
    "h0" = settings[["probabilityOfConclusiveEvidenceUnderH0"]],
    stop(gettextf("Invalid target '%1$s'.", target))
  )
}

.bfdTSearchRangeArgument <- function(settings) {
  if (settings[["tSearchRangeMode"]] != "custom")
    return("adaptive")

  return(c(settings[["tSearchRangeLower"]], settings[["tSearchRangeUpper"]]))
}

.bfdAnalysisPriorIsCauchy <- function(settings) {
  return(isTRUE(settings[["isTTest"]]) && identical(settings[["analysisPriorDistribution"]], "cauchy"))
}

.bfdZAnalysisPriorMean <- function(settings) {
  if (identical(settings[["analysisPriorDistribution"]], "point"))
    return(settings[["analysisPriorLocation"]])

  return(settings[["analysisPriorMean"]])
}

.bfdZAnalysisPriorSd <- function(settings) {
  if (identical(settings[["analysisPriorDistribution"]], "point"))
    return(0)

  return(settings[["analysisPriorScale"]])
}

.bfdZAnalysisPriorMeanRelative <- function(settings) {
  return(.bfdZAnalysisPriorMean(settings) - settings[["nullValue"]])
}

.bfdMomentPriorSpread <- function(settings) {
  if (identical(settings[["analysisPriorDistribution"]], "normalMomentMode"))
    return(settings[["analysisPriorMode"]] / sqrt(2))

  return(settings[["analysisPriorSpread"]])
}

.bfdMomentPriorMode <- function(settings) {
  return(sqrt(2) * .bfdMomentPriorSpread(settings))
}

.bfdTPriorDf <- function(settings) {
  if (.bfdAnalysisPriorIsCauchy(settings))
    return(1)

  return(settings[["tPriorDegreesOfFreedom"]])
}

.bfdTPriorLocationRelative <- function(settings) {
  return(settings[["tPriorLocation"]] - settings[["nullValue"]])
}

.bfdDirectionalZAnalysis <- function(settings) {
  if (!settings[["isZTest"]])
    return(FALSE)

  if (!settings[["analysisPriorDistribution"]] %in% c("normal", "directional"))
    return(FALSE)

  return(settings[["alternative"]] != "two.sided" || settings[["analysisPriorDistribution"]] == "directional")
}

.bfdContinuousDesignPrior <- function(settings, under) {
  if (identical(under, "h0")) {
    distribution <- settings[["designNullPriorDistribution"]]
    designPrior <- list(
      distribution = distribution,
      mean         = settings[["designNullPriorMean"]],
      sd           = if (distribution == "point") 0 else settings[["designNullPriorStandardDeviation"]],
      label        = if (distribution == "point") gettext("Point") else gettext("Normal"),
      parameters   = NULL
    )
  } else {
    distribution <- settings[["designPriorDistribution"]]
    designPrior <- list(
      distribution = distribution,
      mean         = settings[["designPriorMean"]],
      sd           = if (distribution == "point") 0 else settings[["designPriorStandardDeviation"]],
      label        = if (distribution == "point") gettext("Point") else gettext("Normal"),
      parameters   = NULL
    )
  }

  designPrior[["parameters"]] <- .bfdContinuousDesignParameters(designPrior)
  return(designPrior)
}

.bfdContinuousDesignPriorMeanRelative <- function(settings, under) {
  return(.bfdContinuousDesignPrior(settings, under)[["mean"]] - settings[["nullValue"]])
}

.bfdContinuousDesignParameters <- function(designPrior) {
  if (designPrior[["distribution"]] == "point")
    return(gettextf("location = %1$s", .bfdFormatNumber(designPrior[["mean"]])))

  return(gettextf(
    "mean = %1$s, sd = %2$s",
    .bfdFormatNumber(designPrior[["mean"]]),
    .bfdFormatNumber(designPrior[["sd"]])
  ))
}

.bfdBinomialDesignPrior <- function(settings, under) {
  if (under == "h0") {
    distribution <- settings[["binomialDesignNullPriorDistribution"]]
    if (distribution == "point")
      return(.bfdBinomialPointDesignPrior(settings[["designNullProportion"]]))

    return(.bfdBinomialBetaDesignPrior(
      a     = settings[["designNullPriorSuccesses"]],
      b     = settings[["designNullPriorFailures"]],
      lower = settings[["designNullPriorLowerTruncation"]],
      upper = settings[["designNullPriorUpperTruncation"]]
    ))
  }

  if (settings[["binomialDesignPriorDistribution"]] == "point")
    return(.bfdBinomialPointDesignPrior(settings[["designProportion"]]))

  return(.bfdBinomialBetaDesignPrior(
    a     = settings[["designPriorSuccesses"]],
    b     = settings[["designPriorFailures"]],
    lower = settings[["designPriorLowerTruncation"]],
    upper = settings[["designPriorUpperTruncation"]]
  ))
}

.bfdBinomialPointDesignPrior <- function(proportion, label = gettext("Point proportion"), parameters = NULL) {
  if (is.null(parameters))
    parameters <- gettextf("p = %1$s", .bfdFormatNumber(proportion))

  return(list(
    distribution = "point",
    proportion   = proportion,
    label        = label,
    parameters   = parameters
  ))
}

.bfdBinomialBetaDesignPrior <- function(a, b, lower, upper, label = gettext("Beta"), parameters = NULL) {
  if (is.null(parameters)) {
    parameters <- gettextf(
      "a = %1$s, b = %2$s, lower = %3$s, upper = %4$s",
      .bfdFormatNumber(a),
      .bfdFormatNumber(b),
      .bfdFormatNumber(lower),
      .bfdFormatNumber(upper)
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
