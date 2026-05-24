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

.bfdThreshold <- function(settings, target) {
  switch(
    target,
    "h1" = settings[["bf10Threshold"]],
    "h0" = settings[["bf01Threshold"]],
    stop(gettextf("Invalid target '%1$s'.", target))
  )
}

.bfdEventK <- function(settings, target) {
  switch(
    target,
    "h1" = 1 / settings[["bf10Threshold"]],
    "h0" = settings[["bf01Threshold"]],
    stop(gettextf("Invalid target '%1$s'.", target))
  )
}

.bfdLowerTail <- function(target) {
  return(target == "h1")
}

.bfdTargetPower <- function(settings, target) {
  switch(
    target,
    "h1" = settings[["targetPowerH1"]],
    "h0" = settings[["targetPowerH0"]],
    stop(gettextf("Invalid target '%1$s'.", target))
  )
}

.bfdValidateTargetPowers <- function(settings) {
  for (target in c("h1", "h0")) {
    power <- .bfdTargetPower(settings, target)
    if (!is.finite(power) || power <= 0 || power >= 1)
      stop(gettext("Conclusive evidence targets must be between 0 and 1."))
  }
}

.bfdContinuousAnalysisPriorDistribution <- function(options) {
  distribution <- options[["analysisPriorDistribution"]]
  if (length(distribution) != 1 || is.na(distribution) || distribution == "")
    stop(gettext("Analysis prior distribution option is invalid."))

  return(distribution)
}

.bfdAddContinuousZAnalysisPrior <- function(settings, options, includeRelativeMean = FALSE, includeDirectional = FALSE) {
  settings[["analysisPriorIsCauchy"]] <- FALSE
  distribution <- .bfdContinuousAnalysisPriorDistribution(options)

  settings[["analysisPriorDistribution"]] <- distribution
  settings[["analysisPriorMean"]]         <- if (distribution == "point") options[["analysisPriorLocation"]] else options[["analysisPriorMean"]]
  settings[["analysisPriorSd"]]           <- if (distribution == "point") 0 else options[["analysisPriorScale"]]
  if (isTRUE(includeRelativeMean))
    settings[["analysisPriorMeanRelative"]] <- settings[["analysisPriorMean"]] - settings[["nullValue"]]

  settings[["momentPriorSpread"]] <- if (distribution == "normalMomentMode") {
    options[["analysisPriorMode"]] / sqrt(2)
  } else {
    options[["analysisPriorSpread"]]
  }
  settings[["momentPriorMode"]] <- sqrt(2) * settings[["momentPriorSpread"]]

  if (isTRUE(includeDirectional))
    settings[["isDirectionalZTest"]] <- .bfdDirectionalZAnalysis(settings)

  return(settings)
}

.bfdAddContinuousTAnalysisPrior <- function(settings, options, includeDirectional = FALSE) {
  distribution <- .bfdContinuousAnalysisPriorDistribution(options)

  settings[["analysisPriorIsCauchy"]]     <- identical(distribution, "cauchy")
  settings[["analysisPriorDistribution"]] <- "t"
  settings[["tPriorLocation"]]            <- options[["tPriorLocation"]]
  settings[["tPriorScale"]]               <- options[["tPriorScale"]]
  settings[["tPriorDf"]]                  <- if (isTRUE(settings[["analysisPriorIsCauchy"]])) 1 else options[["tPriorDegreesOfFreedom"]]
  settings[["tPriorLocationRelative"]]    <- settings[["tPriorLocation"]] - settings[["nullValue"]]

  if (isTRUE(includeDirectional))
    settings[["isDirectionalZTest"]] <- FALSE

  return(settings)
}

.bfdDirectionalZAnalysis <- function(settings) {
  if (!settings[["isZTest"]])
    return(FALSE)

  if (!settings[["analysisPriorDistribution"]] %in% c("normal", "directional"))
    return(FALSE)

  return(settings[["alternative"]] != "two.sided" || settings[["analysisPriorDistribution"]] == "directional")
}

.bfdAddContinuousDesignPriors <- function(settings, options, includeRelativeToNull = FALSE) {
  designNullPrior <- options[["designNullPriorDistribution"]]
  designH0 <- list(
    distribution = designNullPrior,
    mean         = options[["designNullPriorMean"]],
    sd           = if (designNullPrior == "point") 0 else options[["designNullPriorStandardDeviation"]],
    label        = if (designNullPrior == "point") gettext("Point") else gettext("Normal"),
    parameters   = NULL
  )
  designH0[["parameters"]] <- .bfdContinuousDesignParameters(designH0)

  designPrior <- options[["designPriorDistribution"]]
  designH1 <- list(
    distribution = designPrior,
    mean         = options[["designPriorMean"]],
    sd           = if (designPrior == "point") 0 else options[["designPriorStandardDeviation"]],
    label        = if (designPrior == "point") gettext("Point") else gettext("Normal"),
    parameters   = NULL
  )
  designH1[["parameters"]] <- .bfdContinuousDesignParameters(designH1)

  settings[["designPriorUnderH0"]] <- designH0
  settings[["designPriorUnderH1"]] <- designH1
  settings[["designPrior"]]        <- designH1[["distribution"]]
  settings[["designPriorMean"]]    <- designH1[["mean"]]
  settings[["designPriorSd"]]      <- designH1[["sd"]]
  if (isTRUE(includeRelativeToNull)) {
    settings[["designPriorMeanRelative"]]     <- designH1[["mean"]] - settings[["nullValue"]]
    settings[["designNullPriorMeanRelative"]] <- designH0[["mean"]] - settings[["nullValue"]]
  }

  return(settings)
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

.bfdAddBinomialDesignPriors <- function(settings, options) {
  designH0 <- .bfdExplicitBinomialDesignPrior(settings, options, "h0")
  designH1 <- .bfdExplicitBinomialDesignPrior(settings, options, "h1")

  settings[["designPriorUnderH0"]] <- designH0
  settings[["designPriorUnderH1"]] <- designH1

  return(settings)
}

.bfdExplicitBinomialDesignPrior <- function(settings, options, under) {
  if (under == "h0") {
    distribution <- options[["binomialDesignNullPriorDistribution"]]
    if (distribution == "point")
      return(.bfdBinomialPointDesignPrior(options[["designNullProportion"]]))

    return(.bfdBinomialBetaDesignPrior(
      a     = options[["designNullPriorSuccesses"]],
      b     = options[["designNullPriorFailures"]],
      lower = options[["designNullPriorLowerTruncation"]],
      upper = options[["designNullPriorUpperTruncation"]]
    ))
  }

  if (settings[["binomialDesignPrior"]] == "point")
    return(.bfdBinomialPointDesignPrior(settings[["designProportion"]]))

  return(.bfdBinomialBetaDesignPrior(
    a     = settings[["designPriorSuccesses"]],
    b     = settings[["designPriorFailures"]],
    lower = settings[["designPriorLower"]],
    upper = settings[["designPriorUpper"]]
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
