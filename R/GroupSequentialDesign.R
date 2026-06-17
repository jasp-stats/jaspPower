GroupSequentialDesign <- function(jaspResults, dataset, options) {
  settings <- try(.csdPrepareSettings(options), silent = TRUE)
  result   <- if (jaspBase::isTryError(settings)) {
    settings
  } else {
    try(.csdComputeResult(settings), silent = TRUE)
  }

  if (options[["designSummary"]]) {
    .csdSummaryTable(jaspResults, settings, result)
    .csdEffectScaleConversionsTable(jaspResults, settings, result)
  }

  if (options[["sampleSizeSummary"]])
    .csdPlanningDetailsTable(jaspResults, settings, result)

  if (options[["crossingProbabilitiesTable"]])
    .csdFinalCrossingProbabilitiesTable(jaspResults, settings, result)

  if (options[["stoppingBoundariesTable"]]) {
    .csdLookScheduleTable(jaspResults, settings, result)
    .csdBoundariesTable(jaspResults, settings, result)
  }

  if (options[["crossingProbabilitiesTable"]]) {
    .csdCumulativeCrossingProbabilitiesTable(jaspResults, settings, result)
    .csdStagewiseCrossingProbabilitiesTable(jaspResults, settings, result)
    .csdNonBindingTypeIErrorTable(jaspResults, settings, result)
  }

  if (options[["text"]])
    .csdText(jaspResults, settings, result)

  if (options[["boundariesPlot"]])
    .csdBoundariesPlot(jaspResults, settings, result)

  if (options[["crossingProbabilitiesPlot"]])
    .csdCrossingProbabilitiesPlot(jaspResults, settings, result)

  if (options[["generateRCode"]])
    .csdRCode(jaspResults, settings, result)

  return()
}

.csdDependencies <- c(
  "designType", "numberOfLooks", "timingMode", "timing",
  "alpha", "power", "sampleSizeMode", "fixedSampleSize",
  "effectScale", "effectSize", "naturalEffectSize", "effectStandardDeviation",
  "sampleSizeAllocationRatio", "binaryInputMode", "binaryEventRateGroup1",
  "binaryEventRateGroup2", "binaryBaselineEventRate", "binaryAlternativeEffect",
  "binaryEffectScale", "binaryNullRiskDifference", "binaryNullRiskRatio",
  "binaryNullOddsRatio", "hazardRatio", "nullHazardRatio",
  "survivalInformationMethod", "survivalControlHazard", "survivalDropoutHazard",
  "survivalAccrualDuration", "survivalStudyDuration", "survivalEntry",
  "survivalEntryGamma",
  "survivalAccrualRate", "survivalMinimumFollowup", "survivalGsSurvMethod",
  "upperBoundary", "upperBoundaryParameter", "lowerBoundary",
  "lowerBoundaryParameter", "gridPoints"
)

.csdBoundariesPlotDependencies <- c(.csdDependencies, "boundariesPlot", "legendPosition", "colorPalette")
.csdCrossingProbabilitiesPlotDependencies <- c(.csdDependencies, "crossingProbabilitiesPlot", "legendPosition", "colorPalette")

.csdPrepareSettings <- function(options) {
  settings <- as.list(options)
  settings[["testType"]] <- .csdTestType(settings[["designType"]])
  settings[["timing"]] <- .csdTiming(settings, settings[["timing"]])

  settings <- .csdResolveInformationScale(settings)

  return(settings)
}

.csdResolveInformationScale <- function(settings) {
  if (settings[["sampleSizeMode"]] == "effectSize") {
    return(.csdResolveEffectScale(settings))
  } else if (settings[["sampleSizeMode"]] == "generic") {
    settings[["delta"]] <- 0
    settings[["nFix"]]  <- 1
  } else if (settings[["sampleSizeMode"]] == "fixedDesign") {
    settings[["delta"]] <- 0
    settings[["nFix"]]  <- settings[["fixedSampleSize"]]
  }

  return(settings)
}

.csdResolveEffectScale <- function(settings) {
  if (settings[["effectScale"]] %in% c("independentSamplesSmd", "meanDifferenceIndependentSamples")) {
    settings[["delta"]] <- 0
    settings[["nFix"]]  <- do.call(gsDesign::nNormal, .csdNormalFixedInformationArgs(settings))
    return(settings)
  }

  if (settings[["effectScale"]] == "twoSampleBinary") {
    settings[["delta"]] <- 0
    settings[["nFix"]]  <- do.call(gsDesign::nBinomial, .csdBinomialFixedInformationArgs(settings))
    return(settings)
  }

  if (settings[["effectScale"]] == "survivalHazardRatio") {
    settings[["delta"]] <- 0

    if (settings[["survivalInformationMethod"]] == "gsSurv") {
      settings[["nFix"]]      <- 1
      settings[["usesGsSurv"]] <- TRUE
      return(settings)
    }

    if (settings[["survivalInformationMethod"]] == "nSurvival") {
      fixed <- do.call(gsDesign::nSurvival, .csdNSurvivalFixedInformationArgs(settings))
      settings[["nFix"]]     <- fixed[["nEvents"]]
      settings[["nFixSurv"]] <- fixed[["n"]]
      return(settings)
    }

    settings[["nFix"]] <- do.call(gsDesign::nEvents, .csdNEventsFixedInformationArgs(settings))
    return(settings)
  }

  if (!(settings[["effectScale"]] %in% c("canonicalDelta", "oneSamplePairedSmd", "meanDifferenceOneSamplePaired")))
    return(settings)

  settings[["delta"]] <- if (settings[["effectScale"]] == "meanDifferenceOneSamplePaired") {
    settings[["naturalEffectSize"]] / settings[["effectStandardDeviation"]]
  } else {
    settings[["effectSize"]]
  }
  settings[["nFix"]] <- 1

  return(settings)
}

.csdNormalFixedInformationArgs <- function(settings) {
  helper <- .csdFixedDesignHelperSettings(settings)
  delta1 <- if (settings[["effectScale"]] == "independentSamplesSmd") settings[["effectSize"]] else settings[["naturalEffectSize"]]
  sd     <- if (settings[["effectScale"]] == "independentSamplesSmd") 1 else settings[["effectStandardDeviation"]]

  list(
    delta1 = delta1,
    sd     = sd,
    alpha  = helper[["alpha"]],
    beta   = 1 - settings[["power"]],
    ratio  = settings[["sampleSizeAllocationRatio"]],
    sided  = helper[["sided"]]
  )
}

.csdBinomialFixedInformationArgs <- function(settings) {
  helper <- .csdFixedDesignHelperSettings(settings)
  rates  <- .csdBinomialEventRates(settings)

  list(
    p1     = rates[["p1"]],
    p2     = rates[["p2"]],
    alpha  = helper[["alpha"]],
    beta   = 1 - settings[["power"]],
    delta0 = switch(
      settings[["binaryEffectScale"]],
      riskDifference = settings[["binaryNullRiskDifference"]],
      riskRatio      = log(settings[["binaryNullRiskRatio"]]),
      oddsRatio      = log(settings[["binaryNullOddsRatio"]])
    ),
    ratio  = settings[["sampleSizeAllocationRatio"]],
    sided  = helper[["sided"]],
    scale  = switch(
      settings[["binaryEffectScale"]],
      riskDifference = "Difference",
      riskRatio      = "RR",
      oddsRatio      = "OR"
    )
  )
}

.csdBinomialEventRates <- function(settings) {
  if (settings[["binaryInputMode"]] == "eventRates") {
    p1 <- settings[["binaryEventRateGroup1"]]
    p2 <- settings[["binaryEventRateGroup2"]]

    .csdValidateBinaryEventProbability(p1, gettext("group 1 event probability"))
    .csdValidateBinaryEventProbability(p2, gettext("group 2 event probability"))

    return(list(p1 = p1, p2 = p2))
  }

  p1 <- settings[["binaryBaselineEventRate"]]

  .csdValidateBinaryEventProbability(p1, gettext("baseline event probability"))

  effect <- settings[["binaryAlternativeEffect"]]
  if (!is.finite(effect))
    stop(gettext("The alternative binary effect must be finite."))

  if (settings[["binaryEffectScale"]] %in% c("riskRatio", "oddsRatio") && effect <= 0)
    stop(gettext("Alternative risk ratios and odds ratios must be greater than 0."))

  p2 <- switch(
    settings[["binaryEffectScale"]],
    riskDifference = p1 - effect,
    riskRatio      = p1 / effect,
    oddsRatio      = {
      odds2 <- (p1 / (1 - p1)) / effect
      odds2 / (1 + odds2)
    }
  )

  if (!is.finite(p2) || p2 <= 0 || p2 >= 1)
    stop(gettext("The baseline event probability and alternative binary effect imply a group 2 event probability outside (0, 1)."))

  list(p1 = p1, p2 = p2)
}

.csdValidateBinaryEventProbability <- function(value, label) {
  if (!is.finite(value) || value <= 0 || value >= 1)
    stop(gettextf("The %1$s must be greater than 0 and less than 1.", label))
}

.csdNEventsFixedInformationArgs <- function(settings) {
  helper <- .csdFixedDesignHelperSettings(settings)

  list(
    hr    = settings[["hazardRatio"]],
    alpha = helper[["alpha"]],
    beta  = 1 - settings[["power"]],
    ratio = settings[["sampleSizeAllocationRatio"]],
    sided = helper[["sided"]],
    hr0   = settings[["nullHazardRatio"]]
  )
}

.csdNSurvivalFixedInformationArgs <- function(settings) {
  helper <- .csdFixedDesignHelperSettings(settings)
  gamma  <- if (settings[["survivalEntry"]] == "expo") settings[["survivalEntryGamma"]] else NA_real_

  list(
    lambda1 = settings[["survivalControlHazard"]],
    lambda2 = settings[["survivalControlHazard"]] * settings[["hazardRatio"]],
    Ts      = settings[["survivalStudyDuration"]],
    Tr      = settings[["survivalAccrualDuration"]],
    eta     = settings[["survivalDropoutHazard"]],
    ratio   = settings[["sampleSizeAllocationRatio"]],
    alpha   = helper[["alpha"]],
    beta    = 1 - settings[["power"]],
    sided   = helper[["sided"]],
    approx  = FALSE,
    type    = "rr",
    entry   = settings[["survivalEntry"]],
    gamma   = gamma
  )
}

.csdFixedDesignHelperSettings <- function(settings) {
  if (settings[["testType"]] == 2)
    return(list(alpha = 2 * settings[["alpha"]], sided = 2L))

  return(list(alpha = settings[["alpha"]], sided = 1L))
}

.csdTestType <- function(designType) {
  switch(
    designType,
    oneSided                    = 1,
    twoSidedSymmetric           = 2,
    twoSidedAsymmetricBinding   = 3,
    twoSidedAsymmetricNonBinding = 4
  )
}

.csdDesignTypeLabel <- function(designType) {
  switch(
    designType,
    oneSided                    = gettext("One-sided"),
    twoSidedSymmetric           = gettext("Two-sided symmetric"),
    twoSidedAsymmetricBinding   = gettext("Two-sided asymmetric \u03B2-spending (binding futility)"),
    twoSidedAsymmetricNonBinding = gettext("Two-sided asymmetric \u03B2-spending (non-binding futility)")
  )
}

.csdEffectScaleLabel <- function(effectScale) {
  switch(
    effectScale,
    canonicalDelta                    = gettext("Canonical effect (\u03B4)"),
    oneSamplePairedSmd                = gettext("Standardized mean difference (one sample / paired)"),
    independentSamplesSmd             = gettext("Standardized mean difference (independent samples)"),
    meanDifferenceOneSamplePaired     = gettext("Mean difference (one sample / paired)"),
    meanDifferenceIndependentSamples  = gettext("Mean difference (independent samples)"),
    twoSampleBinary                   = gettext("Two-sample binary endpoint"),
    survivalHazardRatio               = gettext("Survival hazard ratio")
  )
}

.csdPlanningMetricLabel <- function(settings) {
  if (settings[["sampleSizeMode"]] == "generic")
    return(gettext("Information ratio"))

  if (settings[["sampleSizeMode"]] == "fixedDesign")
    return(gettext("Fixed design value"))

  if (settings[["sampleSizeMode"]] == "effectSize")
    return(.csdEffectScaleLabel(settings[["effectScale"]]))
}

.csdBoundaryLabel <- function(boundary, testType = 1) {
  if (testType %in% c(3, 4)) {
    return(switch(
      boundary,
      obrienFleming     = gettext("O'Brien-Fleming spending"),
      pocock            = gettext("Pocock spending"),
      hwangShihDeCani   = gettext("Hwang-Shih-DeCani"),
      kimDeMetsPower    = gettext("Kim-DeMets power")
    ))
  }

  switch(
    boundary,
    obrienFleming     = gettext("O'Brien-Fleming"),
    pocock            = gettext("Pocock"),
    wangTsiatis       = gettext("Wang-Tsiatis"),
    hwangShihDeCani   = gettext("Hwang-Shih-DeCani"),
    kimDeMetsPower    = gettext("Kim-DeMets power")
  )
}

.csdTiming <- function(settings, timingText) {
  if (settings[["timingMode"]] == "even")
    return(1)

  timing <- .csdParseNumericSchedule(timingText, gettext("information fraction schedule"))

  if (!(length(timing) %in% c(settings[["numberOfLooks"]] - 1, settings[["numberOfLooks"]])))
    stop(gettext("The information fraction schedule must contain K or K - 1 values."))

  if (length(timing) == settings[["numberOfLooks"]] && !isTRUE(all.equal(utils::tail(timing, 1), 1)))
    stop(gettext("When K information fractions are supplied, the final value must be 1."))

  if (length(timing) == settings[["numberOfLooks"]] - 1 && any(timing >= 1))
    stop(gettext("When K - 1 information fractions are supplied, all values must be less than 1."))

  if (any(timing <= 0) || any(timing > 1))
    stop(gettext("Information fractions must be greater than 0 and no larger than 1."))

  if (any(diff(timing) <= 0))
    stop(gettext("Information fractions must be strictly increasing."))

  return(timing)
}

.csdParseNumericSchedule <- function(text, label) {
  values <- as.character(text)
  values <- unlist(strsplit(values, "[,;[:space:]]+"))
  values <- values[nzchar(values)]
  parsed <- suppressWarnings(as.numeric(values))

  if (length(parsed) == 0 || any(!is.finite(parsed)))
    stop(gettextf("The %1$s must contain only finite numeric values.", label))

  return(parsed)
}

.csdComputeResult <- function(settings) {
  design <- if (.csdUsesGsSurv(settings)) {
    do.call(gsDesign::gsSurv, .csdGsSurvArgs(settings))
  } else {
    do.call(gsDesign::gsDesign, .csdDesignArgs(settings))
  }

  return(list(
    design = design
  ))
}

.csdDesignArgs <- function(settings) {
  args <- list(
    k         = settings[["numberOfLooks"]],
    test.type = settings[["testType"]],
    alpha     = settings[["alpha"]],
    beta      = 1 - settings[["power"]],
    delta     = settings[["delta"]],
    n.fix     = settings[["nFix"]],
    timing    = settings[["timing"]],
    r         = settings[["gridPoints"]],
    sfu       = .csdSpendingFunction(settings[["upperBoundary"]], settings[["testType"]])
  )

  if (.csdBoundaryUsesParameter(settings[["upperBoundary"]]))
    args[["sfupar"]] <- settings[["upperBoundaryParameter"]]

  if (.csdUsesGsDesignSurvivalEndpoint(settings)) {
    args[["endpoint"]] <- "TTE"
    args[["delta1"]]   <- log(settings[["hazardRatio"]])
    args[["delta0"]]   <- log(settings[["nullHazardRatio"]])

    if (!is.null(settings[["nFixSurv"]]))
      args[["nFixSurv"]] <- settings[["nFixSurv"]]
  }

  if (settings[["testType"]] %in% c(3, 4)) {
    args[["sfl"]] <- .csdSpendingFunction(settings[["lowerBoundary"]], settings[["testType"]])

    if (.csdBoundaryUsesParameter(settings[["lowerBoundary"]]))
      args[["sflpar"]] <- settings[["lowerBoundaryParameter"]]
  }

  return(args)
}

.csdGsSurvArgs <- function(settings) {
  helper <- .csdFixedDesignHelperSettings(settings)
  args   <- list(
    k         = settings[["numberOfLooks"]],
    test.type = settings[["testType"]],
    alpha     = helper[["alpha"]],
    sided     = helper[["sided"]],
    beta      = 1 - settings[["power"]],
    timing    = settings[["timing"]],
    sfu       = .csdSpendingFunction(settings[["upperBoundary"]], settings[["testType"]]),
    r         = settings[["gridPoints"]],
    lambdaC   = settings[["survivalControlHazard"]],
    hr        = settings[["hazardRatio"]],
    hr0       = settings[["nullHazardRatio"]],
    eta       = settings[["survivalDropoutHazard"]],
    gamma     = settings[["survivalAccrualRate"]],
    R         = settings[["survivalAccrualDuration"]],
    T         = settings[["survivalStudyDuration"]],
    minfup    = settings[["survivalMinimumFollowup"]],
    ratio     = settings[["sampleSizeAllocationRatio"]],
    method    = settings[["survivalGsSurvMethod"]]
  )

  if (.csdBoundaryUsesParameter(settings[["upperBoundary"]]))
    args[["sfupar"]] <- settings[["upperBoundaryParameter"]]

  if (settings[["testType"]] %in% c(3, 4)) {
    args[["sfl"]] <- .csdSpendingFunction(settings[["lowerBoundary"]], settings[["testType"]])

    if (.csdBoundaryUsesParameter(settings[["lowerBoundary"]]))
      args[["sflpar"]] <- settings[["lowerBoundaryParameter"]]
  }

  return(args)
}

.csdUsesGsSurv <- function(settings) {
  .csdUsesSurvivalEffect(settings) &&
    identical(settings[["survivalInformationMethod"]], "gsSurv")
}

.csdUsesGsDesignSurvivalEndpoint <- function(settings) {
  .csdUsesSurvivalEffect(settings) &&
    !.csdUsesGsSurv(settings)
}

.csdUsesSurvivalEffect <- function(settings) {
  identical(settings[["sampleSizeMode"]], "effectSize") &&
    identical(settings[["effectScale"]], "survivalHazardRatio")
}

.csdBoundaryUsesParameter <- function(boundary) {
  boundary %in% c("wangTsiatis", "hwangShihDeCani", "kimDeMetsPower")
}

.csdSpendingFunction <- function(boundary, testType) {
  if (testType %in% c(3, 4)) {
    return(switch(
      boundary,
      obrienFleming   = gsDesign::sfLDOF,
      pocock          = gsDesign::sfLDPocock,
      hwangShihDeCani = gsDesign::sfHSD,
      kimDeMetsPower  = gsDesign::sfPower
    ))
  }

  switch(
    boundary,
    obrienFleming   = "OF",
    pocock          = "Pocock",
    wangTsiatis     = "WT",
    hwangShihDeCani = gsDesign::sfHSD,
    kimDeMetsPower  = gsDesign::sfPower
  )
}

.csdSpendingFunctionRValue <- function(boundary, testType) {
  if (testType %in% c(3, 4)) {
    return(switch(
      boundary,
      obrienFleming   = "gsDesign::sfLDOF",
      pocock          = "gsDesign::sfLDPocock",
      hwangShihDeCani = "gsDesign::sfHSD",
      kimDeMetsPower  = "gsDesign::sfPower"
    ))
  }

  switch(
    boundary,
    obrienFleming   = "\"OF\"",
    pocock          = "\"Pocock\"",
    wangTsiatis     = "\"WT\"",
    hwangShihDeCani = "gsDesign::sfHSD",
    kimDeMetsPower  = "gsDesign::sfPower"
  )
}

.csdSummaryTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialSummary"]]))
    return()

  table <- createJaspTable(title = gettext("Group Sequential Design"))
  table$dependOn(c(.csdDependencies, "designSummary"))
  table$position <- 1
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialSummary"]] <- table

  table$addColumnInfo(name = "design", title = gettext("Design"), type = "string")
  table$addColumnInfo(name = "looks", title = gettext("Looks"), type = "integer")
  table$addColumnInfo(name = "alpha", title = gettext("\u03B1"), type = "number")
  table$addColumnInfo(name = "power", title = gettext("Power (1 - \u03B2)"), type = "number")
  table$addColumnInfo(name = "planningMetric", title = gettext("Planning Metric"), type = "string")

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute classical group sequential design: %1$s", .bfdCleanError(result)))
    return()
  }

  informationLabels <- .csdInformationLabels(settings)

  table$addColumnInfo(name = "fixedN", title = informationLabels[["fixed"]], type = "number")
  table$addColumnInfo(name = "maximumN", title = informationLabels[["maximum"]], type = "number")
  table$addColumnInfo(name = "expectedH0", title = gettext("Under H\u2080"), type = "number", overtitle = informationLabels[["expected"]])
  table$addColumnInfo(name = "expectedH1", title = gettext("Under H\u2081"), type = "number", overtitle = informationLabels[["expected"]])
  table$addColumnInfo(name = "inflation", title = gettext("Inflation Ratio"), type = "number")

  table$setData(.csdSummaryRow(settings, result[["design"]]))
  table$addFootnote(gettext("\u03B1 is the one-sided Type I error rate. For symmetric two-sided designs, the total Type I error rate is 2\u03B1."))
  table$addFootnote(.csdSampleSizeFootnote(settings))
}

.csdSummaryRow <- function(settings, design) {
  maximumN <- max(design[["n.I"]], na.rm = TRUE)
  fixedN   <- design[["n.fix"]]

  data.frame(
    design         = .csdDesignTypeLabel(settings[["designType"]]),
    looks          = design[["k"]],
    alpha          = settings[["alpha"]],
    power          = settings[["power"]],
    planningMetric = .csdPlanningMetricLabel(settings),
    fixedN         = fixedN,
    maximumN       = maximumN,
    expectedH0      = .csdExpectedN(design, 0),
    expectedH1      = .csdExpectedN(design, design[["delta"]]),
    inflation      = maximumN / fixedN,
    stringsAsFactors = FALSE
  )
}

.csdPlanningDetailsTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialPlanningDetails"]]))
    return()

  if (jaspBase::isTryError(settings) || !.csdHasPlanningDetails(settings))
    return()

  rows <- NULL
  if (!jaspBase::isTryError(result)) {
    rows <- .csdPlanningDetailRows(settings, result[["design"]])
    if (is.null(rows) || nrow(rows) == 0)
      return()
  }

  table <- createJaspTable(title = gettext("Sample Size / Event Details"))
  table$dependOn(c(.csdDependencies, "sampleSizeSummary"))
  table$position <- 2
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialPlanningDetails"]] <- table

  table$addColumnInfo(name = "quantity", title = gettext("Quantity"), type = "string")
  table$addColumnInfo(name = "fixedDesign", title = gettext("Fixed Design"), type = "number")
  table$addColumnInfo(name = "maximum", title = gettext("Maximum / Final"), type = "number")

  if (.csdPlanningDetailsIncludeExpected(rows)) {
    table$addColumnInfo(name = "expectedH0", title = gettext("Under H\u2080"), type = "number", overtitle = gettext("Expected"))
    table$addColumnInfo(name = "expectedH1", title = gettext("Under H\u2081"), type = "number", overtitle = gettext("Expected"))
  }

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute sample size / event details: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(rows)
  .csdAddPlanningDetailFootnotes(table, settings)
}

.csdHasPlanningDetails <- function(settings) {
  if (settings[["sampleSizeMode"]] != "effectSize")
    return(FALSE)

  if (.csdUsesTwoGroupSampleSizeDetails(settings))
    return(TRUE)

  .csdUsesSurvivalPlanningDetails(settings)
}

.csdUsesTwoGroupSampleSizeDetails <- function(settings) {
  settings[["effectScale"]] %in% c("independentSamplesSmd", "meanDifferenceIndependentSamples", "twoSampleBinary")
}

.csdUsesSurvivalPlanningDetails <- function(settings) {
  settings[["effectScale"]] == "survivalHazardRatio" &&
    settings[["survivalInformationMethod"]] %in% c("nSurvival", "gsSurv")
}

.csdPlanningDetailRows <- function(settings, design) {
  rows <- list()

  if (.csdUsesTwoGroupSampleSizeDetails(settings))
    rows[[length(rows) + 1]] <- .csdGroupSampleSizeDetailRows(settings, design)

  if (.csdUsesSurvivalPlanningDetails(settings)) {
    fixedSurvivalDesign <- .csdFixedSurvivalPlanningDesign(settings, design)
    subjectsRow <- .csdSurvivalSubjectsPlanningDetailRow(design, fixedSurvivalDesign)
    if (!is.null(subjectsRow))
      rows[[length(rows) + 1]] <- subjectsRow

    analysisTimeRow <- .csdAnalysisTimePlanningDetailRow(settings, design, fixedSurvivalDesign)
    if (!is.null(analysisTimeRow))
      rows[[length(rows) + 1]] <- analysisTimeRow
  }

  if (length(rows) == 0)
    return(NULL)

  out <- do.call(rbind, rows)
  row.names(out) <- NULL

  return(out)
}

.csdGroupSampleSizeDetailRows <- function(settings, design) {
  fractions <- .csdAllocationFractions(settings[["sampleSizeAllocationRatio"]])
  totals    <- c(
    fixedDesign = design[["n.fix"]],
    maximum     = max(design[["n.I"]], na.rm = TRUE),
    expectedH0  = .csdExpectedN(design, 0),
    expectedH1  = .csdExpectedN(design, design[["delta"]])
  )

  data.frame(
    quantity    = c(gettext("Group 1 sample size"), gettext("Group 2 sample size")),
    fixedDesign = totals[["fixedDesign"]] * fractions,
    maximum     = totals[["maximum"]] * fractions,
    expectedH0  = totals[["expectedH0"]] * fractions,
    expectedH1  = totals[["expectedH1"]] * fractions,
    stringsAsFactors = FALSE
  )
}

.csdAllocationFractions <- function(ratio) {
  if (!is.finite(ratio) || ratio <= 0)
    return(c(group1 = NA_real_, group2 = NA_real_))

  c(group1 = 1 / (1 + ratio), group2 = ratio / (1 + ratio))
}

.csdSurvivalSubjectsPlanningDetailRow <- function(design, fixedSurvivalDesign = NULL) {
  fixedSubjects   <- .csdFixedSurvivalSubjects(design, fixedSurvivalDesign)
  maximumSubjects <- .csdMaximumSurvivalSubjects(design)

  if (!is.finite(fixedSubjects) && !is.finite(maximumSubjects))
    return(NULL)

  data.frame(
    quantity    = gettext("Enrolled subjects"),
    fixedDesign = fixedSubjects,
    maximum     = maximumSubjects,
    expectedH0  = NA_real_,
    expectedH1  = NA_real_,
    stringsAsFactors = FALSE
  )
}

.csdAnalysisTimePlanningDetailRow <- function(settings, design, fixedSurvivalDesign = NULL) {
  if (!.csdHasGsSurvCalendar(design))
    return(NULL)

  analysisTimes <- .csdVectorOrNA(design[["T"]], design[["k"]])

  data.frame(
    quantity    = gettext("Expected analysis time"),
    fixedDesign = .csdFixedSurvivalAnalysisTime(settings, fixedSurvivalDesign),
    maximum     = utils::tail(analysisTimes, 1),
    expectedH0  = NA_real_,
    expectedH1  = NA_real_,
    stringsAsFactors = FALSE
  )
}

.csdFixedSurvivalPlanningDesign <- function(settings, design) {
  if (!inherits(design, "gsSurv"))
    return(NULL)

  helper <- .csdFixedDesignHelperSettings(settings)
  fixed  <- try(gsDesign::nSurv(
    lambdaC = settings[["survivalControlHazard"]],
    hr      = settings[["hazardRatio"]],
    hr0     = settings[["nullHazardRatio"]],
    eta     = settings[["survivalDropoutHazard"]],
    gamma   = settings[["survivalAccrualRate"]],
    R       = settings[["survivalAccrualDuration"]],
    T       = settings[["survivalStudyDuration"]],
    minfup  = settings[["survivalMinimumFollowup"]],
    ratio   = settings[["sampleSizeAllocationRatio"]],
    alpha   = helper[["alpha"]],
    beta    = 1 - settings[["power"]],
    sided   = helper[["sided"]],
    method  = settings[["survivalGsSurvMethod"]]
  ), silent = TRUE)

  if (jaspBase::isTryError(fixed))
    return(NULL)

  return(fixed)
}

.csdFixedSurvivalAnalysisTime <- function(settings, fixedSurvivalDesign = NULL) {
  value <- .csdPositiveFirst(fixedSurvivalDesign[["T"]])
  if (is.finite(value))
    return(value)

  .csdPositiveFirst(settings[["survivalStudyDuration"]])
}

.csdPlanningDetailsIncludeExpected <- function(rows) {
  if (is.null(rows))
    return(FALSE)

  any(is.finite(rows[["expectedH0"]])) || any(is.finite(rows[["expectedH1"]]))
}

.csdAddPlanningDetailFootnotes <- function(table, settings) {
  if (.csdUsesTwoGroupSampleSizeDetails(settings)) {
    table$addFootnote(gettext("Group sample sizes are split from the total sample size using the allocation ratio N\u2082/N\u2081. Expected values are probability-weighted expected total sample sizes split by the same ratio."))
  }

  if (.csdUsesSurvivalPlanningDetails(settings)) {
    table$addFootnote(gettext("Survival subject and analysis-time quantities are derived from the accrual assumptions. Fixed-design values use the corresponding fixed-design survival calculation; maximum/final values use the group sequential design. Event information is summarized in the main design table and look schedule."))
  }
}

.csdEffectScaleConversionsTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialEffectScaleConversions"]]))
    return()

  if (jaspBase::isTryError(settings) || jaspBase::isTryError(result))
    return()

  rows <- .csdEffectScaleConversionRows(settings, result[["design"]])
  if (is.null(rows) || nrow(rows) == 0)
    return()

  table <- createJaspTable(title = gettext("Effect Scale Conversions"))
  table$dependOn(c(.csdDependencies, "designSummary"))
  table$position <- 4
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialEffectScaleConversions"]] <- table

  table$addColumnInfo(name = "quantity", title = gettext("Quantity"), type = "string")
  table$addColumnInfo(name = "value", title = gettext("Value"), type = "string")
  table$addColumnInfo(name = "description", title = gettext("Description"), type = "string")

  table$setData(rows)
}

.csdEffectScaleConversionRows <- function(settings, design) {
  if (settings[["sampleSizeMode"]] != "effectSize" || settings[["effectScale"]] == "canonicalDelta")
    return(NULL)

  rows <- list(.csdEffectScaleConversionRow(
    gettext("Canonical \u03B4 under H\u2081"),
    design[["delta"]],
    .csdCanonicalDeltaConversionDescription(settings)
  ))

  if (settings[["sampleSizeMode"]] == "effectSize" && settings[["effectScale"]] == "meanDifferenceOneSamplePaired") {
    rows[[length(rows) + 1]] <- .csdEffectScaleConversionRow(
      gettext("Standardized mean difference"),
      settings[["naturalEffectSize"]] / settings[["effectStandardDeviation"]],
      gettext("Mean difference divided by the outcome SD or paired-difference SD.")
    )
  }

  if (settings[["sampleSizeMode"]] == "effectSize" && settings[["effectScale"]] == "meanDifferenceIndependentSamples") {
    rows[[length(rows) + 1]] <- .csdEffectScaleConversionRow(
      gettext("Standardized mean difference"),
      settings[["naturalEffectSize"]] / settings[["effectStandardDeviation"]],
      gettext("Mean difference divided by the common outcome SD.")
    )
  }

  if (settings[["sampleSizeMode"]] == "effectSize" && settings[["effectScale"]] == "twoSampleBinary") {
    rates <- .csdBinomialEventRates(settings)

    if (settings[["binaryInputMode"]] == "baselineEffect") {
      rows[[length(rows) + 1]] <- .csdEffectScaleConversionRow(
        .csdBinaryAlternativeEffectLabel(settings),
        settings[["binaryAlternativeEffect"]],
        gettext("Alternative effect used to derive the group 2 event probability.")
      )
    }

    rows[[length(rows) + 1]] <- .csdEffectScaleConversionRow(
      gettext("Group 1 event probability"),
      rates[["p1"]],
      if (settings[["binaryInputMode"]] == "baselineEffect")
        gettext("Baseline event probability under the alternative hypothesis.")
      else
        gettext("Event probability in group 1 under the alternative hypothesis.")
    )
    rows[[length(rows) + 1]] <- .csdEffectScaleConversionRow(
      gettext("Group 2 event probability"),
      rates[["p2"]],
      if (settings[["binaryInputMode"]] == "baselineEffect")
        gettext("Derived from the baseline event probability and selected alternative binary effect.")
      else
        gettext("Event probability in group 2 under the alternative hypothesis.")
    )
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL

  return(out)
}

.csdBinaryAlternativeEffectLabel <- function(settings) {
  switch(
    settings[["binaryEffectScale"]],
    riskDifference = gettext("Alternative risk difference"),
    riskRatio      = gettext("Alternative risk ratio"),
    oddsRatio      = gettext("Alternative odds ratio")
  )
}

.csdCanonicalDeltaConversionDescription <- function(settings) {
  switch(
    settings[["effectScale"]],
    canonicalDelta                   = gettext("Information-scale effect used by the sequential calculations."),
    oneSamplePairedSmd               = gettext("The one-sample/paired standardized mean difference is used directly as the information-scale effect."),
    independentSamplesSmd            = gettext("Derived from the fixed-design total sample size computed from the independent-samples standardized mean difference and allocation ratio."),
    meanDifferenceOneSamplePaired    = gettext("The raw mean difference is divided by the relevant SD and used as the information-scale effect."),
    meanDifferenceIndependentSamples = gettext("Derived from the fixed-design total sample size computed from the mean difference, common SD, and allocation ratio."),
    twoSampleBinary                  = gettext("Derived from the fixed-design total sample size computed from the entered event probabilities or from the baseline event probability and alternative binary effect."),
    survivalHazardRatio              = gettext("Derived from the event information implied by the hazard-ratio alternative, null hazard ratio, allocation ratio, and selected survival information method.")
  )
}

.csdEffectScaleConversionRow <- function(quantity, value, description) {
  data.frame(
    quantity    = quantity,
    value       = .csdSpecificationValue(value),
    description = description,
    stringsAsFactors = FALSE
  )
}

.csdSpecificationValue <- function(value) {
  if (is.numeric(value))
    return(.csdRNumber(value))

  if (is.logical(value))
    return(if (value) gettext("Yes") else gettext("No"))

  return(as.character(value))
}

.csdInformationLabels <- function(settings) {
  if (settings[["sampleSizeMode"]] == "generic") {
    return(list(
      fixed          = gettext("Fixed Information Ratio"),
      maximum        = gettext("Maximum Information Ratio"),
      expected       = gettext("Expected Information Ratio"),
      lookInformation = gettext("Information Ratio"),
      text           = gettext("information ratio")
    ))
  }

  if (settings[["sampleSizeMode"]] == "fixedDesign") {
    return(list(
      fixed          = gettext("Fixed Design Value"),
      maximum        = gettext("Maximum Value"),
      expected       = gettext("Expected Value"),
      lookInformation = gettext("Planned Value"),
      text           = gettext("fixed-design value")
    ))
  }

  if (settings[["effectScale"]] %in% c("independentSamplesSmd", "meanDifferenceIndependentSamples", "twoSampleBinary")) {
    return(list(
      fixed          = gettext("Fixed Design Total Sample Size"),
      maximum        = gettext("Maximum Total Sample Size"),
      expected       = gettext("Expected Total Sample Size"),
      lookInformation = gettext("Total Sample Size"),
      text           = gettext("total sample size")
    ))
  }

  if (settings[["effectScale"]] == "survivalHazardRatio") {
    return(list(
      fixed          = gettext("Fixed Design Events"),
      maximum        = gettext("Maximum Events"),
      expected       = gettext("Expected Events"),
      lookInformation = gettext("Events"),
      text           = gettext("events")
    ))
  }

  return(list(
    fixed          = gettext("Fixed Design Information Units"),
    maximum        = gettext("Maximum Information Units"),
    expected       = gettext("Expected Information Units"),
    lookInformation = gettext("Information Units"),
    text           = gettext("information units")
  ))
}

.csdFixedSurvivalSubjects <- function(design, fixedSurvivalDesign = NULL) {
  if (inherits(design, "gsSurv"))
    return(.csdPositiveFirst(fixedSurvivalDesign[["n"]]))

  .csdPositiveFirst(design[["nFixSurv"]])
}

.csdMaximumSurvivalSubjects <- function(design) {
  if (inherits(design, "gsSurv"))
    return(.csdFinalGsSurvSubjects(design))

  value <- design[["nSurv"]]

  if (is.null(value) || length(value) == 0 || !is.finite(value[1]) || value[1] <= 0)
    return(NA_real_)

  return(as.numeric(value[1]))
}

.csdFinalGsSurvSubjects <- function(design) {
  subjects <- .csdGsSurvSubjectsByLook(design)

  if (length(subjects) == 0)
    return(NA_real_)

  return(utils::tail(subjects, 1))
}

.csdPositiveFirst <- function(value) {
  if (is.null(value) || length(value) == 0 || !is.finite(value[1]) || value[1] <= 0)
    return(NA_real_)

  return(as.numeric(value[1]))
}

.csdDesignUsesLowerBound <- function(settings) {
  settings[["testType"]] != 1
}

.csdDesignUsesNonBindingFutility <- function(settings) {
  settings[["testType"]] == 4
}

.csdSampleSizeFootnote <- function(settings) {
  if (settings[["sampleSizeMode"]] == "generic")
    return(gettext("In information-ratio mode, fixed design information and maximum information are relative information values, not observations or events."))

  if (settings[["sampleSizeMode"]] == "fixedDesign")
    return(gettext("In fixed design value mode, the entered value defines the scale for the maximum and expected values. It may represent information units, total sample size, or events depending on the endpoint."))

  if (settings[["effectScale"]] == "canonicalDelta")
    return(gettext("Canonical \u03B4 is the information-scale effect; it is not generally Cohen's d. The table reports information units."))

  if (settings[["effectScale"]] == "oneSamplePairedSmd")
    return(gettext("For one-sample and paired normal designs, the standardized mean difference uses the outcome SD or the SD of paired differences. The table reports information units."))

  if (settings[["effectScale"]] %in% c("independentSamplesSmd", "meanDifferenceIndependentSamples"))
    return(gettext("For independent-samples normal designs, values labeled total sample size are total observations across both groups; the allocation ratio determines the group split."))

  if (settings[["effectScale"]] == "meanDifferenceOneSamplePaired")
    return(gettext("For one-sample and paired mean-difference designs, \u03B4 is the mean difference divided by the outcome SD or SD of paired differences. The table reports information units."))

  if (settings[["effectScale"]] == "twoSampleBinary")
    return(gettext("For two-sample binary endpoints, values labeled total sample size are total observations across both groups; the entered event probabilities or derived event probabilities and selected null margin define the endpoint."))

  if (settings[["effectScale"]] == "survivalHazardRatio")
    return(.csdSurvivalFootnote(settings))
}

.csdSurvivalFootnote <- function(settings) {
  if (settings[["survivalInformationMethod"]] == "events")
    return(gettext("For survival events-only mode, values labeled events are required event counts; this mode does not estimate enrolled subjects or analysis times."))

  if (settings[["survivalInformationMethod"]] == "nSurvival")
    return(gettext("For the subjects + events survival mode, values labeled events are event counts. Enrolled-subject rows are planned or expected participant counts under the accrual assumptions, not event counts."))

  if (settings[["survivalInformationMethod"]] == "gsSurv")
    return(gettext("For group sequential accrual survival mode, values labeled events are event information; expected analysis time is measured from trial start in the same time unit as the survival inputs."))
}

.csdUsesLanDeMetsBoundary <- function(settings) {
  if (!(settings[["testType"]] %in% c(3, 4)))
    return(FALSE)

  any(c(settings[["upperBoundary"]], settings[["lowerBoundary"]]) %in% c("obrienFleming", "pocock"))
}

.csdBoundaryBasisFootnote <- function(settings) {
  upper <- .csdBoundaryBasisLabel(
    .csdBoundaryLabel(settings[["upperBoundary"]], settings[["testType"]]),
    settings[["upperBoundary"]],
    settings[["upperBoundaryParameter"]]
  )

  if (settings[["testType"]] == 1) {
    return(gettextf(
      "Boundaries are based on the selected information fractions, \u03B1 and \u03B2 targets, and boundary family: upper = %1$s.",
      upper
    ))
  }

  if (settings[["testType"]] == 2) {
    return(gettextf(
      "Boundaries are based on the selected information fractions, \u03B1 and \u03B2 targets, and symmetric boundary family: upper/lower = %1$s.",
      upper
    ))
  }

  lower <- .csdBoundaryBasisLabel(
    .csdBoundaryLabel(settings[["lowerBoundary"]], settings[["testType"]]),
    settings[["lowerBoundary"]],
    settings[["lowerBoundaryParameter"]]
  )
  gettextf(
    "Boundaries are based on the selected information fractions, \u03B1 and \u03B2 targets, and boundary families: upper = %1$s; lower = %2$s.",
    upper,
    lower
  )
}

.csdBoundaryBasisLabel <- function(label, boundary, parameter) {
  if (!.csdBoundaryUsesParameter(boundary))
    return(label)

  gettextf("%1$s (parameter = %2$s)", label, .csdRNumber(parameter))
}

.csdBoundariesTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialBoundaries"]]))
    return()

  table <- createJaspTable(title = gettext("Stopping Boundaries"))
  table$dependOn(c(.csdDependencies, "stoppingBoundariesTable"))
  table$position <- 6
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialBoundaries"]] <- table

  includeLowerBound <- !jaspBase::isTryError(settings) && .csdDesignUsesLowerBound(settings)
  lowerOvertitle    <- if (includeLowerBound) .csdLowerBoundaryOvertitle(settings) else gettext("Lower Bound")
  upperOvertitle    <- if (!jaspBase::isTryError(settings)) .csdUpperBoundaryOvertitle(settings) else gettext("Upper Bound")

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  table$addColumnInfo(name = "timing", title = gettext("Information Fraction"), type = "number")
  table$addColumnInfo(name = "upperZ", title = "Z", type = "number", overtitle = upperOvertitle)
  table$addColumnInfo(name = "upperP", title = gettext("Upper-tail p"), type = "pvalue", overtitle = upperOvertitle)

  if (includeLowerBound) {
    table$addColumnInfo(name = "lowerZ", title = "Z", type = "number", overtitle = lowerOvertitle)
    table$addColumnInfo(name = "lowerP", title = gettext("Lower-tail p"), type = "pvalue", overtitle = lowerOvertitle)
  }

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stopping boundaries: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.csdBoundaryRows(result[["design"]], includeLowerBound = .csdDesignUsesLowerBound(settings)))
  table$addFootnote(gettext("Nominal p values are one-sided tail areas for crossing the displayed Z-boundary."))
  table$addFootnote(.csdBoundaryBasisFootnote(settings))

  if (settings[["testType"]] %in% c(3, 4))
    table$addFootnote(gettext("For asymmetric designs, the lower bound is a futility boundary."))

  if (.csdUsesLanDeMetsBoundary(settings))
    table$addFootnote(gettext("For asymmetric designs, O'Brien-Fleming and Pocock selections are implemented as Lan-DeMets spending functions."))
}

.csdLookScheduleTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialLookSchedule"]]))
    return()

  table <- createJaspTable(title = gettext("Look Schedule"))
  table$dependOn(c(.csdDependencies, "stoppingBoundariesTable"))
  table$position <- 5
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialLookSchedule"]] <- table

  informationTitle <- if (jaspBase::isTryError(result)) gettext("Information") else .csdInformationLabels(settings)[["lookInformation"]]

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")
  table$addColumnInfo(name = "timing", title = gettext("Information Fraction"), type = "number")
  table$addColumnInfo(name = "information", title = informationTitle, type = "number")

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute look schedule: %1$s", .bfdCleanError(result)))
    return()
  }

  includeSurvivalCalendar <- .csdHasGsSurvCalendar(result[["design"]])
  if (includeSurvivalCalendar) {
    table$addColumnInfo(name = "analysisTime", title = gettext("Expected Analysis Time"), type = "number")
    table$addColumnInfo(name = "expectedSubjects", title = gettext("Expected Enrolled Subjects"), type = "number")
    table$addColumnInfo(name = "expectedEvents", title = gettext("Expected Accrued Events"), type = "number")
  }

  table$setData(.csdLookScheduleRows(result[["design"]], includeSurvivalCalendar))
  table$addFootnote(.csdSampleSizeFootnote(settings))

  if (includeSurvivalCalendar)
    table$addFootnote(gettext("Expected enrolled subjects and expected accrued events are accrual-model quantities at each analysis time. The events column is the planned event information used for boundary construction."))
}

.csdLookScheduleRows <- function(design, includeSurvivalCalendar = FALSE) {
  rows <- data.frame(
    look        = seq_len(design[["k"]]),
    timing      = design[["timing"]],
    information = design[["n.I"]],
    stringsAsFactors = FALSE
  )

  if (includeSurvivalCalendar) {
    rows[["analysisTime"]]     <- .csdVectorOrNA(design[["T"]], design[["k"]])
    rows[["expectedSubjects"]] <- .csdGsSurvSubjectsByLook(design)
    rows[["expectedEvents"]]   <- .csdGsSurvEventsByLook(design)
  }

  return(rows)
}

.csdBoundaryRows <- function(design, includeLowerBound = TRUE) {
  upperBound <- .csdVectorOrNA(design[["upper"]][["bound"]], design[["k"]])

  rows <- data.frame(
    look   = seq_len(design[["k"]]),
    timing = design[["timing"]],
    upperZ = upperBound,
    upperP = stats::pnorm(upperBound, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )

  if (includeLowerBound) {
    lowerBound <- .csdVectorOrNA(design[["lower"]][["bound"]], design[["k"]])
    rows[["lowerZ"]] <- lowerBound
    rows[["lowerP"]] <- stats::pnorm(lowerBound)
  }

  return(rows)
}

.csdCumulativeOrNA <- function(x) {
  if (all(!is.finite(x)))
    return(rep(NA_real_, length(x)))

  return(cumsum(x))
}

.csdUpperBoundaryOvertitle <- function(settings) {
  if (settings[["testType"]] == 1)
    return(gettext("Efficacy Bound"))

  gettext("Upper Efficacy Bound")
}

.csdLowerBoundaryOvertitle <- function(settings) {
  if (settings[["testType"]] == 2)
    return(gettext("Lower Efficacy Bound"))

  gettext("Futility Bound")
}

.csdHasGsSurvCalendar <- function(design) {
  inherits(design, "gsSurv") && !is.null(design[["T"]])
}

.csdGsSurvSubjectsByLook <- function(design) {
  control      <- .csdMatrixRowSums(design[["eNC"]])
  experimental <- .csdMatrixRowSums(design[["eNE"]])

  if (length(control) == 0 || length(experimental) == 0)
    return(numeric())

  return(control + experimental)
}

.csdGsSurvEventsByLook <- function(design) {
  control      <- .csdMatrixRowSums(design[["eDC"]])
  experimental <- .csdMatrixRowSums(design[["eDE"]])

  if (length(control) == 0 || length(experimental) == 0)
    return(numeric())

  return(control + experimental)
}

.csdMatrixRowSums <- function(x) {
  if (is.null(x))
    return(numeric())

  x <- as.matrix(x)
  return(rowSums(x))
}

.csdFinalCrossingProbabilitiesTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialFinalCrossingProbabilities"]]))
    return()

  table <- createJaspTable(title = gettext("Final Boundary Crossing Probabilities"))
  table$dependOn(c(.csdDependencies, "crossingProbabilitiesTable"))
  table$position <- 3
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialFinalCrossingProbabilities"]] <- table

  includeLowerBound <- !jaspBase::isTryError(settings) && .csdDesignUsesLowerBound(settings)

  table$addColumnInfo(name = "under", title = gettext("Assumption"), type = "string")

  if (includeLowerBound)
    table$addColumnInfo(name = "lower", title = .csdLowerCrossingTitle(settings), type = "number")

  table$addColumnInfo(name = "upper", title = gettext("Cross Efficacy Bound"), type = "number")
  table$addColumnInfo(name = "noCrossing", title = gettext("No Boundary Crossing"), type = "number")

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute final boundary crossing probabilities: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.csdFinalCrossingRows(settings, result[["design"]]))
  table$addFootnote(gettext("Probabilities are cumulative through the final planned look."))
}

.csdCumulativeCrossingProbabilitiesTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialCumulativeCrossingProbabilities"]]))
    return()

  table <- createJaspTable(title = gettext("Cumulative Boundary Crossing Probabilities by Look"))
  table$dependOn(c(.csdDependencies, "crossingProbabilitiesTable"))
  table$position <- 7
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialCumulativeCrossingProbabilities"]] <- table

  includeLowerBound <- !jaspBase::isTryError(settings) && .csdDesignUsesLowerBound(settings)

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  h0Overtitle <- gettext("Under H\u2080")
  h1Overtitle <- gettext("Under H\u2081")

  if (includeLowerBound)
    table$addColumnInfo(name = "h0LowerCumulative", title = .csdLowerCrossingTitle(settings), type = "number", overtitle = h0Overtitle)

  table$addColumnInfo(name = "h0UpperCumulative", title = gettext("Cross Efficacy Bound"), type = "number", overtitle = h0Overtitle)
  table$addColumnInfo(name = "h0NoCrossing", title = gettext("No Boundary Crossing Yet"), type = "number", overtitle = h0Overtitle)

  if (includeLowerBound)
    table$addColumnInfo(name = "h1LowerCumulative", title = .csdLowerCrossingTitle(settings), type = "number", overtitle = h1Overtitle)

  table$addColumnInfo(name = "h1UpperCumulative", title = gettext("Cross Efficacy Bound"), type = "number", overtitle = h1Overtitle)
  table$addColumnInfo(name = "h1NoCrossing", title = gettext("No Boundary Crossing Yet"), type = "number", overtitle = h1Overtitle)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute cumulative boundary crossing probabilities: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.csdCumulativeCrossingRows(settings, result[["design"]]))
  table$addFootnote(gettext("Cumulative probabilities show the chance that a boundary has been crossed by each look."))
}

.csdStagewiseCrossingProbabilitiesTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialStagewiseCrossingProbabilities"]]))
    return()

  table <- createJaspTable(title = gettext("New Boundary Crossings by Look"))
  table$dependOn(c(.csdDependencies, "crossingProbabilitiesTable"))
  table$position <- 8
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialStagewiseCrossingProbabilities"]] <- table

  includeLowerBound <- !jaspBase::isTryError(settings) && .csdDesignUsesLowerBound(settings)

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")

  h0Overtitle <- gettext("Under H\u2080")
  h1Overtitle <- gettext("Under H\u2081")

  if (includeLowerBound)
    table$addColumnInfo(name = "h0Lower", title = .csdLowerCrossingTitle(settings), type = "number", overtitle = h0Overtitle)

  table$addColumnInfo(name = "h0Upper", title = gettext("Cross Efficacy Bound"), type = "number", overtitle = h0Overtitle)

  if (includeLowerBound)
    table$addColumnInfo(name = "h1Lower", title = .csdLowerCrossingTitle(settings), type = "number", overtitle = h1Overtitle)

  table$addColumnInfo(name = "h1Upper", title = gettext("Cross Efficacy Bound"), type = "number", overtitle = h1Overtitle)

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute new boundary crossings: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.csdStagewiseCrossingRows(settings, result[["design"]]))
  table$addFootnote(gettext("New crossing probabilities isolate boundary crossings that first occur at the given look."))
}

.csdNonBindingTypeIErrorTable <- function(jaspResults, settings, result) {
  if (jaspBase::isTryError(settings) || !.csdDesignUsesNonBindingFutility(settings))
    return()

  if (!is.null(jaspResults[["classicalSequentialNonBindingTypeIError"]]))
    return()

  table <- createJaspTable(title = gettext("Non-Binding Type I Error Accounting"))
  table$dependOn(c(.csdDependencies, "crossingProbabilitiesTable"))
  table$position <- 9
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["classicalSequentialNonBindingTypeIError"]] <- table

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")
  table$addColumnInfo(name = "upperNonBinding", title = gettext("Stagewise Efficacy Crossing"), type = "number")
  table$addColumnInfo(name = "upperNonBindingCumulative", title = gettext("Cumulative Efficacy Crossing"), type = "number")

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute non-binding Type I error accounting: %1$s", .bfdCleanError(result)))
    return()
  }

  table$setData(.csdNonBindingTypeIErrorRows(result[["design"]]))
  table$addFootnote(gettext("These probabilities evaluate Type I error for the efficacy rule when crossing the futility bound would not force stopping."))
}

.csdFinalCrossingRows <- function(settings, design) {
  rows <- .csdFilteredCrossingRows(design)
  rows <- rows[rows[["look"]] == design[["k"]], , drop = FALSE]
  rows[["lower"]] <- rows[["lowerCumulative"]]
  rows[["upper"]] <- rows[["upperCumulative"]]
  rows[["noCrossing"]] <- .csdNoCrossingProbability(rows[["lower"]], rows[["upper"]])

  columns <- c(
    "under",
    if (.csdDesignUsesLowerBound(settings)) "lower",
    "upper",
    "noCrossing"
  )

  return(rows[, columns, drop = FALSE])
}

.csdCumulativeCrossingRows <- function(settings, design) {
  rows <- .csdFilteredCrossingRows(design)
  rows[["noCrossing"]] <- .csdNoCrossingProbability(rows[["lowerCumulative"]], rows[["upperCumulative"]])
  h0Rows <- .csdCrossingRowsForAssumption(rows, gettext("H\u2080"), design[["k"]])
  h1Rows <- .csdCrossingRowsForAssumption(rows, gettext("H\u2081"), design[["k"]])

  out <- data.frame(
    look               = seq_len(design[["k"]]),
    h0UpperCumulative  = h0Rows[["upperCumulative"]],
    h0NoCrossing       = h0Rows[["noCrossing"]],
    h1UpperCumulative  = h1Rows[["upperCumulative"]],
    h1NoCrossing       = h1Rows[["noCrossing"]],
    stringsAsFactors = FALSE
  )

  if (.csdDesignUsesLowerBound(settings)) {
    out[["h0LowerCumulative"]] <- h0Rows[["lowerCumulative"]]
    out[["h1LowerCumulative"]] <- h1Rows[["lowerCumulative"]]
    out <- out[, c("look", "h0LowerCumulative", "h0UpperCumulative", "h0NoCrossing", "h1LowerCumulative", "h1UpperCumulative", "h1NoCrossing"), drop = FALSE]
  }

  return(out)
}

.csdCrossingRowsForAssumption <- function(rows, assumption, looks) {
  assumptionRows <- rows[rows[["under"]] == assumption, , drop = FALSE]
  assumptionRows <- assumptionRows[match(seq_len(looks), assumptionRows[["look"]]), , drop = FALSE]
  row.names(assumptionRows) <- NULL

  return(assumptionRows)
}

.csdStagewiseCrossingRows <- function(settings, design) {
  rows <- .csdFilteredCrossingRows(design)
  h0Rows <- .csdCrossingRowsForAssumption(rows, gettext("H\u2080"), design[["k"]])
  h1Rows <- .csdCrossingRowsForAssumption(rows, gettext("H\u2081"), design[["k"]])

  out <- data.frame(
    look    = seq_len(design[["k"]]),
    h0Upper = h0Rows[["upper"]],
    h1Upper = h1Rows[["upper"]],
    stringsAsFactors = FALSE
  )

  if (.csdDesignUsesLowerBound(settings)) {
    out[["h0Lower"]] <- h0Rows[["lower"]]
    out[["h1Lower"]] <- h1Rows[["lower"]]
    out <- out[, c("look", "h0Lower", "h0Upper", "h1Lower", "h1Upper"), drop = FALSE]
  }

  return(out)
}

.csdNonBindingTypeIErrorRows <- function(design) {
  upperNonBinding <- .csdVectorOrNA(design[["falseposnb"]], design[["k"]])

  data.frame(
    look                       = seq_len(design[["k"]]),
    upperNonBinding            = upperNonBinding,
    upperNonBindingCumulative  = .csdCumulativeOrNA(upperNonBinding),
    stringsAsFactors = FALSE
  )
}

.csdFilteredCrossingRows <- function(design) {
  rows <- .csdCrossingRows(design)
  rows <- rows[rows[["under"]] %in% c(gettext("H\u2080"), gettext("H\u2081")), , drop = FALSE]
  row.names(rows) <- NULL

  return(rows)
}

.csdNoCrossingProbability <- function(lower, upper) {
  lower <- ifelse(is.finite(lower), lower, 0)
  upper <- ifelse(is.finite(upper), upper, 0)

  return(pmax(0, 1 - lower - upper))
}

.csdLowerCrossingTitle <- function(settings) {
  if (settings[["testType"]] == 2)
    return(gettext("Cross Lower Efficacy Bound"))

  gettext("Cross Futility Bound")
}

.csdCrossingRows <- function(design) {
  upperProb <- design[["upper"]][["prob"]]
  lowerProb <- design[["lower"]][["prob"]]
  falsePosNonBinding <- .csdVectorOrNA(design[["falseposnb"]], design[["k"]])
  theta     <- design[["theta"]]
  rows      <- vector("list", length(theta))

  for (i in seq_along(theta)) {
    upper <- upperProb[, i]
    lower <- if (is.null(lowerProb)) rep(NA_real_, design[["k"]]) else lowerProb[, i]
    upperNonBinding <- if (isTRUE(all.equal(theta[i], 0))) falsePosNonBinding else rep(NA_real_, design[["k"]])

    rows[[i]] <- data.frame(
      under                      = .csdThetaLabel(theta[i], design),
      theta                      = theta[i],
      look                       = seq_len(design[["k"]]),
      lower                      = lower,
      upper                      = upper,
      lowerCumulative            = cumsum(lower),
      upperCumulative            = cumsum(upper),
      upperNonBinding            = upperNonBinding,
      upperNonBindingCumulative  = cumsum(upperNonBinding),
      stringsAsFactors = FALSE
    )
  }

  return(do.call(rbind, rows))
}

.csdThetaLabel <- function(theta, design) {
  if (isTRUE(all.equal(theta, 0)))
    return(gettext("H\u2080"))

  if (isTRUE(all.equal(theta, design[["delta"]])))
    return(gettext("H\u2081"))

  return(gettext("Other"))
}

.csdExpectedN <- function(design, theta) {
  index <- which(vapply(design[["theta"]], function(x) isTRUE(all.equal(x, theta)), logical(1)))

  if (length(index) == 0)
    return(NA_real_)

  return(design[["en"]][index[1]])
}

.csdText <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialText"]]))
    return()

  html <- createJaspHtml(title = gettext("Explanation"))
  html$dependOn(c(.csdDependencies, "text"))
  html$position <- 10
  jaspResults[["classicalSequentialText"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested classical group sequential design could not be completed with the current settings.")
    return()
  }

  design <- result[["design"]]
  informationLabel <- .csdInformationLabels(settings)[["text"]]
  html[["text"]] <- paste0(
    "<p>",
    gettextf(
      "This classical group sequential design uses %1$s with %2$s planned looks.",
      .csdDesignTypeLabel(settings[["designType"]]),
      design[["k"]]
    ),
    "</p><p>",
    gettextf(
      "The maximum %1$s is %2$s, compared with %3$s for the corresponding fixed design.",
      informationLabel,
      .csdFormatNumber(max(design[["n.I"]], na.rm = TRUE)),
      .csdFormatNumber(design[["n.fix"]])
    ),
    "</p>"
  )
}

.csdRCode <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialRCode"]]))
    return()

  html <- createJaspHtml(title = gettext("R Code"))
  html$dependOn(c(.csdDependencies, "generateRCode"))
  html$position <- 99
  jaspResults[["classicalSequentialRCode"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("R code could not be generated because the classical group sequential design could not be computed.")
    return()
  }

  code <- try(.csdRCodeText(settings), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .bfdCleanError(code))
    return()
  }

  html[["text"]] <- .bfdCodeHtml(code)
}

.csdRCodeText <- function(settings) {
  if (.csdUsesGsSurv(settings))
    return(.bfdFormatRCallValues("gsDesign::gsSurv", .csdGsSurvCodeArgs(settings)))

  fixedInformationCode <- .csdFixedInformationRCode(settings)
  nFixValue <- if (is.null(fixedInformationCode)) NULL else .csdFixedInformationNFixRCode(settings)
  args <- .csdDesignCodeArgs(settings, nFixValue)
  designCode <- .bfdFormatRCallValues("gsDesign::gsDesign", args)

  if (!is.null(fixedInformationCode))
    return(paste(paste0("fixedInformation <- ", fixedInformationCode), designCode, sep = "\n\n"))

  return(designCode)
}

.csdFixedInformationNFixRCode <- function(settings) {
  if (identical(settings[["effectScale"]], "survivalHazardRatio") &&
      identical(settings[["survivalInformationMethod"]], "nSurvival")) {
    return("fixedInformation[[\"nEvents\"]]")
  }

  return("fixedInformation")
}

.csdDesignCodeArgs <- function(settings, nFixValue = NULL) {
  args       <- .csdDesignArgs(settings)
  codeValues <- c(sfu = .csdSpendingFunctionRValue(settings[["upperBoundary"]], settings[["testType"]]))

  if (!is.null(nFixValue))
    codeValues[["n.fix"]] <- nFixValue

  if (.csdUsesGsDesignSurvivalEndpoint(settings) &&
      !is.null(settings[["nFixSurv"]]) &&
      identical(settings[["survivalInformationMethod"]], "nSurvival")) {
    codeValues[["nFixSurv"]] <- "fixedInformation[[\"n\"]]"
  }

  if (settings[["testType"]] %in% c(3, 4))
    codeValues[["sfl"]] <- .csdSpendingFunctionRValue(settings[["lowerBoundary"]], settings[["testType"]])

  values <- vapply(args, .bfdFormatRValue, character(1))
  values[names(codeValues)] <- codeValues

  return(values)
}

.csdGsSurvCodeArgs <- function(settings) {
  args       <- .csdGsSurvArgs(settings)
  codeValues <- c(sfu = .csdSpendingFunctionRValue(settings[["upperBoundary"]], settings[["testType"]]))

  if (settings[["testType"]] %in% c(3, 4))
    codeValues[["sfl"]] <- .csdSpendingFunctionRValue(settings[["lowerBoundary"]], settings[["testType"]])

  values <- vapply(args, .bfdFormatRValue, character(1))
  values[names(codeValues)] <- codeValues

  return(values)
}

.csdFixedInformationRCode <- function(settings) {
  if (settings[["sampleSizeMode"]] != "effectSize")
    return(NULL)

  if (settings[["effectScale"]] %in% c("independentSamplesSmd", "meanDifferenceIndependentSamples"))
    return(.bfdFormatRCall("gsDesign::nNormal", .csdNormalFixedInformationArgs(settings)))

  if (settings[["effectScale"]] == "twoSampleBinary")
    return(.bfdFormatRCall("gsDesign::nBinomial", .csdBinomialFixedInformationArgs(settings)))

  if (settings[["effectScale"]] == "survivalHazardRatio") {
    if (settings[["survivalInformationMethod"]] == "nSurvival")
      return(.bfdFormatRCall("gsDesign::nSurvival", .csdNSurvivalFixedInformationArgs(settings)))

    if (settings[["survivalInformationMethod"]] == "events")
      return(.bfdFormatRCall("gsDesign::nEvents", .csdNEventsFixedInformationArgs(settings)))
  }

  return(NULL)
}

.csdRNumber <- function(x) {
  if (is.na(x))
    return("NA_real_")

  if (is.infinite(x))
    return(if (x > 0) "Inf" else "-Inf")

  return(format(signif(x, 15), scientific = FALSE, trim = TRUE))
}

.csdBoundariesPlot <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialBoundariesPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Stopping Boundaries"), width = .bfdPlotWidth(settings), height = 350)
  plot$dependOn(.csdBoundariesPlotDependencies)
  plot$position <- 12
  jaspResults[["classicalSequentialBoundariesPlot"]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute stopping boundaries plot: %1$s", .bfdCleanError(result)))
    return()
  }

  plotResult <- try(.csdBuildBoundariesPlot(settings, result[["design"]]), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute stopping boundaries plot: %1$s", .bfdCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.csdBuildBoundariesPlot <- function(settings, design) {
  data <- .csdBoundaryPlotData(design)

  if (nrow(data) == 0)
    stop(gettext("No finite stopping boundaries are available for the current settings."))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = timing, y = z, color = boundary)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "#666666") +
    .pwrPrettyXAxisScale(c(0, data[["timing"]])) +
    .pwrPrettyYAxisScale(c(0, data[["z"]])) +
    ggplot2::labs(x = gettext("Information fraction"), y = "Z", color = gettext("Boundary"))

  return(.bfdApplyPlotTheme(plot, settings))
}

.csdBoundaryPlotData <- function(design) {
  boundaryRows <- .csdBoundaryRows(design, includeLowerBound = TRUE)
  scheduleRows <- .csdLookScheduleRows(design)

  data <- rbind(
    data.frame(timing = scheduleRows[["timing"]], z = boundaryRows[["lowerZ"]], boundary = gettext("Lower"), stringsAsFactors = FALSE),
    data.frame(timing = scheduleRows[["timing"]], z = boundaryRows[["upperZ"]], boundary = gettext("Upper"), stringsAsFactors = FALSE)
  )

  data <- data[is.finite(data[["z"]]), , drop = FALSE]

  return(data)
}

.csdCrossingProbabilitiesPlot <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialCrossingProbabilitiesPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Boundary Crossing Probabilities"), width = .bfdPlotWidth(settings), height = 350)
  plot$dependOn(.csdCrossingProbabilitiesPlotDependencies)
  plot$position <- 13
  jaspResults[["classicalSequentialCrossingProbabilitiesPlot"]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute crossing probabilities plot: %1$s", .bfdCleanError(result)))
    return()
  }

  plotResult <- try(.csdBuildCrossingProbabilitiesPlot(settings, result[["design"]]), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute crossing probabilities plot: %1$s", .bfdCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.csdBuildCrossingProbabilitiesPlot <- function(settings, design) {
  data <- .csdCrossingPlotData(design)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = look, y = probability, color = boundary, linetype = under)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    .pwrPrettyIntegerXAxisScale(data[["look"]]) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(x = gettext("Look"), y = gettext("Cumulative probability"), color = gettext("Boundary"), linetype = gettext("Under"))

  return(.bfdApplyPlotTheme(plot, settings))
}

.csdCrossingPlotData <- function(design) {
  rows <- .csdCrossingRows(design)
  rows <- rows[rows[["under"]] %in% c(gettext("H\u2080"), gettext("H\u2081")), , drop = FALSE]

  data <- rbind(
    data.frame(look = rows[["look"]], probability = rows[["lowerCumulative"]], boundary = gettext("Lower"), under = rows[["under"]], stringsAsFactors = FALSE),
    data.frame(look = rows[["look"]], probability = rows[["upperCumulative"]], boundary = gettext("Upper"), under = rows[["under"]], stringsAsFactors = FALSE),
    data.frame(look = rows[["look"]], probability = rows[["upperNonBindingCumulative"]], boundary = gettext("Upper (non-binding)"), under = rows[["under"]], stringsAsFactors = FALSE)
  )
  data <- data[is.finite(data[["probability"]]), , drop = FALSE]

  return(data)
}

.csdVectorOrNA <- function(x, n) {
  if (is.null(x))
    return(rep(NA_real_, n))

  return(as.numeric(x))
}

.csdFormatNumber <- function(x) {
  return(format(round(x, 3), nsmall = 3, trim = TRUE))
}
