GroupSequentialDesign <- function(jaspResults, dataset, options) {
  settings <- try(.csdPrepareSettings(options), silent = TRUE)
  result   <- if (jaspBase::isTryError(settings)) {
    settings
  } else {
    try(.csdComputeResult(settings), silent = TRUE)
  }

  .csdSummaryTable(jaspResults, settings, result)
  .csdBoundariesTable(jaspResults, settings, result)
  .csdCrossingProbabilitiesTable(jaspResults, settings, result)

  if (isTRUE(options[["text"]]))
    .csdText(jaspResults, settings, result)

  if (isTRUE(options[["generateRCode"]]))
    .csdRCode(jaspResults, settings, result)

  if (isTRUE(options[["boundariesPlot"]]))
    .csdBoundariesPlot(jaspResults, settings, result)

  if (isTRUE(options[["crossingProbabilitiesPlot"]]))
    .csdCrossingProbabilitiesPlot(jaspResults, settings, result)

  return()
}

.csdDependencies <- c(
  "designType", "numberOfLooks", "timingMode", "timing",
  "alpha", "power", "sampleSizeMode", "fixedSampleSize", "effectSize",
  "upperBoundary", "upperBoundaryParameter", "lowerBoundary",
  "lowerBoundaryParameter", "gridPoints"
)

.csdPrepareSettings <- function(options) {
  testType <- .csdTestType(options[["designType"]])

  settings <- list(
    designType             = options[["designType"]],
    testType               = testType,
    designTypeLabel        = .csdDesignTypeLabel(options[["designType"]]),
    numberOfLooks          = options[["numberOfLooks"]],
    timingMode             = options[["timingMode"]],
    alpha                  = options[["alpha"]],
    power                  = options[["power"]],
    beta                   = 1 - options[["power"]],
    sampleSizeMode         = options[["sampleSizeMode"]],
    fixedSampleSize        = options[["fixedSampleSize"]],
    effectSize             = abs(options[["effectSize"]]),
    upperBoundary          = options[["upperBoundary"]],
    upperBoundaryLabel     = .csdBoundaryLabel(options[["upperBoundary"]], testType),
    upperBoundaryParameter = options[["upperBoundaryParameter"]],
    lowerBoundary          = options[["lowerBoundary"]],
    lowerBoundaryLabel     = .csdBoundaryLabel(options[["lowerBoundary"]], testType),
    lowerBoundaryParameter = options[["lowerBoundaryParameter"]],
    gridPoints             = options[["gridPoints"]]
  )

  settings[["timing"]] <- .csdTiming(settings, options[["timing"]])
  .csdValidateSettings(settings)

  if (settings[["sampleSizeMode"]] == "effectSize") {
    settings[["delta"]] <- settings[["effectSize"]]
    settings[["nFix"]]  <- 1
  } else if (settings[["sampleSizeMode"]] == "generic") {
    settings[["delta"]] <- 0
    settings[["nFix"]]  <- 1
  } else {
    settings[["delta"]] <- 0
    settings[["nFix"]]  <- settings[["fixedSampleSize"]]
  }

  return(settings)
}

.csdValidateSettings <- function(settings) {
  if (settings[["numberOfLooks"]] < 2)
    stop(gettext("The number of looks must be at least 2."))

  if (settings[["testType"]] %in% c(3, 4) && settings[["upperBoundary"]] == "wangTsiatis")
    stop(gettext("Wang-Tsiatis boundaries are not available for asymmetric designs."))

  .csdValidateBoundaryParameter(settings[["upperBoundary"]], settings[["upperBoundaryParameter"]])

  if (settings[["testType"]] %in% c(3, 4))
    .csdValidateBoundaryParameter(settings[["lowerBoundary"]], settings[["lowerBoundaryParameter"]])
}

.csdValidateBoundaryParameter <- function(boundary, parameter) {
  if (!.csdBoundaryUsesParameter(boundary))
    return()

  if (!is.finite(parameter))
    stop(gettext("Boundary parameters must be finite."))

  if (boundary == "hwangShihDeCani" && (parameter < -40 || parameter > 40))
    stop(gettext("Hwang-Shih-DeCani parameters must be between -40 and 40."))

  if (boundary == "kimDeMetsPower" && (parameter <= 0 || parameter > 50))
    stop(gettext("Kim-DeMets power parameters must be greater than 0 and no larger than 50."))
}

.csdTestType <- function(designType) {
  switch(
    designType,
    oneSided                    = 1,
    twoSidedSymmetric           = 2,
    twoSidedAsymmetricBinding   = 3,
    twoSidedAsymmetricNonBinding = 4,
    1
  )
}

.csdDesignTypeLabel <- function(designType) {
  switch(
    designType,
    oneSided                    = gettext("One-sided"),
    twoSidedSymmetric           = gettext("Two-sided symmetric"),
    twoSidedAsymmetricBinding   = gettext("Two-sided asymmetric, binding futility"),
    twoSidedAsymmetricNonBinding = gettext("Two-sided asymmetric, non-binding futility"),
    gettext("One-sided")
  )
}

.csdBoundaryLabel <- function(boundary, testType = 1) {
  if (testType %in% c(3, 4)) {
    return(switch(
      boundary,
      obrienFleming     = gettext("O'Brien-Fleming spending"),
      pocock            = gettext("Pocock spending"),
      wangTsiatis       = gettext("Wang-Tsiatis"),
      hwangShihDeCani   = gettext("Hwang-Shih-DeCani"),
      kimDeMetsPower    = gettext("Kim-DeMets power"),
      gettext("O'Brien-Fleming spending")
    ))
  }

  switch(
    boundary,
    obrienFleming     = gettext("O'Brien-Fleming"),
    pocock            = gettext("Pocock"),
    wangTsiatis       = gettext("Wang-Tsiatis"),
    hwangShihDeCani   = gettext("Hwang-Shih-DeCani"),
    kimDeMetsPower    = gettext("Kim-DeMets power"),
    gettext("O'Brien-Fleming")
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
  if (!requireNamespace("gsDesign", quietly = TRUE))
    stop(gettext("The gsDesign package is required for this analysis."))

  design <- do.call(gsDesign::gsDesign, .csdDesignArgs(settings))

  return(list(
    design = design
  ))
}

.csdDesignArgs <- function(settings) {
  args <- list(
    k         = settings[["numberOfLooks"]],
    test.type = settings[["testType"]],
    alpha     = settings[["alpha"]],
    beta      = settings[["beta"]],
    delta     = settings[["delta"]],
    n.fix     = settings[["nFix"]],
    timing    = settings[["timing"]],
    r         = settings[["gridPoints"]],
    sfu       = .csdSpendingFunction(settings[["upperBoundary"]], settings[["testType"]])
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

.csdBoundaryUsesParameter <- function(boundary) {
  boundary %in% c("wangTsiatis", "hwangShihDeCani", "kimDeMetsPower")
}

.csdSpendingFunction <- function(boundary, testType) {
  if (testType %in% c(3, 4)) {
    return(switch(
      boundary,
      obrienFleming   = gsDesign::sfLDOF,
      pocock          = gsDesign::sfLDPocock,
      wangTsiatis     = stop(gettext("Wang-Tsiatis boundaries are not available for asymmetric designs.")),
      hwangShihDeCani = gsDesign::sfHSD,
      kimDeMetsPower  = gsDesign::sfPower,
      gsDesign::sfLDOF
    ))
  }

  switch(
    boundary,
    obrienFleming   = "OF",
    pocock          = "Pocock",
    wangTsiatis     = "WT",
    hwangShihDeCani = gsDesign::sfHSD,
    kimDeMetsPower  = gsDesign::sfPower,
    "OF"
  )
}

.csdSummaryTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialSummary"]]))
    return()

  table <- createJaspTable(title = gettext("Group Sequential Design"))
  table$dependOn(.csdDependencies)
  table$position <- 1
  jaspResults[["classicalSequentialSummary"]] <- table

  table$addColumnInfo(name = "design", title = gettext("Design"), type = "string")
  table$addColumnInfo(name = "looks", title = gettext("Looks"), type = "integer")
  table$addColumnInfo(name = "alpha", title = gettext("Alpha"), type = "number")
  table$addColumnInfo(name = "power", title = gettext("Power"), type = "number")
  table$addColumnInfo(name = "beta", title = gettext("Beta"), type = "number")
  table$addColumnInfo(name = "delta", title = gettext("Effect Size"), type = "number")
  table$addColumnInfo(name = "fixedN", title = gettext("Fixed Design N"), type = "number")
  table$addColumnInfo(name = "maximumN", title = gettext("Maximum N"), type = "number")
  table$addColumnInfo(name = "inflation", title = gettext("Inflation"), type = "number")
  table$addColumnInfo(name = "expectedNH0", title = gettext("H0"), type = "number", overtitle = gettext("Expected N"))
  table$addColumnInfo(name = "expectedNH1", title = gettext("H1"), type = "number", overtitle = gettext("Expected N"))
  table$addColumnInfo(name = "upperBoundary", title = gettext("Upper Boundary"), type = "string")
  table$addColumnInfo(name = "lowerBoundary", title = gettext("Lower Boundary"), type = "string")

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute classical group sequential design: %1$s", .csdCleanError(result)))
    return()
  }

  table$setData(.csdSummaryRow(settings, result[["design"]]))
  table$addFootnote(gettext("Alpha is the one-sided Type I error rate used by gsDesign. For symmetric two-sided designs, the total Type I error rate is twice alpha."))
  table$addFootnote(.csdSampleSizeFootnote(settings))

  if (.csdUsesLanDeMetsBoundary(settings))
    table$addFootnote(gettext("For asymmetric designs, O'Brien-Fleming and Pocock are implemented as spending functions."))
}

.csdSummaryRow <- function(settings, design) {
  maximumN <- max(design[["n.I"]], na.rm = TRUE)
  fixedN   <- design[["n.fix"]]

  data.frame(
    design        = settings[["designTypeLabel"]],
    looks         = design[["k"]],
    alpha         = design[["alpha"]],
    power         = 1 - design[["beta"]],
    beta          = design[["beta"]],
    delta         = design[["delta"]],
    fixedN        = fixedN,
    maximumN      = maximumN,
    inflation     = maximumN / fixedN,
    expectedNH0   = .csdExpectedN(design, 0),
    expectedNH1   = .csdExpectedN(design, design[["delta"]]),
    upperBoundary = settings[["upperBoundaryLabel"]],
    lowerBoundary = .csdLowerBoundarySummary(settings),
    stringsAsFactors = FALSE
  )
}

.csdLowerBoundarySummary <- function(settings) {
  if (settings[["testType"]] == 1)
    return(gettext("Not applicable"))

  if (settings[["testType"]] == 2)
    return(gettext("Same as upper (symmetric)"))

  return(settings[["lowerBoundaryLabel"]])
}

.csdSampleSizeFootnote <- function(settings) {
  if (settings[["sampleSizeMode"]] == "generic")
    return(gettext("In generic information-ratio mode, fixed design N and maximum N are relative information values, not literal sample sizes."))

  if (settings[["sampleSizeMode"]] == "fixedDesign")
    return(gettext("In fixed design sample size mode, the effect size is implied by the requested alpha, power, and fixed design N."))

  return(gettext("In standardized effect size mode, fixed design N and maximum N are computed from the requested effect size."))
}

.csdUsesLanDeMetsBoundary <- function(settings) {
  if (!(settings[["testType"]] %in% c(3, 4)))
    return(FALSE)

  any(c(settings[["upperBoundary"]], settings[["lowerBoundary"]]) %in% c("obrienFleming", "pocock"))
}

.csdBoundariesTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialBoundaries"]]))
    return()

  table <- createJaspTable(title = gettext("Stopping Boundaries"))
  table$dependOn(.csdDependencies)
  table$position <- 2
  jaspResults[["classicalSequentialBoundaries"]] <- table

  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")
  table$addColumnInfo(name = "timing", title = gettext("Information Fraction"), type = "number")
  table$addColumnInfo(name = "information", title = gettext("Information"), type = "number")
  table$addColumnInfo(name = "lowerZ", title = "Z", type = "number", overtitle = gettext("Lower Bound"))
  table$addColumnInfo(name = "lowerP", title = "p", type = "pvalue", overtitle = gettext("Lower Bound"))
  table$addColumnInfo(name = "lowerSpend", title = gettext("Spend"), type = "number", overtitle = gettext("Lower Bound"))
  table$addColumnInfo(name = "upperZ", title = "Z", type = "number", overtitle = gettext("Upper Bound"))
  table$addColumnInfo(name = "upperP", title = "p", type = "pvalue", overtitle = gettext("Upper Bound"))
  table$addColumnInfo(name = "upperSpend", title = gettext("Spend"), type = "number", overtitle = gettext("Upper Bound"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute stopping boundaries: %1$s", .csdCleanError(result)))
    return()
  }

  table$setData(.csdBoundaryRows(result[["design"]]))
}

.csdBoundaryRows <- function(design) {
  lowerBound <- .csdVectorOrNA(design[["lower"]][["bound"]], design[["k"]])
  lowerSpend <- .csdVectorOrNA(design[["lower"]][["spend"]], design[["k"]])
  upperBound <- .csdVectorOrNA(design[["upper"]][["bound"]], design[["k"]])
  upperSpend <- .csdVectorOrNA(design[["upper"]][["spend"]], design[["k"]])

  data.frame(
    look        = seq_len(design[["k"]]),
    timing      = design[["timing"]],
    information = design[["n.I"]],
    lowerZ      = lowerBound,
    lowerP      = stats::pnorm(lowerBound),
    lowerSpend  = lowerSpend,
    upperZ      = upperBound,
    upperP      = stats::pnorm(upperBound, lower.tail = FALSE),
    upperSpend  = upperSpend,
    stringsAsFactors = FALSE
  )
}

.csdCrossingProbabilitiesTable <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialCrossingProbabilities"]]))
    return()

  table <- createJaspTable(title = gettext("Boundary Crossing Probabilities"))
  table$dependOn(.csdDependencies)
  table$position <- 3
  jaspResults[["classicalSequentialCrossingProbabilities"]] <- table

  table$addColumnInfo(name = "under", title = gettext("Under"), type = "string")
  table$addColumnInfo(name = "theta", title = gettext("Theta"), type = "number")
  table$addColumnInfo(name = "look", title = gettext("Look"), type = "integer")
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = gettext("Stagewise"))
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = gettext("Stagewise"))
  table$addColumnInfo(name = "lowerCumulative", title = gettext("Lower"), type = "number", overtitle = gettext("Cumulative"))
  table$addColumnInfo(name = "upperCumulative", title = gettext("Upper"), type = "number", overtitle = gettext("Cumulative"))
  table$addColumnInfo(name = "upperNonBinding", title = gettext("Upper"), type = "number", overtitle = gettext("Stagewise Non-Binding"))
  table$addColumnInfo(name = "upperNonBindingCumulative", title = gettext("Upper"), type = "number", overtitle = gettext("Cumulative Non-Binding"))

  if (jaspBase::isTryError(result)) {
    table$setError(gettextf("Unable to compute boundary crossing probabilities: %1$s", .csdCleanError(result)))
    return()
  }

  table$setData(.csdCrossingRows(result[["design"]]))

  if (settings[["testType"]] == 4)
    table$addFootnote(gettext("For non-binding futility designs, Type I error computations allow the trial to continue after crossing a lower bound. Lower and upper crossing probabilities assume the trial stops when any boundary is crossed; non-binding upper probabilities allow continuation after lower-bound crossings."))
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
    return(gettext("H0"))

  if (isTRUE(all.equal(theta, design[["delta"]])))
    return(gettext("H1"))

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
  html$position <- 4
  jaspResults[["classicalSequentialText"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("The requested classical group sequential design could not be completed with the current settings.")
    return()
  }

  design <- result[["design"]]
  html[["text"]] <- paste0(
    "<p>",
    gettextf(
      "This classical group sequential design uses %1$s with %2$s planned looks.",
      settings[["designTypeLabel"]],
      design[["k"]]
    ),
    "</p><p>",
    gettextf(
      "The maximum information is %1$s, compared with %2$s for the corresponding fixed design.",
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
  html$position <- 7
  jaspResults[["classicalSequentialRCode"]] <- html

  if (jaspBase::isTryError(result)) {
    html[["text"]] <- gettext("R code could not be generated because the classical group sequential design could not be computed.")
    return()
  }

  code <- try(.csdRCodeText(settings), silent = TRUE)
  if (jaspBase::isTryError(code)) {
    html[["text"]] <- gettextf("Unable to generate R code: %1$s", .csdCleanError(code))
    return()
  }

  html[["text"]] <- paste0(
    "<pre><code>",
    .csdEscapeHtml(code),
    "</code></pre>"
  )
}

.csdRCodeText <- function(settings) {
  args <- .csdDesignCodeArgs(settings)
  maxNameWidth <- max(nchar(names(args)))

  lines <- paste0(
    "  ",
    format(names(args), width = maxNameWidth, justify = "left"),
    " = ",
    unname(args),
    c(rep(",", length(args) - 1), "")
  )

  return(paste(c("gsDesign::gsDesign(", lines, ")"), collapse = "\n"))
}

.csdDesignCodeArgs <- function(settings) {
  args <- c(
    k         = .csdRValue(settings[["numberOfLooks"]]),
    test.type = .csdRValue(settings[["testType"]]),
    alpha     = .csdRValue(settings[["alpha"]]),
    beta      = .csdRValue(settings[["beta"]]),
    delta     = .csdRValue(settings[["delta"]]),
    n.fix     = .csdRValue(settings[["nFix"]]),
    timing    = .csdRValue(settings[["timing"]]),
    r         = .csdRValue(settings[["gridPoints"]]),
    sfu       = .csdSpendingFunctionRValue(settings[["upperBoundary"]], settings[["testType"]])
  )

  if (.csdBoundaryUsesParameter(settings[["upperBoundary"]]))
    args[["sfupar"]] <- .csdRValue(settings[["upperBoundaryParameter"]])

  if (settings[["testType"]] %in% c(3, 4)) {
    args[["sfl"]] <- .csdSpendingFunctionRValue(settings[["lowerBoundary"]], settings[["testType"]])

    if (.csdBoundaryUsesParameter(settings[["lowerBoundary"]]))
      args[["sflpar"]] <- .csdRValue(settings[["lowerBoundaryParameter"]])
  }

  return(args)
}

.csdSpendingFunctionRValue <- function(boundary, testType) {
  if (testType %in% c(3, 4)) {
    return(switch(
      boundary,
      obrienFleming   = "gsDesign::sfLDOF",
      pocock          = "gsDesign::sfLDPocock",
      hwangShihDeCani = "gsDesign::sfHSD",
      kimDeMetsPower  = "gsDesign::sfPower",
      "gsDesign::sfLDOF"
    ))
  }

  switch(
    boundary,
    obrienFleming   = "\"OF\"",
    pocock          = "\"Pocock\"",
    wangTsiatis     = "\"WT\"",
    hwangShihDeCani = "gsDesign::sfHSD",
    kimDeMetsPower  = "gsDesign::sfPower",
    "\"OF\""
  )
}

.csdRValue <- function(x) {
  if (length(x) > 1)
    return(paste0("c(", paste(vapply(x, .csdRValue, character(1)), collapse = ", "), ")"))

  if (is.numeric(x))
    return(.csdRNumber(x))

  if (is.logical(x))
    return(if (isTRUE(x)) "TRUE" else "FALSE")

  return(deparse(x))
}

.csdRNumber <- function(x) {
  if (is.na(x))
    return("NA_real_")

  if (is.infinite(x))
    return(if (x > 0) "Inf" else "-Inf")

  return(format(signif(x, 15), scientific = FALSE, trim = TRUE))
}

.csdEscapeHtml <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)

  return(text)
}

.csdBoundariesPlot <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialBoundariesPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Stopping Boundaries"), width = 735, height = 350)
  plot$dependOn(c(.csdDependencies, "boundariesPlot"))
  plot$position <- 5
  jaspResults[["classicalSequentialBoundariesPlot"]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute stopping boundaries plot: %1$s", .csdCleanError(result)))
    return()
  }

  plotResult <- try(.csdBuildBoundariesPlot(result[["design"]]), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute stopping boundaries plot: %1$s", .csdCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.csdBuildBoundariesPlot <- function(design) {
  data <- .csdBoundaryPlotData(design)

  if (nrow(data) == 0)
    stop(gettext("No finite stopping boundaries are available for the current settings."))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = timing, y = z, color = boundary)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "#666666") +
    ggplot2::labs(x = gettext("Information fraction"), y = "Z", color = gettext("Boundary"))

  return(.pwrApplyPlotTheme(plot))
}

.csdBoundaryPlotData <- function(design) {
  rows <- .csdBoundaryRows(design)

  data <- rbind(
    data.frame(timing = rows[["timing"]], z = rows[["lowerZ"]], boundary = gettext("Lower"), stringsAsFactors = FALSE),
    data.frame(timing = rows[["timing"]], z = rows[["upperZ"]], boundary = gettext("Upper"), stringsAsFactors = FALSE)
  )

  data <- data[is.finite(data[["z"]]), , drop = FALSE]

  return(data)
}

.csdCrossingProbabilitiesPlot <- function(jaspResults, settings, result) {
  if (!is.null(jaspResults[["classicalSequentialCrossingProbabilitiesPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Boundary Crossing Probabilities"), width = 735, height = 350)
  plot$dependOn(c(.csdDependencies, "crossingProbabilitiesPlot"))
  plot$position <- 6
  jaspResults[["classicalSequentialCrossingProbabilitiesPlot"]] <- plot

  if (jaspBase::isTryError(result)) {
    plot$setError(gettextf("Unable to compute crossing probabilities plot: %1$s", .csdCleanError(result)))
    return()
  }

  plotResult <- try(.csdBuildCrossingProbabilitiesPlot(result[["design"]]), silent = TRUE)
  if (jaspBase::isTryError(plotResult)) {
    plot$setError(gettextf("Unable to compute crossing probabilities plot: %1$s", .csdCleanError(plotResult)))
    return()
  }

  plot$plotObject <- plotResult
}

.csdBuildCrossingProbabilitiesPlot <- function(design) {
  data <- .csdCrossingPlotData(design)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = look, y = probability, color = boundary, linetype = under)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(x = gettext("Look"), y = gettext("Cumulative probability"), color = gettext("Boundary"), linetype = gettext("Under"))

  return(.pwrApplyPlotTheme(plot))
}

.csdCrossingPlotData <- function(design) {
  rows <- .csdCrossingRows(design)
  rows <- rows[rows[["under"]] %in% c(gettext("H0"), gettext("H1")), , drop = FALSE]

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

.csdCleanError <- function(error) {
  message <- as.character(error)
  message <- sub("^Error[^:]*: ", "", message)
  message <- trimws(message)

  return(message)
}

.csdFormatNumber <- function(x) {
  return(format(round(x, 3), nsmall = 3, trim = TRUE))
}
