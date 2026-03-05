# Bayesian power analysis helpers based on BayesPower package.

.runBayesianOneSampleT <- function(jaspResults, options) {
  results <- try(.computeBayesianOneSampleT(options))
  .checkResults(results)

  .initBayesianPowerTab(jaspResults, options, results, type = "oneSample")

  if (options$text) {
    .populateIntro(jaspResults, options)
    .populateBayesianText(jaspResults, options, results, type = "oneSample")
  }
}

.runBayesianIndependentSamplesT <- function(jaspResults, options) {
  results <- try(.computeBayesianIndependentSamplesT(options))
  .checkResults(results)

  .initBayesianPowerTab(jaspResults, options, results, type = "twoSample")

  if (options$text) {
    .populateIntro(jaspResults, options)
    .populateBayesianText(jaspResults, options, results, type = "twoSample")
  }
}

.runBayesianOneSampleProportion <- function(jaspResults, options) {
  results <- try(.computeBayesianOneSampleProportion(options))
  .checkResults(results)

  .initBayesianPowerTab(jaspResults, options, results, type = "oneSample")

  if (options$text) {
    .populateIntro(jaspResults, options)
    .populateBayesianText(jaspResults, options, results, type = "oneSample")
  }
}

.mapBayesianAlternative <- function(alternative) {
  switch(alternative,
    "twoSided" = "two.sided",
    alternative
  )
}

.extractBayesianRates <- function(results, type) {
  table <- as.data.frame(results$results, stringsAsFactors = FALSE)
  values <- suppressWarnings(as.numeric(table[1, ]))

  if (length(values) < 5 || anyNA(values[1:5])) {
    stop(gettext("Unable to extract Bayesian power results from BayesPower output."))
  }

  parsed <- list(
    tp = values[1],
    fn = values[2],
    tn = values[3],
    fp = values[4]
  )

  if (type == "twoSample") {
    if (length(values) < 6 || is.na(values[6])) {
      stop(gettext("Unable to extract Bayesian two-sample sizes from BayesPower output."))
    }
    parsed$n1 <- ceiling(values[5])
    parsed$n2 <- ceiling(values[6])
  } else {
    parsed$n <- ceiling(values[5])
  }

  if (any(!is.finite(unlist(parsed)))) {
    stop(gettext("Bayesian power calculation produced non-finite results."))
  }

  parsed
}

.computeBayesianOneSampleT <- function(options) {
  calc <- options$bayesianCalculation

  n <- if (calc == "evidenceRates") options$sampleSize else NULL
  result <- BayesPower::BFpower.ttest.OneSample(
    alternative = .mapBayesianAlternative(options$alternative),
    prior_analysis = options$bayesianPrior,
    location = options$bayesianPriorLocation,
    scale = options$bayesianPriorScale,
    dff = options$bayesianPriorDf,
    prior_design = NULL,
    location_d = options$bayesianPriorLocation,
    scale_d = options$bayesianPriorScale,
    dff_d = options$bayesianPriorDf,
    N = n,
    type_rate = options$bayesianRateType,
    true_rate = options$bayesianTrueRate,
    false_rate = options$bayesianFalseRate,
    threshold = options$bayesianThreshold,
    plot_power = FALSE,
    plot_rel = FALSE
  )

  .extractBayesianRates(result, type = "oneSample")
}

.computeBayesianIndependentSamplesT <- function(options) {
  calc <- options$bayesianCalculation
  n1 <- if (calc == "evidenceRates") options$sampleSize else NULL
  n2 <- if (calc == "evidenceRates") ceiling(options$sampleSizeRatio * options$sampleSize) else NULL

  result <- BayesPower::BFpower.ttest.TwoSample(
    alternative = .mapBayesianAlternative(options$alternative),
    threshold = options$bayesianThreshold,
    true_rate = options$bayesianTrueRate,
    false_rate = options$bayesianFalseRate,
    prior_analysis = options$bayesianPrior,
    location = options$bayesianPriorLocation,
    scale = options$bayesianPriorScale,
    dff = options$bayesianPriorDf,
    prior_design = NULL,
    location_d = options$bayesianPriorLocation,
    scale_d = options$bayesianPriorScale,
    dff_d = options$bayesianPriorDf,
    N1 = n1,
    N2 = n2,
    r = if (calc == "sampleSize") options$sampleSizeRatio else NULL,
    type_rate = options$bayesianRateType,
    plot_power = FALSE,
    plot_rel = FALSE
  )

  .extractBayesianRates(result, type = "twoSample")
}

.computeBayesianOneSampleProportion <- function(options) {
  calc <- options$bayesianCalculation

  n <- if (calc == "evidenceRates") options$sampleSize else NULL

  result <- BayesPower::BFpower.bin(
    alternative = .mapBayesianAlternative(options$alternative),
    threshold = options$bayesianThreshold,
    h0 = options$baselineProportion,
    true_rate = options$bayesianTrueRate,
    false_rate = options$bayesianFalseRate,
    prior_analysis = options$bayesianPrior,
    alpha = options$bayesianPriorAlpha,
    beta = options$bayesianPriorBeta,
    scale = options$bayesianPriorScale,
    prior_design = NULL,
    alpha_d = options$bayesianPriorAlpha,
    beta_d = options$bayesianPriorBeta,
    location_d = options$baselineProportion,
    scale_d = options$bayesianPriorScale,
    N = n,
    type_rate = options$bayesianRateType,
    plot_power = FALSE,
    plot_rel = FALSE
  )

  .extractBayesianRates(result, type = "oneSample")
}

.initBayesianPowerTab <- function(jaspResults, options, results, type = c("oneSample", "twoSample")) {
  type <- match.arg(type)

  table <- jaspResults[["powertab"]]
  if (!is.null(table)) {
    return()
  }

  table <- createJaspTable(title = gettext("Bayesian Power Analysis"))
  table$dependOn(c(
    "test",
    "bayesianCalculation",
    "sampleSize",
    "sampleSizeRatio",
    "alternative",
    "baselineProportion",
    "bayesianThreshold",
    "bayesianRateType",
    "bayesianTrueRate",
    "bayesianFalseRate",
    "bayesianPrior",
    "bayesianPriorLocation",
    "bayesianPriorScale",
    "bayesianPriorDf",
    "bayesianPriorAlpha",
    "bayesianPriorBeta"
  ))
  table$position <- 2
  jaspResults[["powertab"]] <- table

  table$addColumnInfo(name = "tp", title = "TP", type = "number")
  table$addColumnInfo(name = "fn", title = "FN", type = "number")
  table$addColumnInfo(name = "tn", title = "TN", type = "number")
  table$addColumnInfo(name = "fp", title = "FP", type = "number")

  row <- list(
    tp = results$tp,
    fn = results$fn,
    tn = results$tn,
    fp = results$fp
  )

  if (type == "twoSample") {
    table$addColumnInfo(name = "n1", title = "N\u2081", type = "integer")
    table$addColumnInfo(name = "n2", title = "N\u2082", type = "integer")
    row$n1 <- results$n1
    row$n2 <- results$n2
  } else {
    table$addColumnInfo(name = "sampleSize", title = "N", type = "integer")
    row$sampleSize <- results$n
  }

  table$addRows(rowNames = 1, row)

  if (options$bayesianCalculation == "sampleSize") {
    table$addFootnote(gettext("Sample sizes were optimized for the selected target evidence rates."))
  } else {
    table$addFootnote(gettext("Evidence rates were calculated for the fixed sample size design."))
  }
}

.populateBayesianText <- function(jaspResults, options, results, type = c("oneSample", "twoSample")) {
  type <- match.arg(type)

  html <- jaspResults[["tabText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c(
      "test",
      "text",
      "bayesianCalculation",
      "sampleSize",
      "sampleSizeRatio",
      "alternative",
      "baselineProportion",
      "bayesianThreshold",
      "bayesianRateType",
      "bayesianTrueRate",
      "bayesianFalseRate",
      "bayesianPrior",
      "bayesianPriorLocation",
      "bayesianPriorScale",
      "bayesianPriorDf",
      "bayesianPriorAlpha",
      "bayesianPriorBeta"
    ))
    html$position <- 3
    jaspResults[["tabText"]] <- html
  }

  prior_text <- if (options$test == "bayesianOneSampleProportion") {
    if (options$bayesianPrior == "beta") {
      gettextf(
        "a Beta prior with \u03B1 = %1$s and \u03B2 = %2$s",
        options$bayesianPriorAlpha,
        options$bayesianPriorBeta
      )
    } else {
      gettextf(
        "a Moment prior with scale \u03C3 = %1$s",
        options$bayesianPriorScale
      )
    }
  } else if (options$bayesianPrior == "t-distribution") {
    gettextf(
      "a t-distribution prior with \u03BC = %1$s, \u03C3 = %2$s, and \u03BD = %3$s",
      options$bayesianPriorLocation,
      options$bayesianPriorScale,
      options$bayesianPriorDf
    )
  } else {
    gettextf(
      "a %1$s prior with \u03BC = %2$s and \u03C3 = %3$s",
      options$bayesianPrior,
      options$bayesianPriorLocation,
      options$bayesianPriorScale
    )
  }

  size_text <- if (type == "twoSample") {
    gettextf("N\u2081 = %1$s and N\u2082 = %2$s", results$n1, results$n2)
  } else {
    gettextf("N = %1$s", results$n)
  }

  calc_text <- if (options$bayesianCalculation == "sampleSize") {
    target_type <- if (options$bayesianRateType == "positive") {
      gettext("true/false positive")
    } else {
      gettext("true/false negative")
    }

    gettextf(
      "Using a Bayes factor threshold of %1$s and targeting %2$s evidence rates (true rate = %3$s, false rate = %4$s), the required design is %5$s.",
      options$bayesianThreshold,
      target_type,
      options$bayesianTrueRate,
      options$bayesianFalseRate,
      size_text
    )
  } else {
    gettextf(
      "With fixed %1$s and a Bayes factor threshold of %2$s, the estimated evidence rates are shown below.",
      size_text,
      options$bayesianThreshold
    )
  }

  rate_text <- gettextf(
    "Estimated rates: TP = %1$s, FN = %2$s, TN = %3$s, FP = %4$s.",
    format(round(results$tp, 3), nsmall = 3),
    format(round(results$fn, 3), nsmall = 3),
    format(round(results$tn, 3), nsmall = 3),
    format(round(results$fp, 3), nsmall = 3)
  )

  html[["text"]] <- paste0(
    "<p>", calc_text, "</p>",
    "<p>", gettextf("The analysis used %1$s.", prior_text), "</p>",
    "<p>", rate_text, "</p>"
  )
}
