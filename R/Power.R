Power <- function(jaspResults, dataset, options) {
  options <- .checkOptions(options)

  # Run the appropriate test
  if (options$test == "independentSamplesTTest") {
    .runTtestIS(jaspResults, options)
  } else if (options$test == "pairedSamplesTTest") {
    .runTtestPS(jaspResults, options)
  } else if (options$test == "oneSampleTTest") {
    .runTtestOneS(jaspResults, options)
  } else if (options$test == "oneSampleZTest") {
    .runZtestOneS(jaspResults, options)
  } else if (options$test == "oneSampleProportion") {
    .runTest1P(jaspResults, options)
  } else if (options$test == "twoSamplesProportion") {
    .runTest2P(jaspResults, options)
  } else if (options$test == "oneSampleVarianceRatio") {
    .runTest1Var(jaspResults, options)
  } else if (options$test == "twoSamplesVarianceRatio") {
    .runTest2Var(jaspResults, options)
  } else if (options$test == "bayesianOneSampleTTest") {
    .runBayesianOneSampleT(jaspResults, options)
  } else if (options$test == "bayesianIndependentSamplesTTest") {
    .runBayesianIndependentSamplesT(jaspResults, options)
  } else if (options$test == "bayesianOneSampleProportion") {
    .runBayesianOneSampleProportion(jaspResults, options)
  } else if (options$test == "oneSamplePoisson") {
    .runTest2Var(jaspResults, options)
  } else if (options$test == "twoSamplesPoisson") {
    .runTest2Var(jaspResults, options)
  } else if (options$test == "anova") {
    .runAnova(jaspResults, options)
  }

  return()
}

# Check options and overwrite certain settings if necessary
.checkOptions <- function(options) {
  isBayesian <- options$test %in% c(
    "bayesianOneSampleTTest",
    "bayesianIndependentSamplesTTest",
    "bayesianOneSampleProportion"
  )

  # Overwrite options for certain tests
  if (options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio") {
    options$effectSize <- options$varianceRatio
  }

  if (isBayesian) {
    isMissingScalar <- function(x) {
      is.null(x) || length(x) == 0 || is.na(x) || (is.character(x) && !nzchar(x))
    }

    if (options$bayesianThreshold <= 1) {
      .quitAnalysis(gettext("The Bayes factor evidence threshold must be larger than 1."))
    }

    if (options$bayesianCalculation == "sampleSize") {
      if (options$bayesianTrueRate <= 0.6 || options$bayesianTrueRate >= 0.999) {
        .quitAnalysis(gettext("The target true rate must be strictly between 0.6 and 0.999."))
      }
      if (options$bayesianFalseRate <= 0.001 || options$bayesianFalseRate >= 0.1) {
        .quitAnalysis(gettext("The target false rate must be strictly between 0.001 and 0.1."))
      }
    }

    if (options$test == "bayesianIndependentSamplesTTest") {
      if (options$sampleSizeRatio <= 0) {
        .quitAnalysis(gettext("Sample size ratio must be larger than 0."))
      }
      if (options$bayesianCalculation == "evidenceRates" &&
        ceiling(options$sampleSize * options$sampleSizeRatio) < 2) {
        .quitAnalysis(gettext("The second-group sample size must be at least 2. Increase N or the sample size ratio."))
      }
    }

    if (options$test == "bayesianOneSampleProportion") {
      if (isMissingScalar(options$bayesianPrior)) {
        options$bayesianPrior <- "beta"
      }

      if (options$baselineProportion < 0.1 || options$baselineProportion > 0.9) {
        .quitAnalysis(gettext("The null proportion must be between 0.1 and 0.9."))
      }
      if (options$bayesianPrior == "beta") {
        if (options$bayesianPriorAlpha <= 0 || options$bayesianPriorBeta <= 0) {
          .quitAnalysis(gettext("For the Beta prior, both prior shape parameters must be greater than 0."))
        }
      } else if (options$bayesianPrior == "Moment") {
        if (options$bayesianPriorScale <= 0) {
          .quitAnalysis(gettext("For the Moment prior, the prior scale must be greater than 0."))
        }
      } else {
        .quitAnalysis(gettext("Invalid Bayesian prior for the one sample proportion test."))
      }
    } else {
      if (isMissingScalar(options$bayesianPrior)) {
        options$bayesianPrior <- "Normal"
      }

      if (!(options$bayesianPrior %in% c("Normal", "Moment", "t-distribution"))) {
        .quitAnalysis(gettext("Invalid Bayesian prior for this t-test."))
      }
      if (options$bayesianPriorScale <= 0) {
        .quitAnalysis(gettext("The prior scale must be greater than 0."))
      }
      if (options$bayesianPrior == "t-distribution" && options$bayesianPriorDf <= 0) {
        .quitAnalysis(gettext("For a t-distribution prior, prior degrees of freedom must be greater than 0."))
      }
    }

    return(options)
  }

  # Check options for problems
  alternative <- options$alternative
  d <- ifelse(options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio", options$varianceRatio, options$effectSize)
  p0 <- options$baselineProportion
  p1 <- options$comparisonProportion
  # Check whether the provided effect size is valid
  if (options$test == "independentSamplesTTest" || options$test == "pairedSamplesTTest" ||
    options$test == "oneSampleTTest" || options$test == "oneSampleZTest") {
    if (d == 0) {
      .quitAnalysis(gettext("Effect size can't be 0."))
    }
  } else if (options$test == "oneSampleProportion" && options$calculation != "effectSize") {
    if (alternative == "twoSided") {
      if (p1 == p0) {
        .quitAnalysis(gettext("The comparison proportion can't be equal to the hypothesized proportion with a 'Two-sided' alternative hypothesis."))
      }
    } else if (alternative == "less") {
      if (p1 > p0) {
        .quitAnalysis(gettext("The comparison proportion has to be less than the hypothesized proportion with an alternative hypothesis of 'Less'."))
      }
    } else if (alternative == "greater") {
      if (p1 < p0) {
        .quitAnalysis(gettext("The comparison proportion has to be greater than the hypothesized proportion with an alternative hypothesis of 'Greater'."))
      }
    } else {
      .quitAnalysis(gettext("Invalid alternative."))
    }
  } else if (options$test == "twoSamplesProportion" && options$calculation != "effectSize") {
    if (alternative == "twoSided") {
      if (p1 == p0) {
        .quitAnalysis(gettext("The comparison proportion can't be equal to the baseline proportion with a 'Two-sided' alternative hypothesis."))
      }
    } else if (alternative == "less") {
      if (p1 > p0) {
        .quitAnalysis(gettext("The comparison proportion has to be less than the baseline proportion with an alternative hypothesis of 'Less'."))
      }
    } else if (alternative == "greater") {
      if (p1 < p0) {
        .quitAnalysis(gettext("The comparison proportion has to be greater than the baseline proportion with an alternative hypothesis of 'Greater'."))
      }
    } else {
      .quitAnalysis(gettext("Invalid alternative."))
    }
  } else if ((options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio") && options$calculation != "effectSize") {
    if (alternative == "twoSided") {
      if (d == 1) {
        .quitAnalysis(gettext("The variance ratio can't be 1 with a 'Two-sided' alternative hypothesis."))
      }
    } else if (alternative == "less") {
      if (d >= 1) {
        .quitAnalysis(gettext("The variance ratio has to be less than 1 with an alternative hypothesis 'Less'."))
      }
    } else if (alternative == "greater") {
      if (d <= 1) {
        .quitAnalysis(gettext("The variance ratio has to be greater than 1 with an alternative hypothesis of 'Greater'."))
      }
    } else {
      .quitAnalysis(gettext("Invalid alternative."))
    }
  }

  return(options)
}
