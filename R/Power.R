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
  # Overwrite options for certain tests
  if (options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio") {
    options$effectSize <- options$varianceRatio
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
