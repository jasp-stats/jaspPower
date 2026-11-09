context("Bayes factor designs: bfpwr one-sided priors and API")

# The options of the Bayes factor analyses live in QML components that jaspTools::analysisOptions() cannot parse,
# so the tutorial analyses serve as complete option sets that each test modifies.
.bfTutorialOptions <- function(index) {
  jaspFile <- testthat::test_path("jaspfiles", "other", "BayesFactor-Tutorial.jasp")
  return(jaspTools::analysisOptions(jaspFile)[[index]])
}

.bfRun <- function(analysis, index, ..., .dataset = NULL) {
  options <- .bfTutorialOptions(index)
  changes <- list(...)
  options[names(changes)] <- changes
  return(jaspTools::runAnalysis(analysis, .dataset, options, view = FALSE, quiet = TRUE)[["results"]])
}

.bfFootnotes <- function(table) {
  return(vapply(table[["footnotes"]], function(footnote) footnote[["text"]], character(1)))
}

.bfIsEmptyCell <- function(value) {
  return(is.null(value) || identical(value, "") || isTRUE(is.na(value)))
}

.bfRCodeValues <- function(html) {
  text     <- gsub("<[^>]+>", "", html)
  entities <- c("&quot;" = "\"", "&lt;" = "<", "&gt;" = ">", "&amp;" = "&")
  for (entity in names(entities))
    text <- gsub(entity, entities[[entity]], text, fixed = TRUE)

  calls  <- strsplit(text, "# Plan for evidence for H[01]")[[1]]
  calls  <- trimws(calls[nzchar(trimws(calls))])
  values <- lapply(calls, function(call) eval(parse(text = call)))

  return(list(text = text, values = values))
}

.bfFixedZOptions <- list(
  statisticalTest             = "oneSampleZTest",
  knownStandardDeviation      = 1,
  nullValue                   = 0,
  calculationTarget           = "evidenceProbability",
  sampleSize                  = 50,
  designPriorDistribution     = "point",
  designPriorMean             = 0.5,
  designNullPriorDistribution = "point",
  designNullPriorMean         = 0
)

.bfSequentialZOptions <- list(
  statisticalTest                = "oneSampleZTest",
  knownStandardDeviation         = 1,
  nullValue                      = 0,
  calculationTarget              = "evidenceProbability",
  lookScheduleType               = "custom",
  sampleSizeSchedule             = "20, 40, 60",
  designPriorDistribution        = "point",
  designPriorMean                = 0.5,
  designNullPriorDistribution    = "point",
  designNullPriorMean            = 0,
  exactIntegrationOverAllRegions = TRUE,
  integrationMethod              = "lpmvnorm",
  integrationGridPoints          = 10000
)

test_that("Fixed z-test design uses one-sided normal analysis priors", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    analysisPriorDistribution = "normal", analysisPriorMean = 0, analysisPriorScale = 0.707,
    analysisPriorDirection = "greater", generateRCode = TRUE
  )))

  outcome <- results[["evidenceDesignOutcome"]][["data"]]
  expect_equal(outcome[[1]][["alternative"]], bfpwr::pbf01(
    k = 1/6, n = 50, usd = 1, null = 0, pm = 0, psd = 0.707, dpm = 0.5, dpsd = 0, alternative = "greater"
  ), tolerance = 1e-8)
  expect_equal(outcome[[2]][["null"]], bfpwr::pbf01(
    k = 6, n = 50, usd = 1, null = 0, pm = 0, psd = 0.707, dpm = 0, dpsd = 0, lower.tail = FALSE, alternative = "greater"
  ), tolerance = 1e-8)

  priors <- results[["evidencePriors"]][["data"]]
  expect_identical(priors[[1]][["analysisPrior"]], "Normal₊(mean = 0, sd = 0.707)")

  # the generated R code reproduces the table
  code <- .bfRCodeValues(results[["evidenceRCode"]][["rawtext"]])
  expect_match(code[["text"]], "alternative = \"greater\"", fixed = TRUE)
  expect_equal(unlist(code[["values"]]), c(outcome[[1]][["alternative"]], outcome[[2]][["null"]]), tolerance = 1e-8)
})

test_that("Fixed z-test sample size uses one-sided normal-moment analysis priors", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    calculationTarget = "sampleSize", analysisPriorDistribution = "normalMomentSpread", analysisPriorSpread = 0.4,
    analysisPriorDirection = "less", designPriorMean = -0.4, minimumSampleSize = 10, maximumSampleSize = 10000
  )))

  rows <- results[["evidenceResults"]][["data"]]
  expect_equal(rows[[1]][["n"]], bfpwr::nnmbf01(
    k = 1/6, power = 0.8, usd = 1, null = 0, psd = 0.4, dpm = -0.4, dpsd = 0, nrange = c(10, 10000), alternative = "less"
  ))
  expect_equal(rows[[2]][["n"]], bfpwr::nnmbf01(
    k = 6, power = 0.8, usd = 1, null = 0, psd = 0.4, dpm = 0, dpsd = 0, nrange = c(10, 10000), lower.tail = FALSE,
    alternative = "less"
  ))
})

test_that("One-sided point analysis priors must lie on the H1 side", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    analysisPriorDistribution = "point", analysisPriorLocation = 0.5, analysisPriorDirection = "less"
  )))

  expect_identical(
    results[["evidenceResults"]][["error"]][["errorMessage"]],
    "Unable to compute Bayes factor design: A one-sided point analysis prior must lie strictly on the H₁ side of the null value."
  )
})

test_that("Fixed t-test design does not shift the prior location twice for a nonzero null value", {
  results <- .bfRun(
    "BayesFactorDesign", 1, statisticalTest = "oneSampleTTest", sampleSize = 50, nullValue = 0.2,
    analysisPriorDistribution = "cauchy", tPriorLocation = 0.5, tPriorScale = 0.5, analysisPriorDirection = "greater",
    designPriorDistribution = "point", designPriorMean = 0.6, designNullPriorDistribution = "point", designNullPriorMean = 0.2
  )

  # the same design on the scale centered at the null value
  expected <- bfpwr::ptbf01(
    k = 1/6, n = 50, null = 0, plocation = 0.3, pscale = 0.5, pdf = 1, dpm = 0.4, dpsd = 0, type = "one.sample",
    alternative = "greater"
  )
  expect_equal(results[["evidenceDesignOutcome"]][["data"]][[1]][["alternative"]], expected, tolerance = 1e-8)
})

test_that("Observed z-test Bayes factor uses the one-sided analysis prior", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    nullValue = 0.1, analysisPriorDistribution = "normalMomentMode", analysisPriorMode = 0.6, analysisPriorDirection = "greater",
    designNullPriorMean = 0.1, zTestScale = "raw", observedDataAnalysisInput = "summaryStatistics", observedSampleSize = 60,
    observedMean = 0.35
  )))

  observed <- results[["evidenceObservedAnalysis"]][["collection"]][["evidenceObservedAnalysis_result"]][["data"]][[1]]
  expected <- bfpwr::nmbf01(estimate = 0.35, se = 1/sqrt(60), null = 0.1, psd = 0.6/sqrt(2), alternative = "greater")
  expect_equal(observed[["bf10"]], 1 / expected, tolerance = 1e-8)
})

test_that("Sequential z-test design uses one-sided normal analysis priors with a point null", {
  results <- do.call(.bfRun, c(list("BayesFactorSequentialDesign", 3), .bfSequentialZOptions, list(
    nullValue = 0.1, analysisPriorDistribution = "normal", analysisPriorMean = 0.1, analysisPriorScale = 0.707,
    analysisPriorDirection = "greater", nullPriorDistribution = "point", designPriorMean = 0.6, designNullPriorMean = 0.1
  )))

  n <- c(20, 40, 60)
  h1 <- bfpwr::pbf01seq(k1 = 1/6, k0 = 6, se = 1/sqrt(n), n = n, null = 0.1, pm = 0.1, psd = 0.707, dpm = 0.6, dpsd = 0,
                        alternative = "greater")
  h0 <- bfpwr::pbf01seq(k1 = 1/6, k0 = 6, se = 1/sqrt(n), n = n, null = 0.1, pm = 0.1, psd = 0.707, dpm = 0.1, dpsd = 0,
                        alternative = "greater")

  outcome <- results[["sequentialEvidenceDesignOutcome"]][["data"]]
  expect_equal(outcome[[1]][["alternative"]], utils::tail(h1[["cumpH1"]], 1), tolerance = 1e-6)
  expect_equal(outcome[[2]][["null"]], utils::tail(h0[["cumpH0"]], 1), tolerance = 1e-6)
})

test_that("Sequential directional z-test reflects 'less' designs around the null value", {
  results <- do.call(.bfRun, c(list("BayesFactorSequentialDesign", 3), .bfSequentialZOptions, list(
    nullValue = 0.2, analysisPriorDistribution = "normal", analysisPriorMean = 0.2, analysisPriorScale = 0.707,
    analysisPriorDirection = "less", nullPriorDistribution = "directional", designPriorMean = -0.3, designNullPriorMean = 0.2
  )))

  n <- c(20, 40, 60)
  expected <- bfpwr::pbf01seq(k1 = 1/6, k0 = 6, se = 1/sqrt(n), n = n, null = 0, pm = 0, psd = 0.707, dpm = 0.5, dpsd = 0,
                              type = "directional")

  outcome <- results[["sequentialEvidenceDesignOutcome"]][["data"]]
  expect_equal(outcome[[1]][["alternative"]], utils::tail(expected[["cumpH1"]], 1), tolerance = 1e-6)
  expect_match(results[["sequentialEvidencePriors"]][["data"]][[2]][["analysisPrior"]], "^Directional\\(")
})

test_that("Sequential maximum sample-size search supports one-sided normal-moment priors", {
  results <- do.call(.bfRun, c(list("BayesFactorSequentialDesign", 3), .bfSequentialZOptions, list(
    calculationTarget = "sampleSize", lookScheduleType = "even", numberOfLooks = 3, firstInformationFraction = 0.25,
    analysisPriorDistribution = "normalMomentMode", analysisPriorMode = 0.5, analysisPriorDirection = "greater",
    nullPriorDistribution = "point", sampleSizeSearchStrategy = "adaptive", designSampleSizeBasis = "eachDesignHypothesis",
    lowerSearchBoundForMaximumSampleSize = 10, upperSearchBoundForMaximumSampleSize = 2000
  )))

  expected <- bfpwr::nbf01seq(
    k1 = 1/6, k0 = 6, power = 0.8, usd = 1, null = 0, psd = 0.5/sqrt(2), dpm = 0.5, dpsd = 0, type = "moment",
    target = "H1", nrange = c(10, 2000), timing = c(0.25, 0.625, 1), alternative = "greater"
  )

  rows <- results[["sequentialEvidenceResults"]][["data"]]
  expect_equal(rows[[1]][["maximumN"]], expected)
})

# ---- regressions from the end-to-end review --------------------------------------------------------------------

test_that("Independent z-test sample size uses the exact allocation ratio, matching the generated R code", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    statisticalTest = "independentSamplesZTest", sampleSizeAllocationRatio = 0.35, calculationTarget = "sampleSize",
    analysisPriorDistribution = "normal", analysisPriorMean = 0, analysisPriorScale = 0.707, analysisPriorDirection = "twoSided",
    minimumSampleSize = 10, maximumSampleSize = 10000, generateRCode = TRUE
  )))

  rows <- results[["evidenceResults"]][["data"]]
  expected <- bfpwr::nbf01(k = 1/6, power = 0.8, usd = sqrt(1 + 1/0.35), null = 0, pm = 0, psd = 0.707, dpm = 0.5, dpsd = 0,
                           nrange = c(10, 10000), analytical = FALSE)
  expect_equal(rows[[1]][["n1"]], expected)
  expect_equal(rows[[1]][["n2"]], ceiling(expected * 0.35))
  expect_gte(rows[[1]][["probability"]], 0.8)

  code <- .bfRCodeValues(results[["evidenceRCode"]][["rawtext"]])
  expect_equal(unlist(code[["values"]]), c(rows[[1]][["n1"]], rows[[2]][["n1"]]))
})

test_that("Fixed sample-size search handles a target met at the minimum and point priors limited by the maximum", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    calculationTarget = "sampleSize", analysisPriorDistribution = "normal", analysisPriorDirection = "twoSided",
    designPriorMean = 1.5, minimumSampleSize = 10, maximumSampleSize = 10000
  )))
  expect_equal(results[["evidenceResults"]][["data"]][[1]][["n"]], 10)

  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    calculationTarget = "sampleSize", analysisPriorDistribution = "point", analysisPriorLocation = 0.5, analysisPriorDirection = "twoSided",
    designPriorMean = 0.2, minimumSampleSize = 10, maximumSampleSize = 50
  )))
  table <- results[["evidenceResults"]]
  expect_true(.bfIsEmptyCell(table[["data"]][[1]][["n"]]))
  expect_true(any(grepl("not reached within the selected sample-size range", .bfFootnotes(table), fixed = TRUE)))
  # no combined design exists when a planning target fails
  expect_false(any(grepl("To satisfy both design priors", .bfFootnotes(table), fixed = TRUE)))
})

test_that("Binomial sample-size search does not stop at an oscillation peak at the minimum sample size", {
  settings <- jaspPower:::.bfdPrepareSettings(list(
    statisticalTest = "oneSampleProportion", calculationTarget = "sampleSize", sampleSize = 50,
    nullProportion = 0.5, nullPriorDistribution = "point", analysisPriorSuccesses = 1, analysisPriorFailures = 1,
    binomialDesignPriorDistribution = "point", designProportion = 0.7,
    conclusiveEvidenceThresholdH1 = 6, probabilityOfConclusiveEvidenceUnderH1 = 0.8,
    minimumSampleSize = 10, maximumSampleSize = 10000
  ))
  power <- function(n) bfpwr::pbinbf01(k = 1/6, n = n, p0 = 0.5, type = "point", a = 1, b = 1, dp = 0.7)

  # the target holds at N = 74 and 77 but not at 75, 76 and 78; bfpwr's search returns 79, from where it keeps holding
  expect_true(power(74) >= 0.8 && power(75) < 0.8 && power(77) >= 0.8 && power(78) < 0.8)
  expect_equal(bfpwr::nbinbf01(k = 1/6, power = 0.8, p0 = 0.5, type = "point", a = 1, b = 1, dp = 0.7, nrange = c(10, 10000)), 79)

  for (minimumSampleSize in c(10, 74, 77, 79)) {
    settings[["minimumSampleSize"]] <- minimumSampleSize
    expect_equal(jaspPower:::.bfdFindSampleSize(settings, target = "h1", under = "h1"), 79L)
  }
})

test_that("Observed Bayes factors are computed on the log scale for very strong evidence", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    analysisPriorDistribution = "normal", analysisPriorDirection = "twoSided",
    zTestScale = "raw", observedDataAnalysisInput = "summaryStatistics", observedSampleSize = 1000, observedMean = 1.5
  )))

  observed <- results[["evidenceObservedAnalysis"]][["collection"]][["evidenceObservedAnalysis_result"]]
  expect_identical(observed[["status"]], "complete")
  expect_identical(observed[["data"]][[1]][["decision"]], "BF supports H₁")
})

test_that("Binomial dataset input rejects a success value that does not occur in the variable", {
  results <- .bfRun(
    "BayesFactorDesign", 1, statisticalTest = "oneSampleProportion", analysisPriorDistribution = "beta",
    calculationTarget = "evidenceProbability", observedDataAnalysisInput = "columns",
    observedProportionVariable = "contBinom", observedSuccessValue = "yes", .dataset = "debug.csv"
  )

  observed <- results[["evidenceObservedAnalysis"]][["collection"]][["evidenceObservedAnalysis_result"]]
  expect_match(observed[["error"]][["errorMessage"]], "does not occur in the selected variable", fixed = TRUE)
})

test_that("One-sided normal-moment priors are described by their single mode", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    nullValue = 0.1, designNullPriorMean = 0.1, analysisPriorDistribution = "normalMomentMode", analysisPriorMode = 0.5,
    analysisPriorDirection = "less"
  )))

  expect_identical(results[["evidencePriors"]][["data"]][[1]][["analysisPrior"]], "Normal-moment₋(spread = 0.3536, mode = -0.4)")
})

test_that("Sequential directional 'less' designs report the stopping boundaries on the original z scale", {
  results <- do.call(.bfRun, c(list("BayesFactorSequentialDesign", 3), .bfSequentialZOptions, list(
    analysisPriorDistribution = "normal", analysisPriorMean = 0, analysisPriorScale = 1, analysisPriorDirection = "less",
    nullPriorDistribution = "directional", designPriorMean = -0.3, stoppingBoundariesTable = TRUE
  )))

  boundaries <- results[["sequentialEvidenceBoundaries"]][["data"]][[1]]
  expect_true(.bfIsEmptyCell(boundaries[["h1Upper"]]))
  expect_true(.bfIsEmptyCell(boundaries[["h0Lower"]]))
  expect_lt(boundaries[["h1Lower"]], 0)
  expect_equal(boundaries[["h0Upper"]], -boundaries[["h1Lower"]])

  # the observed Bayes factor at the H1 boundary is the H1 threshold
  z  <- boundaries[["h1Lower"]]
  se <- 1 / sqrt(20)
  expect_equal(1 / bfpwr::dirbf01(estimate = -z * se, se = se, null = 0, pm = 0, psd = 1), 6, tolerance = 1e-4)
})

test_that("Sequential independent z designs allow small allocation ratios and use the exact ratio", {
  results <- do.call(.bfRun, c(list("BayesFactorSequentialDesign", 3), .bfSequentialZOptions, list(
    statisticalTest = "independentSamplesZTest", sampleSizeAllocationRatio = 0.3, lookScheduleType = "even",
    initialSampleSize = 2, maximumSampleSize = 6, numberOfLooks = 3, designPriorMean = 2.5,
    analysisPriorDistribution = "normal", analysisPriorMean = 0, analysisPriorScale = 1, analysisPriorDirection = "greater"
  )))

  n        <- c(2, 4, 6)
  expected <- bfpwr::pbf01seq(k1 = 1/6, k0 = 6, se = sqrt(1 + 1/0.3) / sqrt(n), n = n, null = 0, pm = 0, psd = 1, dpm = 2.5, dpsd = 0,
                              alternative = "greater")
  outcome  <- results[["sequentialEvidenceDesignOutcome"]][["data"]]
  expect_equal(outcome[[1]][["alternative"]], utils::tail(expected[["cumpH1"]], 1), tolerance = 1e-6)
})

test_that("Sequential designs blank results that depend on an unreached planning target", {
  results <- do.call(.bfRun, c(list("BayesFactorSequentialDesign", 3), .bfSequentialZOptions, list(
    calculationTarget = "sampleSize", lookScheduleType = "even", numberOfLooks = 3, firstInformationFraction = 0.25,
    analysisPriorDistribution = "normal", analysisPriorMean = 0, analysisPriorScale = 1, analysisPriorDirection = "greater",
    designPriorMean = 1, designSampleSizeBasis = "eachDesignHypothesis", sampleSizeSearchStrategy = "adaptive",
    lowerSearchBoundForMaximumSampleSize = 10, upperSearchBoundForMaximumSampleSize = 30
  )))

  outcome <- results[["sequentialEvidenceDesignOutcome"]]
  expect_false(.bfIsEmptyCell(outcome[["data"]][[1]][["alternative"]]))
  expect_true(.bfIsEmptyCell(outcome[["data"]][[2]][["null"]]))
  expect_true(any(grepl("could not be reached", .bfFootnotes(outcome), fixed = TRUE)))
})

test_that("Standardized z-tests ignore the known SD and match raw z-tests with a rescaled effect", {
  common <- c(.bfFixedZOptions, list(
    statisticalTest = "independentSamplesZTest", sampleSizeAllocationRatio = 2, sampleSize = 40,
    analysisPriorDistribution = "normal", analysisPriorMean = 0, analysisPriorScale = 0.707, analysisPriorDirection = "greater"
  ))
  common[["knownStandardDeviation"]] <- NULL
  run <- function(...) do.call(.bfRun, c(list("BayesFactorDesign", 1), common, list(...)))
  outcome <- function(results) results[["evidenceDesignOutcome"]][["data"]][[1]][["alternative"]]

  standardized <- run(zTestScale = "standardized", knownStandardDeviation = 15)
  raw          <- run(zTestScale = "raw", knownStandardDeviation = 2, analysisPriorScale = 2 * 0.707, designPriorMean = 2 * 0.5)
  expected     <- bfpwr::pbf01(k = 1/6, n = 40, usd = sqrt(1 + 1/2), null = 0, pm = 0, psd = 0.707, dpm = 0.5, dpsd = 0,
                               alternative = "greater")
  expect_equal(outcome(standardized), expected, tolerance = 1e-8)
  expect_equal(outcome(raw), expected, tolerance = 1e-8)
})

test_that("Standardized z-tests analyse Cohen's d from summary statistics", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    statisticalTest = "independentSamplesZTest", zTestScale = "standardized", analysisPriorDistribution = "normal",
    analysisPriorMean = 0, analysisPriorScale = 0.707, analysisPriorDirection = "greater",
    observedDataAnalysisInput = "summaryStatistics", observedInputType = "meansAndSDs", observedMean1 = 1.2, observedMean2 = 0.4,
    observedSd1 = 1.5, observedSd2 = 2, observedSampleSizeGroup1 = 30, observedSampleSizeGroup2 = 40
  )))

  table    <- results[["evidenceObservedAnalysis"]][["collection"]][["evidenceObservedAnalysis_result"]]
  pooledSd <- sqrt((29 * 1.5^2 + 39 * 2^2) / 68)
  se       <- sqrt(1/30 + 1/40)
  expected <- bfpwr::bf01(estimate = 0.8 / pooledSd, se = se, null = 0, pm = 0, psd = 0.707, alternative = "greater")
  expect_equal(table[["data"]][[1]][["bf10"]], 1 / expected, tolerance = 1e-8)
  expect_equal(table[["data"]][[1]][["statistic"]], 0.8 / pooledSd / se, tolerance = 1e-8)
  expect_true(any(grepl(sprintf("observed pooled standard deviation (SD = %s) is used", format(signif(pooledSd, 4))),
                        .bfFootnotes(table), fixed = TRUE)))
})

test_that("Standardized z-tests use the observed SD of dataset columns", {
  results <- do.call(.bfRun, c(list("BayesFactorDesign", 1), .bfFixedZOptions, list(
    statisticalTest = "oneSampleZTest", zTestScale = "standardized", analysisPriorDistribution = "normal",
    analysisPriorMean = 0, analysisPriorScale = 0.707, analysisPriorDirection = "twoSided",
    observedDataAnalysisInput = "columns", observedVariable = "contNormal", .dataset = "debug.csv"
  )))

  collection <- results[["evidenceObservedAnalysis"]][["collection"]]
  summary    <- collection[["evidenceObservedAnalysis_summaryStatistics"]][["data"]][[1]]
  result     <- collection[["evidenceObservedAnalysis_result"]]
  expected   <- bfpwr::bf01(estimate = summary[["mean"]] / summary[["sd"]], se = 1 / sqrt(summary[["n"]]), null = 0, pm = 0, psd = 0.707)
  expect_equal(summary[["estimate"]], summary[["mean"]] / summary[["sd"]], tolerance = 1e-8)
  expect_equal(result[["data"]][[1]][["bf10"]], 1 / expected, tolerance = 1e-6)
  expect_true(any(grepl(sprintf("observed standard deviation (SD = %s) is used", format(signif(summary[["sd"]], 4))),
                        .bfFootnotes(result), fixed = TRUE)))
})
