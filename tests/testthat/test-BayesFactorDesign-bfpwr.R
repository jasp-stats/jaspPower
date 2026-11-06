context("Bayes factor designs: bfpwr one-sided priors and API")

# The options of the Bayes factor analyses live in QML components that jaspTools::analysisOptions() cannot parse,
# so the tutorial analyses serve as complete option sets that each test modifies.
.bfTutorialOptions <- function(index) {
  jaspFile <- testthat::test_path("jaspfiles", "other", "BayesFactor-Tutorial.jasp")
  return(jaspTools::analysisOptions(jaspFile)[[index]])
}

.bfRun <- function(analysis, index, ...) {
  options <- .bfTutorialOptions(index)
  changes <- list(...)
  options[names(changes)] <- changes
  return(jaspTools::runAnalysis(analysis, NULL, options, view = FALSE, quiet = TRUE)[["results"]])
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
    designNullPriorMean = 0.1, observedDataAnalysisInput = "summaryStatistics", observedSampleSize = 60, observedMean = 0.35
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
    calculationTarget = "sampleSize", lookScheduleType = "equal", numberOfLooks = 3, firstInformationFraction = 1/3,
    analysisPriorDistribution = "normalMomentMode", analysisPriorMode = 0.5, analysisPriorDirection = "greater",
    nullPriorDistribution = "point", sampleSizeSearchStrategy = "adaptive", designSampleSizeBasis = "eachDesignHypothesis",
    lowerSearchBoundForMaximumSampleSize = 10, upperSearchBoundForMaximumSampleSize = 2000
  )))

  expected <- bfpwr::nbf01seq(
    k1 = 1/6, k0 = 6, power = 0.8, usd = 1, null = 0, psd = 0.5/sqrt(2), dpm = 0.5, dpsd = 0, type = "moment",
    target = "H1", nrange = c(10, 2000), timing = c(1/3, 2/3, 1), alternative = "greater"
  )

  rows <- results[["sequentialEvidenceResults"]][["data"]]
  expect_equal(rows[[1]][["maximumN"]], expected)
})
