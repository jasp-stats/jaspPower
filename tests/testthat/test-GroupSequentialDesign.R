context("Group sequential designs: checks beyond the worked examples")

# The options of this analysis live in QML components that jaspTools::analysisOptions() cannot parse,
# so the worked example analyses serve as complete option sets that each test modifies.
.gsdRun <- function(example, index, ...) {
  jaspFile <- testthat::test_path("jaspfiles", "other", paste0("GroupSequential-", example, ".jasp"))
  options  <- jaspTools::analysisOptions(jaspFile)[[index]]
  changes  <- list(...)
  options[names(changes)] <- changes
  return(jaspTools::runAnalysis("GroupSequentialDesign", NULL, options, view = FALSE, quiet = TRUE)[["results"]])
}

.gsdSummaryError <- function(results) {
  return(results[["classicalSequentialSummary"]][["error"]][["errorMessage"]])
}

test_that("The explanation names the design type in lower case", {
  results <- .gsdRun("Continuous", 2)

  expect_match(
    results[["classicalSequentialText"]][["rawtext"]],
    "This is a two-sided symmetric classical group sequential design with 5 planned looks", fixed = TRUE
  )
})

test_that("Invalid inputs show validation messages instead of a fatal plot error", {
  results <- .gsdRun("Continuous", 1, numberOfLooks = 3, timingMode = "custom", timing = "0.5, 0.5, 1")

  expect_match(.gsdSummaryError(results), "strictly increasing", fixed = TRUE)
  expect_match(results[["classicalSequentialBoundariesPlot"]][["error"]][["errorMessage"]], "strictly increasing", fixed = TRUE)
})

test_that("Zero follow-up is valid and the study then ends with enrollment", {
  results <- .gsdRun("Survival", 2, survivalMinimumFollowup = 0)
  rows    <- results[["classicalSequentialPlanningDetails"]][["data"]]
  study   <- Filter(function(row) identical(row[["quantity"]], "Study duration"), rows)[[1]]

  expect_equal(study[["maximum"]], 0.5)
})

test_that("Group sequential accrual code derives T and does not claim an integration grid", {
  results <- .gsdRun("Survival", 3, gridPoints = 40)
  rCode   <- results[["classicalSequentialRCode"]][["rawtext"]]

  expect_match(rCode, "T += 36")
  # gsSurv() ignores r, so the generated code must not claim a grid
  expect_false(grepl("\n  r ", rCode, fixed = TRUE))
})

test_that("Subjects + events reports a null hazard ratio other than 1 and non-finite sample sizes", {
  margin <- .gsdRun("Survival", 2, nullHazardRatio = 1.2)
  entry  <- .gsdRun("Survival", 2, survivalControlHazard = 0.1, survivalDropoutHazard = 0, survivalEntry = "expo", survivalEntryGamma = 0.1)

  expect_match(.gsdSummaryError(margin), "supports only a null hazard ratio of 1", fixed = TRUE)
  expect_match(.gsdSummaryError(entry), "returned no finite sample size", fixed = TRUE)
})

test_that("Equal alternative and null hazard ratios are reported clearly", {
  results <- .gsdRun("Survival", 1, hazardRatio = 1)

  expect_match(.gsdSummaryError(results), "must differ from the null hazard ratio", fixed = TRUE)
})
