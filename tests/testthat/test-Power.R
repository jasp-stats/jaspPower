library(jaspGraphs)

# Some plot snapshots slightly differ by operating system, this code will first
# decide which os-specific variant to use and will then use the correct variant
# and make sure that unused variants are not automatically deleted
running_on_mac <- tolower(Sys.info()[["sysname"]]) == "darwin"
power_contour_variant <- if (running_on_mac) "mac" else "default"
possible_power_contour_variants <- c("mac", "default")

test_variant_dependent_plot <- function (variant, all_variants, plot, name) {
  # Use the 3rd edition of test_that to use announce_snapshot_file
  testthat::local_edition(3)

  # Function to combine the variant and test/file name, just used here
  # to guarantee consistenccy
  make_variant_name <- function (variant, name) {
    paste(variant, name, sep = "-")
  }
  alternative_names <- make_variant_name(
    all_variants[all_variants != variant],
    name
  )
  actual_test_name <- make_variant_name(variant, name)

  # Register alternative files so they don't get deleted / trigger warnings
  testthat::announce_snapshot_file(paste0(alternative_names, ".svg"))

  # Run actual test
  jaspTools::expect_equal_plots(plot, actual_test_name)
}

# ==== Test Different Types of Tests ====
test_that("Results for Power Analysis for Independent Samples T-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "independentSamplesTTest"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 86, 86, 0.9))
})
test_that("Results for Power Analysis for Paired Samples T-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "pairedSamplesTTest"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 0.9, 44))
})
test_that("Results for Power Analysis for One Sample T-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "oneSampleTTest"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 0.9, 44))
})
test_that("Results for Power Analysis for One Sample Z-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "oneSampleZTest"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 0.9, 43))
})
test_that("Results for Power Analysis for One Proportion Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "oneSampleProportion"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 0.6, 0.201357920790331, 0.9, 260))
})
test_that("Results for Power Analysis for Two Proportions Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "twoSamplesProportion"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 0.6, 0.201357920790331, 519, 519, 0.9))
})
test_that("Results for Power Analysis for One Variances Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "oneSampleVarianceRatio"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 2, 0.9, 44))
})
test_that("Results for Power Analysis for Two Variances Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "twoSamplesVarianceRatio"
  options$alternative  <- "twoSided"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 2, 90, 90, 0.9))
})


# TODO: Add ANOVA here, once finished

# ==== Test full output for one more complex setting here ====

# ==== Independent samples T-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "independentSamplesTTest"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-independent-samples-t-test")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size-independent-samples-t-test")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-independent-samples-t-test")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-independent-samples-t-test")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; |<unicode>| <unicode>  0.763", "<unicode>50%",
                                      "Good chance of missing", "0.763 &lt; |<unicode>| <unicode> 1.039",
                                      "50% <unicode> 80%", "Probably detect", "1.039 &lt; |<unicode>| <unicode> 1.303",
                                      "80% <unicode> 95%", "Almost surely detect", "|<unicode>| <unicode> 1.303",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 1.18361686527406, 20, 20, 0.9))
})

# ==== Paired samples T-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "pairedSamplesTTest"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-paired-samples-t-test")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size-paired-samples-t-test")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-paired-samples-t-test")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-paired-samples-t-test")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; |<unicode>| <unicode>  0.560", "<unicode>50%",
                                      "Good chance of missing", "0.560 &lt; |<unicode>| <unicode> 0.764",
                                      "50% <unicode> 80%", "Probably detect", "0.764 &lt; |<unicode>| <unicode> 0.958",
                                      "80% <unicode> 95%", "Almost surely detect", "|<unicode>| <unicode> 0.958",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.87027940914645, 0.9, 20))
})

# ==== One sample T-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "oneSampleTTest"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-one-sample-t-test")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size-one-sample-t-test")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-one-sample-t-test")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-one-sample-t-test")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; |<unicode>| <unicode>  0.560", "<unicode>50%",
                                      "Good chance of missing", "0.560 &lt; |<unicode>| <unicode> 0.764",
                                      "50% <unicode> 80%", "Probably detect", "0.764 &lt; |<unicode>| <unicode> 0.958",
                                      "80% <unicode> 95%", "Almost surely detect", "|<unicode>| <unicode> 0.958",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.87027940914645, 0.9, 20))
})

# ==== One sample Z-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "oneSampleZTest"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-one-sample-z-test")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size-one-sample-z-test")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-one-sample-z-test")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-one-sample-z-test")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; |<unicode>| <unicode>  0.520", "<unicode>50%",
                                      "Good chance of missing", "0.520 &lt; |<unicode>| <unicode> 0.708",
                                      "50% <unicode> 80%", "Probably detect", "0.708 &lt; |<unicode>| <unicode> 0.888",
                                      "80% <unicode> 95%", "Almost surely detect", "|<unicode>| <unicode> 0.888",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.806729050354758, 0.9, 20))
})

# ==== One proportion test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "oneSampleProportion"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-one-sample-proportion")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size-one-sample-proportion")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-one-sample-proportion")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-one-sample-proportion")
})

test_that("Power by effect size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; |h| <unicode>  0.520", "<unicode>50%",
                                      "Good chance of missing", "0.520 &lt; |h| <unicode> 0.708",
                                      "50% <unicode> 80%", "Probably detect", "0.708 &lt; |h| <unicode> 0.888",
                                      "80% <unicode> 95%", "Almost surely detect", "|h| <unicode> 0.888",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.5, 0.860994285850988, 0.806672079243465, 0.9, 20))
})

# ==== Two proportions test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "twoSamplesProportion"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-two-samples-proportion")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size-two-samples-proportion")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-two-samples-proportion")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-two-samples-proportion")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; |h| <unicode>  0.736", "<unicode>50%",
                                      "Good chance of missing", "0.736 &lt; |h| <unicode> 1.002",
                                      "50% <unicode> 80%", "Probably detect", "1.002 &lt; |h| <unicode> 1.256",
                                      "80% <unicode> 95%", "Almost surely detect", "|h| <unicode> 1.256",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.5, 0.954494090806987, 1.14085014024285, 20, 20, 0.9))
})


# ==== one variance test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "oneSampleVarianceRatio"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-one-sample-variance-ratio")
})

test_that("Power Curve by variance ratio plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-variance-ratio-one-sample-variance-ratio")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-one-sample-variance-ratio")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-one-sample-variance-ratio")
})

test_that("Power by variance ratio table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "1 &lt; <unicode> <unicode>  1.974", "<unicode>50%",
                                      "Good chance of missing", "1.974 &lt; <unicode> <unicode> 2.639",
                                      "50% <unicode> 80%", "Probably detect", "2.639 &lt; <unicode> <unicode> 3.577",
                                      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 3.577",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 3.10627058910143, 0.9, 20))
})

# ==== Two variances test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "twoSamplesVarianceRatio"
options$alpha <- 0.01
options$calculation <- "effectSize"
options$alternative <- "greater"
options$powerBySampleSize <- TRUE
options$powerDemonstration <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  test_variant_dependent_plot(variant=power_contour_variant, all_variants = possible_power_contour_variants, testPlot, "power-contour-two-samples-variance-ratio")
})

test_that("Power Curve by variance ratio plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-variance-ratio-two-samples-variance-ratio")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n-two-samples-variance-ratio")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration-two-samples-variance-ratio")
})

test_that("Power by variance ratio table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "1 &lt; <unicode> <unicode>  3.027", "<unicode>50%",
                                      "Good chance of missing", "3.027 &lt; <unicode> <unicode> 4.483",
                                      "50% <unicode> 80%", "Probably detect", "4.483 &lt; <unicode> <unicode> 6.564",
                                      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 6.564",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 5.5170628379941, 20, 20, 0.9))
})


