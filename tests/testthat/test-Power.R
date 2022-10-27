# ==== Test Different Types of Tests ====
test_that("Results for Power Analysis for Independent Samples T-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "ttest_independent"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 86, 86, 0.9))
})
test_that("Results for Power Analysis for Paired Samples T-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "ttest_paired"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 44, 0.9))
})
test_that("Results for Power Analysis for One Sample T-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "ttest_onesample"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 44, 0.9))
})
test_that("Results for Power Analysis for One Sample Z-Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "ztest_onesample"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 43, 0.9))
})
test_that("Results for Power Analysis for One Proportion Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "test_oneprop"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 43, 0.9))
})
test_that("Results for Power Analysis for Two Proportions Test match", {
  options <- jaspTools::analysisOptions("Power")
  options$test <- "test_twoprop"

  results <- jaspTools::runAnalysis("Power", NULL, options)
  table <- results[["results"]][["powertab"]][["data"]]

  jaspTools::expect_equal_tables(table, list(0.05, 0.5, 85, 85, 0.9))
})


# TODO: Add ANOVA here, once finished

# ==== Test full output for one more complex setting here ====

# ==== Independent samples T-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "ttest_independent"
options$alpha <- 0.01
options$calc <- "es"
options$alt <- "greater"
options$powerCurveN <- TRUE
options$powerDist <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-contour")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; <unicode> <unicode>  0.763", "<unicode>50%",
                                      "Good chance of missing", "0.763 &lt; <unicode> <unicode> 1.039",
                                      "50% <unicode> 80%", "Probably detect", "1.039 &lt; <unicode> <unicode> 1.303",
                                      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 1.303",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 1.18361686527406, 20, 20, 0.9))
})

# ==== Paired samples T-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "ttest_paired"
options$alpha <- 0.01
options$calc <- "es"
options$alt <- "greater"
options$powerCurveN <- TRUE
options$powerDist <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
	# Power Contour plot differs by OS, so skip the test for now on windows / linux
	skip_on_os(c("windows", "linux"))

	plotName <- results[["results"]][["powerContour"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "power-contour")
})

test_that("Power Curve by Effect Size plot matches", {
	plotName <- results[["results"]][["powerCurveES"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size")
})

test_that("Power Curve by N plot matches", {
	plotName <- results[["results"]][["powerCurveN"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "power-curve-by-n")
})

test_that("Power Demonstration plot matches", {
	plotName <- results[["results"]][["powerDist"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "power-demonstration")
})

test_that("Power by Effect Size table results match", {
	table <- results[["results"]][["powerEStab"]][["data"]]
	jaspTools::expect_equal_tables(table,
  list("Likely miss", "0 &lt; <unicode> <unicode> 0.560", "<unicode>50%",
      "Good chance of missing", "0.560 &lt; <unicode> <unicode> 0.764",
      "50% <unicode> 80%", "Probably detect", "0.764 &lt; <unicode> <unicode> 0.958",
      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 0.958",
      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
	table <- results[["results"]][["powertab"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.01, 0.87027940914645, 20, 0.9))
})

# ==== One sample T-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "ttest_onesample"
options$alpha <- 0.01
options$calc <- "es"
options$alt <- "greater"
options$powerCurveN <- TRUE
options$powerDist <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-contour")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; <unicode> <unicode>  0.560", "<unicode>50%",
                                      "Good chance of missing", "0.560 &lt; <unicode> <unicode> 0.764",
                                      "50% <unicode> 80%", "Probably detect", "0.764 &lt; <unicode> <unicode> 0.958",
                                      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 0.958",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.87027940914645, 20, 0.9))
})

# ==== One sample Z-test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "ztest_onesample"
options$alpha <- 0.01
options$calc <- "es"
options$alt <- "greater"
options$powerCurveN <- TRUE
options$powerDist <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-contour")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; <unicode> <unicode>  0.520", "<unicode>50%",
                                      "Good chance of missing", "0.520 &lt; <unicode> <unicode> 0.708",
                                      "50% <unicode> 80%", "Probably detect", "0.708 &lt; <unicode> <unicode> 0.888",
                                      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 0.888",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.806729050354758, 20, 0.9))
})

# ==== One proportion test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "test_oneprop"
options$alpha <- 0.01
options$calc <- "es"
options$alt <- "greater"
options$powerCurveN <- TRUE
options$powerDist <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-contour")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; <unicode> <unicode>  0.520", "<unicode>50%",
                                      "Good chance of missing", "0.520 &lt; <unicode> <unicode> 0.708",
                                      "50% <unicode> 80%", "Probably detect", "0.708 &lt; <unicode> <unicode> 0.888",
                                      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 0.888",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 0.806729050354758, 20, 0.9))
})

# ==== Two proportions test ====

options <- jaspTools::analysisOptions("Power")
options$test <- "test_twoprop"
options$alpha <- 0.01
options$calc <- "es"
options$alt <- "greater"
options$powerCurveN <- TRUE
options$powerDist <- TRUE

# The tests below were auto-generated with runAnalysis(makeTests = T)

results <- jaspTools::runAnalysis("Power", NULL, options)

test_that("Power Contour plot matches", {
  plotName <- results[["results"]][["powerContour"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-contour")
})

test_that("Power Curve by Effect Size plot matches", {
  plotName <- results[["results"]][["powerCurveES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-effect-size")
})

test_that("Power Curve by N plot matches", {
  plotName <- results[["results"]][["powerCurveN"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-curve-by-n")
})

test_that("Power Demonstration plot matches", {
  plotName <- results[["results"]][["powerDist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-demonstration")
})

test_that("Power by Effect Size table results match", {
  table <- results[["results"]][["powerEStab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Likely miss", "0 &lt; <unicode> <unicode>  0.736", "<unicode>50%",
                                      "Good chance of missing", "0.736 &lt; <unicode> <unicode> 1.002",
                                      "50% <unicode> 80%", "Probably detect", "1.002 &lt; <unicode> <unicode> 1.256",
                                      "80% <unicode> 95%", "Almost surely detect", "<unicode> <unicode> 1.256",
                                      "<unicode>95%"))
})

test_that("A Priori Power Analysis table results match", {
  table <- results[["results"]][["powertab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.01, 1.14091909274051, 20, 20, 0.9))
})


