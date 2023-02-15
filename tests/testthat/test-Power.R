library(jaspGraphs)

# ==== Test Different Types of Tests ====
test_that("Results for Power Analysis for Indipendent Samples T-Test match", {
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
# TODO: Add ANOVA here, once finished

# ==== Test full output for one more complex setting here ====
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
