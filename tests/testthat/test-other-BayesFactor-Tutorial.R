context("Other: BayesFactor-Tutorial")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("BayesFactorDesign (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BayesFactor-Tutorial.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesFactorDesign", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["evidenceDesignOutcome"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.648744478583904, 0.00416451934295338, 0.347091002073142, "Under H<unicode>",
     0.00442147235831613, 0.641162535194495, 0.354415992447189, "Under H<unicode>"
    ))

  plotName <- results[["results"]][["evidencePriorPlot"]][["collection"]][["evidencePriorPlot_analysis"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_analysis-prior")

  plotName <- results[["results"]][["evidencePriorPlot"]][["collection"]][["evidencePriorPlot_design"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_design-prior")

  table <- results[["results"]][["evidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["evidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> <unicode> 6", 100, 0.648744478583904, "Under H<unicode>",
     "BF<unicode><unicode> <unicode> 6", 100, 0.641162535194495,
     "Under H<unicode>"))

})

test_that("BayesFactorDesign (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BayesFactor-Tutorial.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesFactorDesign", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["evidenceDesignOutcome"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.800293936475422, 0.00141749051153053, 0.198288573013048, "Under H<unicode>",
     0.00288983975005506, 0.800156732825602, 0.196953427424343, "Under H<unicode>"
    ))

  table <- results[["results"]][["evidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["evidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> <unicode> 6", 136, 0.800293936475422, 0.8,
     "Under H<unicode>", "BF<unicode><unicode> <unicode> 6", 286,
     0.800156732825602, 0.8, "Under H<unicode>"))

})

test_that("BayesFactorSequentialDesign (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BayesFactor-Tutorial.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesFactorSequentialDesign", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["sequentialEvidenceDesignOutcome"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.717189695406689, 0.0343669319758845, 0.248443372617427, "Under H<unicode>",
     0.0152793722734242, 0.766503557732258, 0.218217069994318, "Under H<unicode>"
    ))

  table <- results[["results"]][["sequentialEvidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["sequentialEvidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.717189695406689, "BF<unicode><unicode> <unicode> 6", "Under H<unicode>",
     100, 0.766503557732258, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 100))

  table <- results[["results"]][["sequentialEvidenceSampleSizeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Under H<unicode>", 40, 7, 100, 69.3732668339601, 24.6525533769964,
     "Under H<unicode>", 40, 7, 100, 62.4205127058815, 25.0874585796706
    ))

  table <- results[["results"]][["sequentialEvidenceStagewiseTotal"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00552251400372678, 0.435271727881964, 0.559205758114309, 0.259818720124365,
     0.0196832717924163, 0.720498008083219, 1, 40, 0.00810841439145492,
     0.539728620954297, 0.452162964654248, 0.366359872541951, 0.0257925036214141,
     0.607847623836635, 2, 50, 0.009937310675174, 0.61117347372566,
     0.378889215599166, 0.456786260391837, 0.0292885644674971, 0.513925175140666,
     3, 60, 0.0114241186524994, 0.664255413507154, 0.324320467840346,
     0.535854918015572, 0.031495031083134, 0.432650050901294, 4,
     70, 0.0128211295001644, 0.706185953838143, 0.280992916661692,
     0.605377867759931, 0.0329674990726358, 0.361654633167434, 5,
     80, 0.0140527644470874, 0.739467287834522, 0.24647994771839,
     0.66535632337212, 0.0338924843611179, 0.300751192266762, 6,
     90, 0.0152793722734242, 0.766503557732258, 0.218217069994318,
     0.717189695406689, 0.0343669319758845, 0.248443372617427, 7,
     100))

})

test_that("BayesFactorSequentialDesign (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BayesFactor-Tutorial.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesFactorSequentialDesign", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["sequentialEvidenceDesignOutcome"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.828456974578152, 0.0348709550002474, 0.136672070421601, "Under H<unicode>",
     0.0169156353036729, 0.806806381629615, 0.176277983066712, "Under H<unicode>"
    ))

  table <- results[["results"]][["sequentialEvidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["sequentialEvidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.828456974578152, "BF<unicode><unicode> <unicode> 6", "Under H<unicode>",
     130, 0.8, 0.806806381629615, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 120, 0.8))

  table <- results[["results"]][["sequentialEvidenceSampleSizeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> <unicode> 6", "Under H<unicode>", 40, 10,
     130, 75.5621509946193, 33.4358530481688, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 40, 9, 120, 66.5603522056146, 31.6850149069885
    ))

  table <- results[["results"]][["sequentialEvidenceStagewiseTotal"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00552251400372678, 40, 0.435271727881964, 0.559205758114309,
     0.259818720124365, 40, 0.0196832717924163, 0.720498008083219,
     1, 0.00810841439145492, 50, 0.539728620954297, 0.452162964654248,
     0.366359872541951, 50, 0.0257925036214141, 0.607847623836635,
     2, 0.009937310675174, 60, 0.61117347372566, 0.378889215599166,
     0.456786260391837, 60, 0.0292885644674971, 0.513925175140666,
     3, 0.0114241186524994, 70, 0.664255413507154, 0.324320467840346,
     0.535854918015572, 70, 0.031495031083134, 0.432650050901294,
     4, 0.0128211295001644, 80, 0.706185953838143, 0.280992916661692,
     0.605377867759931, 80, 0.0329674990726358, 0.361654633167434,
     5, 0.0140527644470874, 90, 0.739467287834522, 0.24647994771839,
     0.66535632337212, 90, 0.0338924843611179, 0.300751192266762,
     6, 0.0152793722734242, 100, 0.766503557732258, 0.218217069994318,
     0.717189695406689, 100, 0.0343669319758845, 0.248443372617427,
     7, 0.0161454070398661, 110, 0.788087712981143, 0.195766879978991,
     0.762107038539966, 110, 0.0346564697254039, 0.20323649173463,
     8, 0.0169156353036729, 120, 0.806806381629615, 0.176277983066712,
     0.797996181632356, 120, 0.0347952666537866, 0.167208551713858,
     9, "", "", "", "", 0.828456974578152, 130, 0.0348709550002474,
     0.136672070421601, 10))

  plotName <- results[["results"]][["sequentialEvidenceStoppingProbabilitiesUnderH0Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_cumulative-decision-probabilities-by-look-design-prior-under-h-")

  plotName <- results[["results"]][["sequentialEvidenceStoppingProbabilitiesUnderH1Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_cumulative-decision-probabilities-by-look-design-prior-under-h-")

})

