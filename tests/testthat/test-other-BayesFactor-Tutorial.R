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
    list(0.648750199783757, 0.00416447295308893, 0.347085327263154, "Under H<unicode>",
     0.00442167224608301, 0.641161123730334, 0.354417204023583, "Under H<unicode>"
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
    list("BF<unicode><unicode> <unicode> 6", 100, 0.648750199783757, "Under H<unicode>",
     "BF<unicode><unicode> <unicode> 6", 100, 0.641161123730334,
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
    list(0.800300531585994, 0.00141748681370422, 0.198281981600302, "Under H<unicode>",
     0.00288630367663853, 0.800294166624811, 0.19681952969855, "Under H<unicode>"
    ))

  table <- results[["results"]][["evidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["evidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> <unicode> 6", 136, 0.800300531585994, 0.8,
     "Under H<unicode>", "BF<unicode><unicode> <unicode> 6", 286,
     0.800294166624811, 0.8, "Under H<unicode>"))

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
    list(0.716868115242328, 0.0338961948398939, 0.249235689917778, "Under H<unicode>",
     0.0154336464059592, 0.766435596025244, 0.218130757568796, "Under H<unicode>"
    ))

  table <- results[["results"]][["sequentialEvidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["sequentialEvidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.716868115242328, "BF<unicode><unicode> <unicode> 6", "Under H<unicode>",
     100, 0.766435596025244, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 100))

  table <- results[["results"]][["sequentialEvidenceSampleSizeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Under H<unicode>", 40, 7, 100, 69.3817615551449, 24.6622825539766,
     "Under H<unicode>", 40, 7, 100, 62.4166409363782, 25.0841264411279
    ))

  table <- results[["results"]][["sequentialEvidenceStagewiseTotal"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00552254146369468, 0.435278615105548, 0.559198843430757, 0.259819283783809,
     0.0196841074587883, 0.720496608757403, 1, 40, 0.00815124098825553,
     0.539604705646636, 0.452244053365109, 0.366536943250088, 0.0257442753304205,
     0.607718781419491, 2, 50, 0.0101557374788985, 0.610931159061885,
     0.378913103459216, 0.45694658240983, 0.0291735267250253, 0.513879890865145,
     3, 60, 0.0118032017489748, 0.664241019541737, 0.323955778709288,
     0.536079948290394, 0.0312474663255728, 0.432672585384033, 4,
     70, 0.013178684556405, 0.705886049771032, 0.280935265672563,
     0.605421342030983, 0.0325332797456515, 0.362045378223365, 5,
     80, 0.0143760853707485, 0.739206865628368, 0.246417049000884,
     0.665278565373302, 0.0333585237616439, 0.301362910865054, 6,
     90, 0.0154336464059592, 0.766435596025244, 0.218130757568796,
     0.716868115242328, 0.0338961948398939, 0.249235689917778, 7,
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
    list(0.829632685381816, 0.0345303049428599, 0.135837009675324, "Under H<unicode>",
     0.017021644380707, 0.80824517739918, 0.174733178220113, "Under H<unicode>"
    ))

  table <- results[["results"]][["sequentialEvidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["sequentialEvidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.829632685381816, "BF<unicode><unicode> <unicode> 6", "Under H<unicode>",
     130, 0.8, 0.80824517739918, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 120, 0.8))

  table <- results[["results"]][["sequentialEvidenceSampleSizeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> <unicode> 6", "Under H<unicode>", 40, 10,
     130, 75.5904052897226, 33.4621009414499, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 40, 9, 120, 66.5441005721855, 31.6642181153434
    ))

  table <- results[["results"]][["sequentialEvidenceStagewiseTotal"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00552254146369468, 40, 0.435278615105548, 0.559198843430757,
     0.259819283783809, 40, 0.0196841074587883, 0.720496608757403,
     1, 0.00815124098825553, 50, 0.539604705646636, 0.452244053365109,
     0.366536943250088, 50, 0.0257442753304205, 0.607718781419491,
     2, 0.0101557374788985, 60, 0.610931159061885, 0.378913103459216,
     0.45694658240983, 60, 0.0291735267250253, 0.513879890865145,
     3, 0.0118032017489748, 70, 0.664241019541737, 0.323955778709288,
     0.536079948290394, 70, 0.0312474663255727, 0.432672585384033,
     4, 0.013178684556405, 80, 0.705886049771032, 0.280935265672563,
     0.605421342030983, 80, 0.0325332797456515, 0.362045378223365,
     5, 0.0143760853707485, 90, 0.739206865628368, 0.246417049000884,
     0.665278565373302, 90, 0.0333585237616439, 0.301362910865054,
     6, 0.0154336464059592, 100, 0.766435596025244, 0.218130757568796,
     0.716868115242328, 100, 0.0338961948398939, 0.249235689917778,
     7, 0.0163146370188436, 110, 0.789070156969222, 0.194615206011935,
     0.761275275649696, 110, 0.0342142060294438, 0.20451051832086,
     8, 0.017021644380707, 120, 0.80824517739918, 0.174733178220113,
     0.798499638497557, 120, 0.0343821962833108, 0.167118165219132,
     9, "", "", "", "", 0.829632685381816, 130, 0.0345303049428599,
     0.135837009675324, 10))

  plotName <- results[["results"]][["sequentialEvidenceStoppingProbabilitiesUnderH0Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_cumulative-decision-probabilities-by-look-design-prior-under-h-")

  plotName <- results[["results"]][["sequentialEvidenceStoppingProbabilitiesUnderH1Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_cumulative-decision-probabilities-by-look-design-prior-under-h-")

})

