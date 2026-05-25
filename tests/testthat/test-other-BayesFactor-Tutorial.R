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
    list(0.648755344735423, 0.0041643515715748, 0.347080303693002, "Under H<unicode>",
     0.00442185200874173, 0.641157430484635, 0.354420717506624, "Under H<unicode>"
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
    list("BF<unicode><unicode> <unicode> 6", 100, 0.648755344735423, "Under H<unicode>",
     "BF<unicode><unicode> <unicode> 6", 100, 0.641157430484635,
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
    list(0.80030048553043, 0.00141733306850977, 0.19828218140106, "Under H<unicode>",
     0.00288675940034807, 0.800189913829316, 0.196923326770336, "Under H<unicode>"
    ))

  table <- results[["results"]][["evidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["evidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> <unicode> 6", 136, 0.80030048553043, 0.8,
     "Under H<unicode>", "BF<unicode><unicode> <unicode> 6", 286,
     0.800189913829316, 0.8, "Under H<unicode>"))

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
    list(0.717193534020241, 0.0343676884950306, 0.248438777484728, "Under H<unicode>",
     0.0152796774292087, 0.766504561527023, 0.218215761043768, "Under H<unicode>"
    ))

  table <- results[["results"]][["sequentialEvidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["sequentialEvidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.717193534020241, "BF<unicode><unicode> <unicode> 6", "Under H<unicode>",
     100, 0.766504561527023, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 100))

  table <- results[["results"]][["sequentialEvidenceSampleSizeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Under H<unicode>", 40, 7, 100, 69.3730924509874, 24.6525138629411,
     "Under H<unicode>", 40, 7, 100, 62.4203188756419, 25.0875169672277
    ))

  table <- results[["results"]][["sequentialEvidenceStagewiseTotal"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00552254122326146, 0.435282467687196, 0.559194991089543, 0.259819278848543,
     0.0196845749267326, 0.720496146224724, 1, 40, 0.00810846914988634,
     0.539733377828811, 0.452158153021303, 0.366360904559271, 0.0257935340090479,
     0.607845561431681, 2, 50, 0.0099374006844473, 0.611176415254942,
     0.378886184060611, 0.456787910584087, 0.0292895138762108, 0.513922575539702,
     3, 60, 0.0114242510790566, 0.664253040291564, 0.324322708629379,
     0.535857268400227, 0.0314956425295031, 0.432647089070269, 4,
     70, 0.0128213191804475, 0.706185866522032, 0.280992814297521,
     0.605380855841489, 0.0329681772092325, 0.361650966949278, 5,
     80, 0.0140530129296015, 0.73946995060456, 0.246477036465838,
     0.665359817846736, 0.0338932762701806, 0.300746905883084, 6,
     90, 0.0152796774292087, 0.766504561527023, 0.218215761043768,
     0.717193534020241, 0.0343676884950306, 0.248438777484728, 7,
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
    list(0.804499219174772, 0.034796409295367, 0.160704371529861, "Under H<unicode>",
     0.0167697161742349, 0.801081707013342, 0.182148576812423, "Under H<unicode>"
    ))

  table <- results[["results"]][["sequentialEvidencePriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cauchy<unicode>(location = 0, scale = 0.636)", "Point(location = 0.3)",
     "H<unicode>", "Point(location = 0)", "Point(location = 0)",
     "H<unicode>"))

  table <- results[["results"]][["sequentialEvidenceResults"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.804499219174772, "BF<unicode><unicode> <unicode> 6", "Under H<unicode>",
     121, 0.8, 0.801081707013342, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 116, 0.8))

  table <- results[["results"]][["sequentialEvidenceSampleSizeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> <unicode> 6", "Under H<unicode>", 40, 10,
     121, 74.05700094181, 31.070620307879, "BF<unicode><unicode> <unicode> 6",
     "Under H<unicode>", 40, 9, 116, 65.7770701268757, 30.3770970083329
    ))

  table <- results[["results"]][["sequentialEvidenceStagewiseTotal"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00552254122326146, 40, 0.435282467687196, 0.559194991089543,
     0.259819278848543, 40, 0.0196845749267326, 0.720496146224724,
     1, 0.00810846914988634, 50, 0.539733377828811, 0.452158153021303,
     0.366360904559271, 50, 0.0257935340090479, 0.607845561431681,
     2, 0.0099374006844473, 60, 0.611176415254942, 0.378886184060611,
     0.456787910584087, 60, 0.0292895138762108, 0.513922575539702,
     3, 0.0114242510790566, 70, 0.664253040291564, 0.324322708629379,
     0.535857268400227, 70, 0.0314956425295031, 0.432647089070269,
     4, 0.0128213191804475, 80, 0.706185866522032, 0.280992814297521,
     0.605380855841489, 80, 0.0329681772092325, 0.361650966949278,
     5, 0.0140530129296015, 90, 0.73946995060456, 0.246477036465838,
     0.665359817846736, 90, 0.0338932762701805, 0.300746905883084,
     6, 0.0152796774292086, 100, 0.766504561527023, 0.218215761043768,
     0.717193534020241, 100, 0.0343676884950306, 0.248438777484728,
     7, 0.016145761232681, 110, 0.788088631967967, 0.195765606799352,
     0.762111078634988, 110, 0.0346572265277379, 0.203231694837274,
     8, 0.0167697161742349, 116, 0.801081707013342, 0.182148576812423,
     0.798000186160256, 120, 0.0347960462371691, 0.167203767602575,
     9, "", "", "", "", 0.804499219174772, 121, 0.034796409295367,
     0.160704371529861, 10))

  plotName <- results[["results"]][["sequentialEvidenceStoppingProbabilitiesUnderH0Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_cumulative-decision-probabilities-by-look-design-prior-under-h-")

  plotName <- results[["results"]][["sequentialEvidenceStoppingProbabilitiesUnderH1Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_cumulative-decision-probabilities-by-look-design-prior-under-h-")

})

