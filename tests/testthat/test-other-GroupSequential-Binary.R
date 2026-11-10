context("Other: GroupSequential-Binary")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("GroupSequentialDesign (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Binary.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("GroupSequentialDesign", dataset, opts)

  table <- results[["results"]][["classicalSequentialBoundaries"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.405659799886502, -0.238724007280887, 0.333333333333333, 0.00130306171619525,
     3.01073948538569, 2, 0.826664807668243, 0.941067274680193, 0.666666666666667,
     0.00543998421371974, 2.54653056127702, 3, 0.977208066734577,
     1.99922637074817, 1, 0.0227919332654229, 1.99922637074817))

  plotName <- results[["results"]][["classicalSequentialBoundariesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_stopping-boundaries")

  plotName <- results[["results"]][["classicalSequentialCrossingProbabilitiesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_boundary-crossing-probabilities")

  table <- results[["results"]][["classicalSequentialCumulativeCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.405659799886502, 0.593037138397303, 0.00130306171619525, 0.0148337098059493,
     0.843970199050668, 0.141196091143382, 1, 0.834664289542422,
     0.159094339560075, 0.00624137089750333, 0.0437258324822218,
     0.374804425045946, 0.581469742471833, 2, 0.976695474425594,
     1.02209503430317e-08, 0.0233045153534558, 0.100000000744283,
     0, 0.900000000000274, 3))

  table <- results[["results"]][["classicalSequentialEffectScaleConversions"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Derived from the fixed-design total sample size computed from the entered event probabilities or from the baseline event probability and alternative binary effect.",
     "Canonical <unicode> under H<unicode>", 0.0756785, "Event probability in group 1 under the alternative hypothesis.",
     "Group 1 event probability", 0.15, "Event probability in group 2 under the alternative hypothesis.",
     "Group 2 event probability", 0.1))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.976695474425594, 1.02209503430317e-08, "H<unicode>", 0.0233045153534558,
     0.100000000744283, 0, "H<unicode>", 0.900000000000274))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(654.283924123754, 1, 0.333333333333333, 1308.56784824751, 2, 0.666666666666667,
     1962.85177237126, 3, 1))

  table <- results[["results"]][["classicalSequentialNonBindingTypeIError"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.00130306171619525, 0.00130306171619525, 2, 0.00494338339770296,
     0.0062464451138982, 3, 0.018753555133155, 0.0250000002470532
    ))

  table <- results[["results"]][["classicalSequentialPlanningDetails"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(573.195729489353, 725.85428390007, 917.320633928364, 981.425886185631,
     "Group 1 sample size", 573.195729489353, 725.85428390007, 917.320633928364,
     981.425886185631, "Group 2 sample size"))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.405659799886502, 0.00130306171619525, 0.0148337098059493, 0.141196091143382,
     1, 0.42900448965592, 0.00493830918130808, 0.0288921226762725,
     0.44027365132845, 2, 0.142031184883172, 0.0170631444559524,
     0.0562741682620608, 0.318530257528442, 3))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.025, "Two-sided asymmetric <unicode>-spending (non-binding futility)",
     1146.39145897871, 1451.70856780014, 1834.64126785673, 1.06988314650979,
     3, 1962.85177237126, "Two-sample binary endpoint", 0.9))

})

test_that("GroupSequentialDesign (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Binary.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("GroupSequentialDesign", dataset, opts)

  table <- results[["results"]][["classicalSequentialBoundaries"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.659907327121022, 0.412210221598643, 0.5, 0.00298007305055294,
     2.74996593183522, 2, 0.976211734324967, 1.98113147526348, 1,
     0.0237882656750334, 1.98113147526348))

  plotName <- results[["results"]][["classicalSequentialBoundariesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_stopping-boundaries")

  plotName <- results[["results"]][["classicalSequentialCrossingProbabilitiesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_boundary-crossing-probabilities")

  table <- results[["results"]][["classicalSequentialCumulativeCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.659907327121022, 0.337112599828425, 0.00298007305055294, 0.0268941421369995,
     0.631916032235579, 0.341189825627421, 1, 0.976071757348326,
     9.13118199455387e-09, 0.0239282335204918, 0.100000005426544,
     0, 0.900000000000008, 2))

  table <- results[["results"]][["classicalSequentialEffectScaleConversions"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Derived from the fixed-design total sample size computed from the entered event probabilities or from the baseline event probability and alternative binary effect.",
     "Canonical <unicode> under H<unicode>", 0.1807675, "Event probability in group 1 under the alternative hypothesis.",
     "Group 1 event probability", 0.3, "Event probability in group 2 under the alternative hypothesis.",
     "Group 2 event probability", 0.15))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.976071757348326, 9.13118199455387e-09, "H<unicode>", 0.0239282335204918,
     0.100000005426544, 0, "H<unicode>", 0.900000000000008))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(167.675190294818, 1, 0.5, 335.350380589635, 2, 1))

  table <- results[["results"]][["classicalSequentialNonBindingTypeIError"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.00298007305055294, 0.00298007305055294, 2, 0.0220199269495405,
     0.0250000000000934))

  table <- results[["results"]][["classicalSequentialPlanningDetails"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(112.100304810915, 136.815915625132, 160.777677855185, 167.675190294818,
     "Group 1 sample size", 112.100304810915, 136.815915625132, 160.777677855185,
     167.675190294818, "Group 2 sample size"))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.659907327121022, 0.00298007305055294, 0.0268941421369995, 0.341189825627421,
     1, 0.316164430227304, 0.0209481604699389, 0.0731058632895446,
     0.558810174372586, 2))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.025, "Two-sided asymmetric <unicode>-spending (non-binding futility)",
     224.20060962183, 273.631831250265, 321.555355710371, 1.04290093333631,
     2, 335.350380589635, "Two-sample binary endpoint", 0.9))

})

test_that("GroupSequentialDesign (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Binary.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("GroupSequentialDesign", dataset, opts)

  table <- results[["results"]][["classicalSequentialBoundaries"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.659907327121022, 0.412210221598642, 0.5, 0.00298007305055294,
     2.74996593183522, 2, 0.976211734324967, 1.98113147526348, 1,
     0.0237882656750334, 1.98113147526348))

  plotName <- results[["results"]][["classicalSequentialBoundariesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_stopping-boundaries")

  plotName <- results[["results"]][["classicalSequentialCrossingProbabilitiesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_boundary-crossing-probabilities")

  table <- results[["results"]][["classicalSequentialCumulativeCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.659907327121022, 0.337112599828425, 0.00298007305055294, 0.0268941421369995,
     0.631916032235579, 0.341189825627421, 1, 0.976071757348326,
     9.13118198761498e-09, 0.0239282335204918, 0.100000005426544,
     0, 0.900000000000008, 2))

  table <- results[["results"]][["classicalSequentialEffectScaleConversions"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Derived from the fixed-design total sample size computed from the entered event probabilities or from the baseline event probability and alternative binary effect.",
     "Canonical <unicode> under H<unicode>", 0.1817873, "Event probability in group 1 under the alternative hypothesis.",
     "Group 1 event probability", 0.3, "Event probability in group 2 under the alternative hypothesis.",
     "Group 2 event probability", 0.15))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.976071757348326, 9.13118198761498e-09, "H<unicode>", 0.0239282335204918,
     0.100000005426544, 0, "H<unicode>", 0.900000000000008))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(165.799036690663, 1, 0.5, 331.598073381327, 2, 1))

  table <- results[["results"]][["classicalSequentialNonBindingTypeIError"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.00298007305055294, 0.00298007305055294, 2, 0.0220199269495405,
     0.0250000000000934))

  table <- results[["results"]][["classicalSequentialPlanningDetails"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(110.845990499251, 135.285053052354, 158.978701994504, 165.799036690663,
     "Group 1 sample size", 110.845990499251, 135.285053052354, 158.978701994504,
     165.799036690663, "Group 2 sample size"))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.659907327121022, 0.00298007305055294, 0.0268941421369995, 0.341189825627421,
     1, 0.316164430227304, 0.0209481604699389, 0.0731058632895447,
     0.558810174372587, 2))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.025, "Two-sided asymmetric <unicode>-spending (non-binding futility)",
     221.691980998501, 270.570106104709, 317.957403989008, 1.04290093333631,
     2, 331.598073381327, "Two-sample binary endpoint", 0.9))

})

