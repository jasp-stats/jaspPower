context("Other: GroupSequential-Continuous")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("GroupSequentialDesign (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Continuous.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("GroupSequentialDesign", dataset, opts)

  table <- results[["results"]][["classicalSequentialBoundaries"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.505195054510725, 0.0130224385814717, 0.5, 0.0200092537161181,
     2.05355782726248, 2, 0.905990242007318, 1.31646053515659, 1,
     0.0940097579926819, 1.31646053515659))

  plotName <- results[["results"]][["classicalSequentialBoundariesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_stopping-boundaries")

  plotName <- results[["results"]][["classicalSequentialCrossingProbabilitiesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_boundary-crossing-probabilities")

  table <- results[["results"]][["classicalSequentialCumulativeCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.505195054510725, 0.474795691773157, 0.0200092537161181, 0.0523081206240792,
     0.60959435291455, 0.338097526461371, 1, 0.904584020805008, 2.8012682867784e-09,
     0.0954159763937236, 0.170000008547201, 0, 0.830000000137848,
     2))

  table <- results[["results"]][["classicalSequentialEffectScaleConversions"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Derived from the fixed-design total sample size computed from the mean difference, common SD, and allocation ratio.",
     "Canonical <unicode> under H<unicode>", 0.2, "Mean difference divided by the common outcome SD.",
     "Standardized mean difference", 0.4))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.904584020805008, 2.8012682867784e-09, "H<unicode>", 0.0954159763937236,
     0.170000008547201, 0, "H<unicode>", 0.830000000137848))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(66.9039677197002, 1, 0.5, 133.8079354394, 2, 1))

  table <- results[["results"]][["classicalSequentialNonBindingTypeIError"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.0200092537161181, 0.0200092537161181, 2, 0.0799907463694305,
     0.100000000085549))

  table <- results[["results"]][["classicalSequentialPlanningDetails"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(49.3348416777721, 53.8441243146034, 62.4803711672111, 66.9039677197002,
     "Group 1 sample size", 49.3348416777721, 53.8441243146034, 62.4803711672111,
     66.9039677197002, "Group 2 sample size"))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.505195054510725, 0.0200092537161181, 0.0523081206240792, 0.338097526461371,
     1, 0.399388966294283, 0.0754067226776055, 0.117691887923122,
     0.491902473676477, 2))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.1, "Two-sided asymmetric <unicode>-spending (non-binding futility)",
     98.6696833555443, 107.688248629207, 124.960742334422, 1.07079978031261,
     2, 133.8079354394, "Mean difference (independent samples)",
     0.83))

})

test_that("GroupSequentialDesign (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Continuous.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("GroupSequentialDesign", dataset, opts)

  table <- results[["results"]][["classicalSequentialBoundaries"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.000571633968585956, -3.25266848844068, 0.2, 0.000571633968585956,
     3.25266848844068, 2, 0.00141305181393178, -2.98604574627367,
     0.4, 0.00141305181393178, 2.98604574627367, 3, 0.00355489765108363,
     -2.69165733872992, 0.6, 0.00355489765108362, 2.69165733872992,
     4, 0.00880623324782417, -2.37366616113389, 0.8, 0.00880623324782417,
     2.37366616113389, 5, 0.0214172142613398, -2.0253208539591, 1,
     0.0214172142613398, 2.0253208539591))

  plotName <- results[["results"]][["classicalSequentialBoundariesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_stopping-boundaries")

  plotName <- results[["results"]][["classicalSequentialCrossingProbabilitiesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_boundary-crossing-probabilities")

  table <- results[["results"]][["classicalSequentialCumulativeCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.000571633968585956, 0.998856732062828, 0.000571633968585956,
     1.18381432502879e-06, 0.962959504373221, 0.0370393118124536,
     1, 0.00184382876191139, 0.996312342476177, 0.00184382876191139,
     1.37664498905206e-06, 0.811730034742113, 0.188268588612898,
     2, 0.00467515055864694, 0.990649698882706, 0.00467515055864694,
     1.44784427627974e-06, 0.546985364763289, 0.453013187392434,
     3, 0.0109763894715343, 0.978047221056931, 0.0109763894715343,
     1.492564201422e-06, 0.277064144847161, 0.722934362588637, 4,
     0.0250000170712616, 0.949999965857477, 0.0250000170712616, 1.53644981059013e-06,
     0.0999984635755449, 0.899999999974645, 5))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0250000170712616, 0.949999965857477, "H<unicode>", 0.0250000170712616,
     1.53644981059013e-06, 0.0999984635755449, "H<unicode>", 0.899999999974645
    ))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(163.750338281451, 1, 0.2, 327.500676562901, 2, 0.4, 491.251014844352,
     3, 0.6, 655.001353125803, 4, 0.8, 818.751691407253, 5, 1))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.000571633968585956, 0.000571633968585956, 1.18381432502879e-06,
     0.0370393118124536, 1, 0.00127219479332544, 0.00127219479332544,
     1.92830664023265e-07, 0.151229276800445, 2, 0.00283132179673555,
     0.00283132179673555, 7.11992872276831e-08, 0.264744598779536,
     3, 0.00630123891288736, 0.00630123891288735, 4.47199251422661e-08,
     0.269921175196203, 4, 0.0140236275997273, 0.0140236275997273,
     4.38856091681279e-08, 0.177065637386007, 5))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.025, "Two-sided symmetric", 812.834735779667, 589.294736615513,
     800, 1.02343961425907, 5, 818.751691407253, "Fixed design value",
     0.9))

})

