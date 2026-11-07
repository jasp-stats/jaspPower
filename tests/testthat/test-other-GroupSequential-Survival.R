context("Other: GroupSequential-Survival")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("GroupSequentialDesign (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Survival.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
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
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_stopping-boundaries")

  plotName <- results[["results"]][["classicalSequentialCrossingProbabilitiesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_boundary-crossing-probabilities")

  table <- results[["results"]][["classicalSequentialCumulativeCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.659907327121022, 0.337112599828425, 0.00298007305055294, 0.0268941421369995,
     0.631916032235579, 0.341189825627421, 1, 0.976071757348326,
     9.13118198414553e-09, 0.0239282335204918, 0.100000005426544,
     0, 0.900000000000008, 2))

  table <- results[["results"]][["classicalSequentialEffectScaleConversions"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Derived from the event information implied by the hazard-ratio alternative, null hazard ratio, allocation ratio, and selected survival information method.",
     "Canonical <unicode> under H<unicode>", 0.1783375))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.976071757348326, 9.13118198414553e-09, "H<unicode>", 0.0239282335204918,
     0.100000005426544, 0, "H<unicode>", 0.900000000000008))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(172.27571741328, 1, 0.5, 344.551434826561, 2, 1))

  table <- results[["results"]][["classicalSequentialNonBindingTypeIError"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.00298007305055294, 0.00298007305055294, 2, 0.0220199269495405,
     0.0250000000000934))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.659907327121022, 0.00298007305055294, 0.0268941421369995, 0.341189825627421,
     1, 0.316164430227304, 0.0209481604699389, 0.0731058632895447,
     0.558810174372587, 2))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.025, "Two-sided asymmetric <unicode>-spending (non-binding futility)",
     230.352032397778, 281.139505211618, 330.37791396381, 1.04290093333631,
     2, 344.551434826561, "Survival hazard ratio", 0.9))

})

test_that("GroupSequentialDesign (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Survival.jasp")
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
     1.18381432242312e-06, 0.96295950433698, 0.0370393118486978,
     1, 0.00184382876191139, 0.996312342476177, 0.00184382876191139,
     1.3766449858067e-06, 0.811730034573087, 0.188268588781927, 2,
     0.00467515055864694, 0.990649698882706, 0.00467515055864694,
     1.44784427273632e-06, 0.54698536445893, 0.453013187696798, 3,
     0.0109763894715343, 0.978047221056931, 0.0109763894715343, 1.49256419765973e-06,
     0.277064144549383, 0.72293436288642, 4, 0.0250000170712616,
     0.949999965857477, 0.0250000170712616, 1.53644980658803e-06,
     0.0999984634007113, 0.900000000149482, 5))

  table <- results[["results"]][["classicalSequentialEffectScaleConversions"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Derived from the event information implied by the hazard-ratio alternative, null hazard ratio, allocation ratio, and selected survival information method.",
     "Canonical <unicode> under H<unicode>", 0.3414984))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0250000170712616, 0.949999965857477, "H<unicode>", 0.0250000170712616,
     1.53644980658803e-06, 0.0999984634007113, "H<unicode>", 0.900000000149482
    ))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(18.4421257672474, 1, 0.2, 36.8842515344949, 2, 0.4, 55.3263773017423,
     3, 0.6, 73.7685030689898, 4, 0.8, 92.2106288362372, 5, 1))

  table <- results[["results"]][["classicalSequentialPlanningDetails"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(429.618859886606, 440, "Enrolled subjects", 2, 2, "Study duration"
    ))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.000571633968585956, 0.000571633968585956, 1.18381432242312e-06,
     0.0370393118486978, 1, 0.00127219479332544, 0.00127219479332544,
     1.92830663383585e-07, 0.15122927693323, 2, 0.00283132179673555,
     0.00283132179673554, 7.11992869296188e-08, 0.26474459891487,
     3, 0.00630123891288735, 0.00630123891288735, 4.47199249234051e-08,
     0.269921175189622, 4, 0.0140236275997273, 0.0140236275997273,
     4.38856089283035e-08, 0.177065637263063, 5))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.025, "Two-sided symmetric", 91.5442409619379, 66.3683981252151,
     90.0987488613899, 1.02343961488407, 5, 92.2106288362372, "Survival hazard ratio",
     0.9))

})

test_that("GroupSequentialDesign (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GroupSequential-Survival.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("GroupSequentialDesign", dataset, opts)

  table <- results[["results"]][["classicalSequentialBoundaries"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.589537438392402, 0.226355258715427, 0.25, 0.000801465082002123,
     3.15537303265962, 2, 0.805629099252381, 0.861901358294227, 0.5,
     0.00241360886536692, 2.81834323104959, 3, 0.927709523121083,
     1.45894242944619, 0.75, 0.00736344133589261, 2.43902663706031,
     4, 0.977975432564131, 2.01362295155474, 1, 0.0220245674358688,
     2.01362295155474))

  plotName <- results[["results"]][["classicalSequentialBoundariesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_stopping-boundaries")

  plotName <- results[["results"]][["classicalSequentialCrossingProbabilitiesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_boundary-crossing-probabilities")

  table <- results[["results"]][["classicalSequentialCumulativeCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.589537438392402, 0.409661096525596, 0.000801465082002123, 0.05,
     0.850457736450475, 0.0995422635495252, 1, 0.836578523522521,
     0.160449264712477, 0.00297221176500224, 0.070710679176813, 0.490473831109995,
     0.438815489713192, 2, 0.944464264615945, 0.0470549887265901,
     0.00848074665746468, 0.0866054452074165, 0.135769742350634,
     0.77762481244195, 3, 0.981317107547902, 1.80199328549124e-08,
     0.0186828744321648, 0.100001866104382, 5.62844193474632e-09,
     0.899998128267177, 4))

  table <- results[["results"]][["classicalSequentialEffectScaleConversions"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Derived from the event information implied by the hazard-ratio alternative, null hazard ratio, allocation ratio, and selected survival information method.",
     "Canonical <unicode> under H<unicode>", 0.3492576))

  table <- results[["results"]][["classicalSequentialFinalCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.981317107547902, 1.80199328549124e-08, "H<unicode>", 0.0186828744321648,
     0.100001866104382, 5.62844193474632e-09, "H<unicode>", 0.899998128267177
    ))

  table <- results[["results"]][["classicalSequentialLookSchedule"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(12.7662939235159, 28.7046782418707, 74.8581997067192, 28.70467895316,
     1, 0.25, 19.5789794936863, 57.4092439831594, 114.806001316668,
     57.4093579063199, 2, 0.5, 25.6899975682638, 86.1140366840585,
     140.729706187626, 86.1140368594799, 3, 0.75, 36, 114.81871581264,
     140.729706187626, 114.81871581264, 4, 1))

  table <- results[["results"]][["classicalSequentialNonBindingTypeIError"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.000801465082002123, 0.000801465082002123, 2, 0.00217863579520126,
     0.00298010087720338, 3, 0.00592401875196128, 0.00890411962916467,
     4, 0.0160978754141748, 0.0250019950433394))

  table <- results[["results"]][["classicalSequentialPlanningDetails"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(105.578980998531, 140.729706187626, "Enrolled subjects", 36, 36,
     "Expected analysis time"))

  table <- results[["results"]][["classicalSequentialStagewiseCrossingProbabilities"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.589537438392402, 0.000801465082002123, 0.05, 0.0995422635495252,
     1, 0.247041085130119, 0.00217074668300012, 0.0207106791768129,
     0.339273226163667, 2, 0.107885741093424, 0.00550853489246244,
     0.0158947660306035, 0.338809322728758, 3, 0.0368528429319571,
     0.0102021277747001, 0.013396420896965, 0.122373315825227, 4
    ))

  table <- results[["results"]][["classicalSequentialSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.025, "Two-sided asymmetric <unicode>-spending (non-binding futility)",
     46.4202121849102, 71.092915963869, 86.1399014000385, 1.33293298397702,
     4, 114.81871581264, "Survival hazard ratio", 0.9))

})

