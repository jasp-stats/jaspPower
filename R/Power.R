Power <- function(jaspResults, dataset, options) {
  if (options$test == "ttest_independent") {
    instance <- ttestISClass$new(jaspResults, options)
  } else if (options$test == "ttest_paired") {
    instance <- ttestPSClass$new(jaspResults, options)
  } else if (options$test == "ttest_onesample") {
    instance <- ttestOneSClass$new(jaspResults, options)
  } else if (options$test == "ztest_onesample") {
    instance <- ztestOneSClass$new(jaspResults, options)
  } else if (options$test == "test_oneprop") {
    instance <- test1PClass$new(jaspResults, options)
  } else if (options$test == "test_twoprop") {
    instance <- test2PClass$new(jaspResults, options)
  } else if (options$test == "test_onevar") {
    instance <- test1VarClass$new(jaspResults, options)
  } else if (options$test == "test_twovar") {
    instance <- test2VarClass$new(jaspResults, options)
  } else if (options$test == "anova") {
    instance <- anovaClass$new(jaspResults, options)
  }

  if (!is.null(instance)) {
    instance$run()
  }

  return()
}

