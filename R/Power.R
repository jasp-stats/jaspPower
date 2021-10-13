Power <- function(jaspResults, dataset, options) {
  if (options$test == "ttest_independent") {
    instance <- ttestISClass$new(jaspResults, options)
  } else if (options$test == "ttest_paired") {
    instance <- ttestPSClass$new(jaspResults, options)
  } else if (options$test == "ttest_onesample") {
    instance <- ttestOneSClass$new(jaspResults, options)
  } else if (options$test == "anova") {
    instance <- anovaClass$new(jaspResults, options)
  }

  if (!is.null(instance)) {
    instance$run()
  }

  return()
}

