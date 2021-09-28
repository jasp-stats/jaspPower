Power <- function(jaspResults, dataset, options) {
  if (options$test == "ttest_independent") {
    instance <- ttestISClass$new(jaspResults, options)
    instance$run()
  } else if (options$test == "ttest_paired") {

  } else if (options$test == "ttest_onesample") {

  } else if (options$test == "anova") {

  }

  return()
}

