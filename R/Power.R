Power <- function(jaspResults, dataset, options) {
  if (options$test == "independentSamplesTTest") {
    instance <- ttestISClass$new(jaspResults, options)
  } else if (options$test == "pairedSamplesTTest") {
    instance <- ttestPSClass$new(jaspResults, options)
  } else if (options$test == "oneSampleTTest") {
    instance <- ttestOneSClass$new(jaspResults, options)
  } else if (options$test == "oneSampleZTest") {
    instance <- ztestOneSClass$new(jaspResults, options)
  } else if (options$test == "oneSampleProportion") {
    instance <- test1PClass$new(jaspResults, options)
  } else if (options$test == "twoSamplesProportion") {
    instance <- test2PClass$new(jaspResults, options)
  } else if (options$test == "oneSampleVarianceRatio") {
    instance <- test1VarClass$new(jaspResults, options)
  } else if (options$test == "twoSamplesVarianceRatio") {
    instance <- test2VarClass$new(jaspResults, options)
  } else if (options$test == "oneSamplePoisson") {
    instance <- test2VarClass$new(jaspResults, options)
  } else if (options$test == "twoSamplesPoisson") {
    instance <- test2VarClass$new(jaspResults, options)
  } else if (options$test == "anova") {
    instance <- anovaClass$new(jaspResults, options)
  }

  if (!is.null(instance)) {
    instance$run()
  }

  return()
}

