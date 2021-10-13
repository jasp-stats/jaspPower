tTestBaseClass <- R6::R6Class(
  "tTestBaseClass",
  inherit = basicShimClass,
  private = list(
    #### Init + run functions ----
    .run = function() {

      ## Get options from interface
      n <- self$options$n
      n_ratio <- self$options$n_ratio
      pow <- self$options$power
      alt <- self$options$alt
      es <- self$options$es
      alpha <- self$options$alpha

      if (pow >= 1) stop("Power must be less than 1.")

      stats <- list(
        # Independentend samples
        n1 = n,
        n2 = ceiling(n_ratio * n),
        # Rest
        n = n,
        # Shared
        n_ratio = n_ratio,
        pow = pow,
        alt = alt,
        es = es,
        alpha = alpha
      )

      ## Compute results
      results <- private$.compute(stats)

      private$.initPowerTab(results)

      if (self$options$text) {
        private$.initPowerESTab(results, stats)
      }

      ## Populate tables and plots
      if (self$options$text) {
        private$.populateIntro()
      }

      if (self$options$powerContour) {
        private$.preparePowerContour(results, stats)
        if (self$options$text) {
          private$.populateContourText(results, stats)
        }
      }
      if (self$options$powerCurveES) {
        private$.preparePowerCurveES(results, stats)
        if (self$options$text) {
          private$.populatePowerCurveESText(results, stats)
        }
      }
      if (self$options$powerCurveN) {
        private$.preparePowerCurveN(results, stats)
        if (self$options$text) {
          private$.populatePowerCurveNText(results, stats)
        }
      }
      if (self$options$powerDist) {
        private$.preparePowerDist(results, stats)
        if (self$options$text) {
          private$.populateDistText(results, stats)
        }
      }
    }
  )
)
