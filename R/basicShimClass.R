basicShimClass <- R6::R6Class("basicShimClass", list(
    options = NULL,
    jaspResults = NULL,

    # Constructor
    initialize = function (jaspResults, options) {
      # Pass options
      self$options <- options
      self$jaspResults <- jaspResults

      private$.init()
    },

    # Run analyses
    run = function () {
      private$.run()
    }
  )
)