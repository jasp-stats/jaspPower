baseClass <- R6::R6Class(
  "baseClass",
  inherit = basicShimClass,
  private = list(
    .check = function () {
      alternative <- self$options$alternative
      d <- ifelse(self$options$test == "oneSampleVarianceRatio" || self$options$test == "twoSamplesVarianceRatio", self$options$varianceRatio, self$options$effectSize)
      p0 <- self$options$baselineProportion
      p1 <- self$options$comparisonProportion
      # Check whether the provided effect size is valid
      if (self$options$test == "independentSamplesTTest" || self$options$test == "pairedSamplesTTest" ||
          self$options$test == "oneSampleTTest" || self$options$test == "oneSampleZTest") {
        if (d == 0)
          .quitAnalysis(gettext("Effect size can't be 0."))
      } else if (self$options$test == "oneSampleProportion" && self$options$calculation != "effectSize") {
        if (alternative == "twoSided") {
          if (p1 == p0) {
            .quitAnalysis(gettext("The comparison proportion can't be equal to the hypothesized proportion with a 'Two-sided' alternative hypothesis."))
          }
        } else if (alternative == "less") {
          if (p1 > p0) {
            .quitAnalysis(gettext("The comparison proportion has to be less than the hypothesized proportion with an alternative hypothesis of 'Less'."))
          }
        } else if (alternative == "greater") {
          if (p1 < p0) {
            .quitAnalysis(gettext("The comparison proportion has to be greater than the hypothesized proportion with an alternative hypothesis of 'Greater'."))
          }
        } else {
          .quitAnalysis(gettext("Invalid alternative."))
        }
      } else if (self$options$test == "twoSamplesProportion" && self$options$calculation != "effectSize") {
        if (alternative == "twoSided") {
          if (p1 == p0) {
            .quitAnalysis(gettext("The comparison proportion can't be equal to the baseline proportion with a 'Two-sided' alternative hypothesis."))
          }
        } else if (alternative == "less") {
          if (p1 > p0) {
            .quitAnalysis(gettext("The comparison proportion has to be less than the baseline proportion with an alternative hypothesis of 'Less'."))
          }
        } else if (alternative == "greater") {
          if (p1 < p0) {
            .quitAnalysis(gettext("The comparison proportion has to be greater than the baseline proportion with an alternative hypothesis of 'Greater'."))
          }
        } else {
          .quitAnalysis(gettext("Invalid alternative."))
        }
      } else if ((self$options$test == "oneSampleVarianceRatio" || self$options$test == "twoSamplesVarianceRatio") && self$options$calculation != "effectSize") {
        if (alternative == "twoSided") {
          if (d == 1) {
            .quitAnalysis(gettext("The variance ratio can't be 1 with a 'Two-sided' alternative hypothesis."))
          }
        } else if (alternative == "less") {
          if (d >= 1) {
            .quitAnalysis(gettext("The variance ratio has to be less than 1 with an alternative hypothesis 'Less'."))
          }
        } else if (alternative == "greater") {
          if (d <= 1) {
            .quitAnalysis(gettext("The variance ratio has to be greater than 1 with an alternative hypothesis of 'Greater'."))
          }
        } else {
          .quitAnalysis(gettext("Invalid alternative."))
        }
      }
    },
    #### Init + run functions ----
    .run = function() {
      ## Get options from interface
      if (self$options$test == "oneSampleVarianceRatio" || self$options$test == "twoSamplesVarianceRatio")
        self$options$effectSize <- self$options$varianceRatio
      n <- self$options$sampleSize
      n_ratio <- self$options$sampleSizeRatio
      pow <- self$options$power
      alt <- self$options$alternative
      p0 <- self$options$baselineProportion
      p1 <- self$options$comparisonProportion
      es <- self$options$effectSize
      alpha <- self$options$alpha

      if (pow >= 1) .quitAnalysis(gettext("Power must be less than 1."))

      stats <- list(
        # Independent samples
        n1 = n,
        n2 = ceiling(n_ratio * n),
        # One sample
        n = n,
        # t and z tests, variance ratio tests
        es = es,
        # Proportion tests
        p0 = p0,
        p1 = p1,
        # Shared
        n_ratio = n_ratio,
        pow = pow,
        alt = switch(alt,
                     "twoSided" = "two.sided",
                     alt),
        alpha = alpha
      )

      ## Compute results

      results <- try(private$.compute(stats))
      if(inherits(results, "try-error"))
        .quitAnalysis(gettext("Unable to compute the power results. Try to enter less extreme values for the input parameters."))

      private$.initPowerTab(results, stats)

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
      if (self$options$powerByEffectSize) {
        private$.preparePowerCurveES(results, stats)
        if (self$options$text) {
          private$.populatePowerCurveESText(results, stats)
        }
      }
      if (self$options$powerBySampleSize) {
        private$.preparePowerCurveN(results, stats)
        if (self$options$text) {
          private$.populatePowerCurveNText(results, stats)
        }
      }
      if (self$options$powerDemonstration) {
        private$.preparePowerDist(results, stats)
        if (self$options$text) {
          private$.populateDistText(results, stats)
        }
      }
      if (self$options$saveDataset && self$options$savePath != "") {
        private$.generateDataset(results, stats)
      }
    },

    # ==== Functions to Populate Text ====
    .populateIntro = function() {
      calc <- self$options$calculation

      html <- self$jaspResults[["intro"]]
      if (is.null(html)) {
        html <- createJaspHtml(title = "Introduction")
        html$dependOn(c("test", "text"))
        html$position <- 1
        self$jaspResults[["intro"]] <- html
      }

      str <- gettext(
        "The purpose of a <i>power analysis</i> is to evaluate the sensitivity of a design and test. "
      )

      test_names <- c(
        independentSamplesTTest = gettext("an independent samples t-test"),
        pairedSamplesTTest = gettext("a paired samples t-test"),
        oneSampleTTest = gettext("a one sample t-test"),
        oneSampleZTest = gettext("a one sample z-test"),
        oneSampleProportion = gettext("a one sample proportions test"),
        twoSamplesProportion = gettext("a two samples proportions test"),
        oneSampleVarianceRatio = gettext("a one sample variance test"),
        twoSamplesVarianceRatio = gettext("a two samples variance test"),
        oneSamplePoisson = gettext("a one sample Poisson rate test"),
        twoSamplesPoisson = gettext("a two samples Poisson rate test")
      )
      test_sentence_end <- gettextf(
        " when using <i>%s</i>.", test_names[[self$options$test]]
      )

      if (calc == "sampleSize") {
        mid_sentence <- gettextf(
          "You have chosen to calculate the minimum sample size needed to have an experiment sensitive enough to consistently detect the specified hypothetical effect size"
        )
      } else if (calc == "effectSize") {
        mid_sentence <- gettextf(
          "You have chosen to calculate the minimum hypothetical effect size for which the chosen design will have the specified sensitivity"
        )
      } else if (calc == "power") {
        mid_sentence <- gettextf(
          "You have chosen to calculate the sensitivity of the chosen design for detecting the specified effect size"
        )
      }
      str <- paste0(
        str,
        mid_sentence,
        test_sentence_end
      )

      html[["text"]] <- str
    },

    # ==== Plotting Functions ====
    .powerContour = function(state, ggtheme, ...) {
      calc <- self$options$calculation

      z.delta <- state$z.delta
      z.pwr <- state$z.pwr
      ps <- state$ps
      pow <- state$pow
      n <- state$n
      n1 <- state$n1
      n2 <- state$n2
      alpha <- state$alpha
      dd <- state$dd
      nn <- state$nn
      ps <- state$ps
      delta <- state$delta
      n_ratio <- state$n_ratio

      if (is.null(n_ratio)) {
        # Use ratio of 1 for one or paired samples
        n_ratio <- 1
      }

      if (is.null(n)) {
        # Add n for indipendent samples
        n <- n1
      }
      if (self$options$test == "independentSamplesTTest" || self$options$test == "pairedSamplesTTest" ||
           self$options$test == "oneSampleTTest" || self$options$test == "oneSampleZTest")
        es <- paste0("|", "\u03B4", "|")
      if (self$options$test == "oneSampleProportion" || self$options$test == "twoSamplesProportion")
        es <- "|h|"
      if (self$options$test == "oneSampleVarianceRatio" || self$options$test == "twoSamplesVarianceRatio")
        es <- "\u03C1"


      # If group sizes differ, provide a secondary axis
      secondary_axis <- ggplot2::waiver()
      if (n_ratio != 1) {
        secondary_axis <- ggplot2::sec_axis(
          ~ . * n_ratio,
          name = gettext("Sample size (group 2)")
        )
      }

      # Checking whether geom_contour_filled is availaible
      # (as it is e.g. not yet in the ggplot2 version of JASP 0.14.1)
      # TODO: Remove this check in the future
      if (!exists("geom_contour_filled", where = asNamespace("ggplot2"), mode = "function")) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(title = "ERROR: The power contour plot needs a newer version of ggplot / JASP to function")
        )
      }

      # Dirty fix for a bug where ggplot is unable to properly bin values of 1...
      z.pwr[z.pwr == 1] <- 0.999999999

      p <- ggplot2::ggplot(
        transformContourMatrix(x = nn, y = dd, z = z.pwr),
        ggplot2::aes(x = x, y = y, z = z)
      ) +
        ggplot2::geom_contour_filled(breaks = pretty(c(0, 1), n = ps$pow.n.levels), alpha = ps$background.alpha) +
        ggplot2::scale_x_log10(sec.axis = secondary_axis) +
        ggplot2::labs(
          x = gettext("Sample size (group 1)"),
          y = gettextf("Hypothetical effect size (%s)", es),
          fill = gettext("Power")
        ) +
        ggplot2::guides(fill = ggplot2::guide_colorsteps(barheight = ggplot2::unit(7, "cm"))) +
        # Highlight boundary of power
        # Note: This doesn't render quite perfectly right now
        # ggplot2::scale_y_continuous(limits = c(min(dd), max(dd))) +
        # ggplot2::annotate("line", x = nn, y = z.delta) +
        # Highlight N on axis
        .segment(
          x = n, y = delta, xend = n, yend = min(dd)
        ) +
        # Highlight effect size on axis
        .segment(
          x = n, y = delta, xend = min(nn), yend = delta
        ) +
        # Add point highlighting intersection
        ggplot2::annotate("point", x = n, y = delta, size = 3) +
        ggtheme

      return(p)
    },
    .powerCurveES = function(state, ggtheme, ...) {
      y <- state$y
      cols <- state$cols
      yrect <- state$yrect
      pow <- state$pow
      n1 <- state$n1
      n2 <- state$n2
      n <- state$n
      alpha <- state$alpha
      dd <- state$dd
      delta <- state$delta

      if (self$options$test == "independentSamplesTTest" || self$options$test == "pairedSamplesTTest" ||
          self$options$test == "oneSampleTTest" || self$options$test == "oneSampleZTest")
        es <- paste0("|", "\u03B4", "|")
      if (self$options$test == "oneSampleProportion" || self$options$test == "twoSamplesProportion")
        es <- "|h|"
      if (self$options$test == "oneSampleVarianceRatio" || self$options$test == "twoSamplesVarianceRatio")
        es <- "\u03C1"

      ps <- ttestPlotSettings

      if (is.null(n2)) {
        # Paired or one sample
        plot_subtitle <- substitute(
          paste(N == n, ", ", alpha == a),
          list(a = alpha, n = n)
        )
      } else {
        # Indipendent Samples
        plot_subtitle <- substitute(
          paste(N[1] == n1, ", ", N[2] == n2, ", ", alpha == a),
          list(a = alpha, n1 = n1, n2 = n2)
        )
      }

      p <- ggplot2::ggplot(data.frame(), ggplot2::aes(x = dd, y = y))

      # Add shaded background
      for (i in 1:ps$pow.n.levels) {
        p <- p +
          ggplot2::annotate(geom = "rect", xmin = min(dd), ymin = yrect[i], xmax = max(dd), ymax = yrect[i + 1], fill = cols[i])
      }

      p <- p +
        ggplot2::geom_line(size = 1.5) +
        ggplot2::labs(
          x = gettextf("Hypothetical effect size (%s)", es),
          y = gettext("Power"),
          subtitle = plot_subtitle
        ) +
        .segment(
          x = delta, y = pow,
          xend = delta, yend = 0
        ) +
        .segment(
          x = min(dd), y = pow,
          xend = delta, yend = pow,
        ) +
        ggtheme

      return(p)
    },
    .powerCurveN = function(state, ggtheme, ...) {
      cols <- state$cols
      yrect <- state$yrect
      lims <- state$lims
      delta <- state$delta
      alpha <- state$alpha
      n_ratio <- state$n_ratio
      nn <- state$nn
      pow <- state$pow
      n <- state$n
      y <- state$y

      if (self$options$test == "independentSamplesTTest" || self$options$test == "pairedSamplesTTest" ||
          self$options$test == "oneSampleTTest" || self$options$test == "oneSampleZTest")
        es <- paste0("|", "\u03B4", "|")
      if (self$options$test == "oneSampleProportion" || self$options$test == "twoSamplesProportion")
        es <- "|h|"
      if (self$options$test == "oneSampleVarianceRatio" || self$options$test == "twoSamplesVarianceRatio")
        es <- "\u03C1"

      ps <- ttestPlotSettings

      if (is.null(n_ratio)) {
        # Paired or one sample
        plot_subtitle <- gettextf("%s = %s, %s = %s", es, round(delta, 3), "\u03B1", alpha)
      } else {
        # Indipendent Samples
        plot_subtitle <- gettextf("%s = %s, N%s = %s %s N%s, %s = %s", es, round(delta, 3),"\u2082", n_ratio, "\u00D7", "\u2081", "\u03B1", alpha)
      }

      # Creat basic plot
      p <- ggplot2::ggplot(data.frame(), ggplot2::aes(x = nn, y = y))

      # Add shaded background
      for (i in 1:ps$pow.n.levels) {
        p <- p +
          ggplot2::annotate(geom = "rect", xmin = lims$xlim[1], ymin = yrect[i], xmax = lims$xlim[2], ymax = yrect[i + 1], fill = cols[i])
      }

      # Add main plot
      p <- p +
        ggplot2::geom_line(size = 1.5) +
        ggplot2::scale_x_log10(limits = lims$xlim) +
        ggplot2::scale_y_continuous(limits = lims$ylim) +
        ggplot2::labs(
          x = gettext("Sample size (group 1)"),
          y = gettext("Power"),
          subtitle = plot_subtitle
        ) +
        .segment(
          x = n, y = pow,
          xend = n, yend = 0
        ) +
        .segment(
          x = min(nn), y = pow,
          xend = n, yend = pow,
        ) +
        ggtheme

      return(p)
    },
    .powerDist = function(state, ggtheme, ...) {
      if (self$options$test == "independentSamplesTTest" || self$options$test == "pairedSamplesTTest" ||
          self$options$test == "oneSampleTTest" || self$options$test == "oneSampleZTest")
        es <- paste0("|", "\u03B4", "|")
      if (self$options$test == "oneSampleProportion" || self$options$test == "twoSamplesProportion")
        es <- "|h|"
      if (self$options$test == "oneSampleVarianceRatio" || self$options$test == "twoSamplesVarianceRatio")
        es <- ifelse(state$d < 1, "1/\u03C1", "\u03C1")

      ps <- ttestPlotSettings

      if (is.null(state)) {
        return(FALSE)
      }

      curves <- state$curves
      rect <- state$rect
      lims <- state$lims
      alt <- self$options$alternative

      themeSpec <- ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "none"
      )

      p <- ggplot2::ggplot() +
        ggplot2::geom_ribbon(data = curves, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = group), color = "#333333", alpha = .6) +
        ggplot2::geom_rect(data = rect, ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "white", alpha = 0.4) +
        ggplot2::geom_vline(data = rect, ggplot2::aes(xintercept = x1), linetype = "dashed") +
        ggplot2::geom_vline(data = rect, ggplot2::aes(xintercept = x2), linetype = "dashed") +
        ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE) +
        ggplot2::labs(x = gettextf("Observed effect size (%s)", es), y = gettext("Probability Density")) +
        ggtheme +
        themeSpec +
        ggplot2::scale_fill_manual(values = ps$pal(5)[c(4, 1)])

      return(p)
    }
  )
)
