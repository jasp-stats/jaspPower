# Originally based on https://github.com/richarddmorey/jpower

ttestISClass <- R6::R6Class(
  "ttestISClass",
  inherit = basicShimClass,
  private = list(
    #### Init + run functions ----
    .init = function() {
      private$.initPowerTab()
      private$.initPowerESTab()
    },
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
        n1 = n,
        n2 = ceiling(n_ratio * n),
        n_ratio = n_ratio,
        pow = pow,
        alt = alt,
        es = es,
        alpha = alpha
      )

      ## Compute results
      results <- private$.compute(stats)

      ## Populate tables and plots
      private$.populateIntro()
      private$.populateTabText(results, stats)
      private$.populatePowerTab(results)
      private$.preparePowerDist(results, stats)
      private$.populateContourText(results, stats)
      private$.preparePowerContour(results, stats)
      private$.preparePowerCurveES(results, stats)
      private$.populatePowerCurveESText(results, stats)
      private$.populatePowerCurveNText(results, stats)
      private$.populateDistText(results, stats)
      private$.preparePowerCurveN(results, stats)
    },

    #### Compute results ----
    .compute = function(stats) {


      ## Compute numbers for table
      pow.n <- try(ceiling(pwr.t2n.ratio(n_ratio = stats$n_ratio, d = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)), silent = TRUE)
      pow.es <- try(pwr.t2n.test(n1 = stats$n1, n2 = stats$n2, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$d, silent = TRUE)
      pow.pow <- try(pwr.t2n.test(n1 = stats$n1, n2 = stats$n2, d = stats$es, sig.level = stats$alpha, alternative = stats$alt)$power, silent = TRUE)
      #            pow.alpha = try(pwr.t2n.test(n1 = stats$n1, n2 = stats$n2, d = stats$es, sig.level = NULL, power = stats$pow, alternative = stats$alt)$sig.level, silent=TRUE)

      #            if (class(pow.alpha) == 'try-error')
      #                pow.alpha <- 0

      d50 <- pwr.t2n.test(
        n1 = stats$n1,
        n2 = stats$n2,
        sig.level = stats$alpha,
        power = .5, alternative = stats$alt
      )$d

      return(list(n1 = pow.n, n2 = ceiling(pow.n * stats$n_ratio), es = pow.es, power = pow.pow, d50 = d50))
    },

    #### Init table ----
    .initPowerTab = function() {
      table <- self$jaspResults[["powertab"]]

      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = "A Priori Power Analysis")
        table$dependOn(c(
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        self$jaspResults[["powertab"]] <- table
      }

      calc <- self$options$calc

      if (calc == "n") {
        order <- c(1, 2, 3, 4, 5)
      } else if (calc == "es") {
        order <- c(3, 1, 2, 4, 5)
      } else if (calc == "power") {
        order <- c(4, 1, 2, 3, 5)
      } else {
        order <- c(5, 1, 2, 3, 4)
      }

      colNames <- c("n1", "n2", "es", "power", "alpha", "d50")
      colLabels <- c("N\u2081", "N\u2082", "Effect Size", "Power", "\u03B1", "ES for design<br/>to have 50% power")
      colType <- c("integer", "integer", "number", "number", "number", "number")

      for (i in seq_along(order)) {
        table$addColumnInfo(colNames[order[i]],
          title = colLabels[order[i]],
          overtitle = if (calc == "n" && i > 2 || calc != "n" && i > 1) "User Defined" else NULL,
          type = colType[order[i]]
        )
      }

      row <- list()
      for (i in 2:5) {
        row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]
      }

      if (self$options$calc != "n") {
        row[["n1"]] <- self$options[["n"]]
        row[["n2"]] <- ceiling(self$options[["n"]] * self$options[["n_ratio"]])
      }

      table$addRows(rowNames = 1, row)
    },
    .initPowerESTab = function() {
      table <- self$jaspResults[["powerEStab"]]

      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = "Power by Effect Size")
        table$dependOn(c(
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        self$jaspResults[["powerEStab"]] <- table
      }

      pow <- c("\u226450%", "50% \u2013 80%", "80% \u2013 95%", "\u226595%")
      desc <- c("Likely miss", "Good chance of missing", "Probably detect", "Almost surely detect")

      table$addColumnInfo(
        name = "es",
        title = "True effect size",
        type = "number"
      )
      table$addColumnInfo(
        name = "power",
        title = "Power to detect",
        type = "string"
      )
      table$addColumnInfo(
        name = "desc",
        title = "Description",
        type = "string"
      )

      for (i in 1:4) {
        row <- list("power" = pow[i], "desc" = desc[i])
        table$addRows(rowNames = i, row)
      }
    },
    .populateIntro = function() {
      calc <- self$options$calc

      html <- self$jaspResults[["intro"]]
      if (is.null(html)) {
        html <- createJaspHtml(title = "Introduction")
        html$dependOn(c("text"))
        self$jaspResults[["intro"]] <- html
      }

      str <- paste0(
        "The purpose of a <i>power analysis</i> is to evaluate ",
        "the sensitivity of a design and test. "
      )

      if (calc == "n") {
        str <- paste0(
          str, "You have chosen to calculate the minimum sample size needed ",
          "to have an experiment sensitive enough to consistently detect the specified hypothetical effect size."
        )
      } else if (calc == "es") {
        str <- paste0(
          str, "You have chosen to calculate the minimum hypothetical effect size ",
          "for which the chosen design will have the specified sensitivity."
        )
      } else if (calc == "power") {
        str <- paste0(
          str, "You have chosen to calculate the sensitivity of the chosen design ",
          "for detecting the specified effect size."
        )
      }

      html[["text"]] <- str
    },
    .populateTabText = function(r, lst) {
      html <- self$jaspResults[["tabText"]]
      if (is.null(html)) {
        html <- createJaspHtml(title = "Table context")
        html$dependOn(c("text"))
        self$jaspResults[["tabText"]] <- html
      }

      # This table is created in init
      table <- self$jaspResults[["powerEStab"]]

      ## Get options from interface
      calc <- self$options$calc
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- ifelse(n1 == n2,
        paste0("a sample size of ", n1, " in each group "),
        paste0("group sample sizes of ", n1, " and ", n2, ", respectively, ")
      )
      tail_text <- ifelse(alt == "two.sided",
        "two-sided",
        "one-sided"
      )

      print("===> pre-calc")
      probs <- c(.5, .8, .95)
      probs_es <- sapply(probs, function(p) {
        pwr.t2n.test(
          n1 = n1, n2 = n2,
          sig.level = alpha, power = p,
          alternative = alt
        )$d
      })
      print("===> post-calc")
      print(probs_es)

      if (calc == "n") {
        str <- paste0(
          "We would need ", n_text, " to reliably (with probability greater than ",
          power, ") detect an effect size of ",
          "<i>\u03B4\u2265</i>", d, ", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>", alpha,
          "."
        )
      } else if (calc == "es") {
        str <- paste0(
          "A design with ", n_text, "will reliably (with probability greater than ",
          power, ") detect effect sizes of <i>\u03B4\u2265</i>", round(d, 3),
          ", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>", alpha,
          "."
        )
      } else if (calc == "power") {
        str <- paste0(
          "A design with ", n_text, " can detect effect sizes of ",
          "<i>\u03B4\u2265</i>", d, " with a probability of at least ",
          round(power, 3), ", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>", alpha,
          "."
        )
      }

      hypo_text <- ifelse(alt == "two.sided",
        "<i>|\u03B4|>0</i>",
        "<i>\u03B4>0</i>"
      )

      str <- paste0(
        str, "<p>To evaluate the design specified in the table, we can consider ",
        "how sensitive it is to true effects of increasing sizes; that is, are we likely to ",
        "correctly conclude that ", hypo_text, " when the effect size is large enough to care about?"
      )

      html[["text"]] <- str

      esText <- c(
        paste0("0 < \u03B4 \u2264 ", format(round(probs_es[1], 3), nsmall = 3)),
        paste0(format(round(probs_es[1], 3), nsmall = 3), " < \u03B4 \u2264 ", format(round(probs_es[2], 3), nsmall = 3)),
        paste0(format(round(probs_es[2], 3), nsmall = 3), " < \u03B4 \u2264 ", format(round(probs_es[3], 3), nsmall = 3)),
        paste0("\u03B4 \u2265 ", format(round(probs_es[3], 3), nsmall = 3))
      )

      cols <- list("es" = esText)
      # TODO: Remove this temporary fix; currently JASP get's hung up here when passing the actual contents
      cols <- list("es" = LETTERS[1:4])
      table$addColumns(cols)
    },
    #### Populate table ----
    .populatePowerTab = function(results) {
      table <- self$jaspResults[["powertab"]]

      calc <- self$options$calc

      row <- list()

      # TODO: Fix table
      row[["d50"]] <- results[["d50"]]

      if (calc == "n") {
        row[["n1"]] <- results[["n1"]]
        row[["n2"]] <- results[["n2"]]
      } else {
        row[[calc]] <- results[[calc]]
      }
      table$addRows(rowNames = 1, row)
    },

    #### Plot functions ----
    .preparePowerContour = function(r, lst) {
      image <- self$jaspResults[["powerContour"]]
      if (is.null(image)) {
        image <- createJaspPlot(title="Power Contour", width=400, height=350)
        image$dependOn(c(
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        self$jaspResults[["powerContour"]] <- image
      }

      ps <- ttestPlotSettings

      calc <- self$options$calc

      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt


      maxn <- pwr.t2n.ratio(
        n_ratio = n_ratio,
        power = max(0.9, power),
        d = d,
        sig.level = alpha,
        alternative = alt
      )

      if (n1 > maxn && n1 >= ps$maxn) {
        maxn <- ceiling(n1 * ps$max.scale)
      } else if (maxn < ps$maxn) {
        maxn <- ps$maxn
      }


      minn <- ifelse(n_ratio < 1,
        max(ceiling(3 / (1 + n_ratio)), 2 / n_ratio),
        max(ceiling(3 / (1 + n_ratio)), 2 * n_ratio)
      )

      nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))
      dd <- seq(ps$mind, ps$maxd, len = ps$lens)
      nn2 <- ceiling(n_ratio * nn)

      z.pwr <- sapply(dd, function(delta) {
        pwr.t2n.test(nn, nn2,
          d = delta,
          sig.level = alpha,
          alternative = alt
        )$power
      })

      z.delta <- sapply(nn, function(N) {
        n2 <- ceiling(n_ratio * N)
        pwr.t2n.test(N, n2,
          sig.level = alpha,
          power = power,
          alternative = alt
        )$d
      })



      state = list(
        z.pwr = z.pwr,
        z.delta = z.delta,
        ps = ps,
        nn = nn,
        dd = dd,
        n1 = n1,
        n_ratio = n_ratio,
        delta = d,
        alpha = alpha,
        minn = minn,
        maxn = maxn
      )
      image$plotObject <- private$.powerContour(state = state, ggtheme = jmvTheme())
    },
    .powerContour = function(state, ggtheme, ...) {
      calc <- self$options$calc

      z.delta <- state$z.delta
      z.pwr <- state$z.pwr
      ps <- state$ps
      pow <- state$pow
      n1 <- state$n1
      n2 <- state$n2
      alpha <- state$alpha
      dd <- state$dd
      nn <- state$nn
      ps <- state$ps
      delta <- state$delta
      n_ratio <- state$n_ratio

      # If group sizes differ, provide a secondary axis
      secondary_axis <- ggplot2::waiver()
      if (n_ratio != 1) {
        secondary_axis <- ggplot2::sec_axis(
          ~ . * n_ratio,
          name = "Sample size (group 2)"
        )
      }

      # Checking whether geom_contour_filled is availaible
      # (as it is e.g. not yet in the ggplot2 version of JASP 0.14.1)
      # TODO: Remove this check in the future
      if (!exists('geom_contour_filled', where=asNamespace('ggplot2'), mode='function')) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(title = "ERROR: The power contour plot needs a newer version of ggplot / JASP to function")
        )
      }

      p <- ggplot2::ggplot(
        transformContourMatrix(x = nn, y = dd, z = z.pwr),
        ggplot2::aes(x = x, y = y, z = z)
      ) +
        ggplot2::geom_contour_filled(bins = ps$pow.n.levels) +
        ggplot2::scale_x_log10(sec.axis = secondary_axis) +
        ggplot2::labs(
          x = "Sample size (group 1)",
          y = expression(paste("Hypothetical effect size (", delta, ")", sep = "")),
          fill = "Power"
        ) +
        guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
        # Highlight boundary of power
        # TODO: This currently goes out of bounds
        # ggplot2::annotate("line", x = nn, y = z.delta) +
        # Highlight N on axis
        ggplot2::annotate(
          "segment", x = n1, y = delta, xend = n1, yend = par()$usr[3]
        ) +
        # Highlight effect size on axis
        ggplot2::annotate(
          "segment", x = n1, y = delta, xend = par()$usr[1], yend = delta
        ) +
        # Add point highlighting intersection
        ggplot2::annotate("point", x = n1, y = delta) +
        ggtheme

      return(p)
    },
    .populateContourText = function(r, lst) {
      html <- self$jaspResults[["contourText"]]

      ## Get options from interface
      calc <- self$options$calc
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      str <- paste0(
        "<p>The power contour plot shows how the sensitivity of the ",
        "test changes with the hypothetical effect size ",
        "and the sample sizes in the design. As we increase the sample sizes, ",
        "smaller effect sizes become reliably detectable.",
        "<p>Conversely, if one is satisfied ",
        "to reliably detect only larger effect sizes, smaller sample sizes are needed. ",
        "The solid black curve on the contour plot shows sample size/effect size combinations
                       with a power of ", round(power, 3), ". The point shows the specified ",
        " design and effect size."
      )

      html[["text"]] <- str
    },
    .preparePowerCurveES = function(r, lst) {
      image <- self$jaspResults[["powerCurveES"]]
      if (is.null(image)) {
        image <- createJaspPlot(title="Power Curve by Effect Size", width=400, height=350)
        image$dependOn(c(
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        self$jaspResults[["powerCurveES"]] <- image
      }

      ps <- ttestPlotSettings

      calc <- self$options$calc

      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      dd <- seq(ps$mind, ps$maxd, len = ps$curve.n)

      y <- pwr.t2n.test(n1 = n1, n2 = n2, d = dd, sig.level = alpha, alternative = alt)$power
      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      state = list(cols = cols, dd = dd, y = y, yrect = yrect, n1 = n1, n2 = n2, alpha = alpha, delta = d, pow = power)
      image$plotObject <- private$.powerCurveES(state = state, ggtheme = jmvTheme())
    },
    .powerCurveES = function(state, ggtheme, ...) {
      y <- state$y
      cols <- state$cols
      yrect <- state$yrect
      pow <- state$pow
      n1 <- state$n1
      n2 <- state$n2
      alpha <- state$alpha
      dd <- state$dd
      delta <- state$delta

      ps <- ttestPlotSettings


      label <- paste0(
        "N\u2081 = ", n1,", N\u2082 = ", n2,", \u03B1 = ", round(alpha, 3)
      )

      p <- ggplot2::ggplot(data.frame(), ggplot2::aes(x = dd, y = y))

      # Add shaded background
      for (i in 1:ps$pow.n.levels) {
        p <- p +
          ggplot2::annotate(geom = "rect", xmin = min(dd), ymin = yrect[i], xmax = max(dd), ymax = yrect[i + 1], fill = cols[i])
      }

      p <- p +
        ggplot2::geom_line(size = 1.5) +
        ggplot2::labs(
          x = expression(
            paste("Hypothetical effect size (", delta, ")", sep = "")
          ),
          y = "Power",
          subtitle = substitute(
            paste(N[1] == n1, ", ", N[2] == n2, ", ", alpha == a),
            list(a = alpha, n1 = n1, n2 = n2)
          )
        ) +
        ggplot2::annotate(
          geom = "segment",
          x = delta, y = pow,
          xend = delta, yend = 0
        ) +
        ggplot2::annotate(
          geom = "segment",
          x = min(dd), y = pow,
          xend = delta, yend = pow,
        ) +
        ggtheme

      return(p)
    },
    .populatePowerCurveESText = function(r, lst) {
      html <- self$jaspResults[["curveESText"]]

      ## Get options from interface
      calc <- self$options$calc
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- ifelse(n1 == n2,
        paste0("sample sizes of ", n1, " in each group"),
        paste0("group sample sizes of ", n1, " and ", n2, ", respectively")
      )

      if (alt == "two.sided") {
        tail_text <- "two-sided"
        null_text <- "<i>\u03B4\u2264</i>0,"
        alt_text <- "<i>|\u03B4|\u003E</i>"
        crit_text <- "criteria"
      } else {
        tail_text <- "one-sided"
        null_text <- "<i>\u03B4=</i>0,"
        alt_text <- "<i>\u03B4\u003E</i>"
        crit_text <- "criterion"
      }

      if (calc == "power") {
        pwr_string <- paste0("have power of at least ", round(power, 3))
      } else {
        pwr_string <- paste0("only be sufficiently sensitive (power >", round(power, 3), ")")
      }

      d50 <- pwr.t2n.test(n1 = n1, n2 = n2, sig.level = alpha, power = .5, alternative = alt)$d

      str <- paste0(
        "<p>The power curve above shows how the sensitivity of the test and design ",
        "is larger for larger effect sizes. If we obtained ", n_text,
        " our test and design would ", pwr_string, " to effect sizes of ", alt_text, d, ". ",
        "<p>We would be more than likely to miss (power less than 50%) effect sizes less than <i>\u03B4=</i>",
        round(d50, 3), "."
      )

      html[["text"]] <- str
    },
    .preparePowerCurveN = function(r, lst) {
      image <- self$jaspResults[["powerCurveN"]]
      if (is.null(image)) {
        image <- createJaspPlot(title="Power Curve by N", width=400, height=350)
        image$dependOn(c(
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        self$jaspResults[["powerContour"]] <- image
      }

      calc <- self$options$calc

      ps <- ttestPlotSettings

      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      n_ratio <- lst$n_ratio
      d <- ifelse(calc == "es", r$es, lst$es)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      maxn <- pwr.t2n.ratio(
        n_ratio = n_ratio,
        power = max(0.9, power),
        d = d,
        sig.level = alpha,
        alternative = alt
      )

      if (n1 > maxn && n1 >= ps$maxn) {
        maxn <- ceiling(n1 * ps$max.scale)
      } else if (maxn < ps$maxn) {
        maxn <- ps$maxn
      }


      minn <- ifelse(n_ratio < 1,
        max(ceiling(3 / (1 + n_ratio)), 2 / n_ratio),
        max(ceiling(3 / (1 + n_ratio)), 2 * n_ratio)
      )

      nn <- seq(minn, maxn)

      y <- pwr.t2n.test(
        n1 = nn,
        n2 = ceiling(nn * lst$n_ratio),
        d = d, sig.level = alpha, alternative = alt
      )$power

      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      lims <- data.frame(
        xlim = c(minn, maxn),
        ylim = c(0, 1)
      )

      state = list(n1 = n1, cols = cols, nn = nn, y = y, yrect = yrect, lims = lims, delta = d, alpha = alpha, n_ratio = n_ratio, pow = power)
      image$plotObject <- private$.powerCurveN(state = state, ggtheme = jmvTheme())
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
      n1 <- state$n1
      y <- state$y

      ps <- ttestPlotSettings

      label <- paste0(
        " N\u2082 = ",
        round(n_ratio, 3),
        " \u00D7 N\u2081,  \u03B4 = ",
        round(delta, 3),
        ", \u03B1 = ", round(alpha, 3)
      )

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
          x = "Sample size (group 1)",
          y = "Power",
          subtitle = substitute(
            paste(delta == d, ", ", N[2] == nr %*% N[1], ", ", alpha == a),
            list(a = alpha, nr = n_ratio, d = round(delta, 3))
          )
        ) +
        ggplot2::annotate(
          geom = "segment",
          x = n1, y = pow,
        xend = n1, yend = 0,
        ) +
        ggplot2::annotate(
          geom = "segment",
          x = min(nn), y = pow,
          xend = n1, yend = pow,
        ) +
        ggtheme

      return(p)
    },
    .preparePowerDist = function(r, lst) {
      image <- self$jaspResults[["powerDist"]]
      if (is.null(image)) {
        image <- createJaspPlot(title="Power Demonstration", width=400, height=300)
        image$dependOn(c(
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        self$jaspResults[["powerDist"]] <- image
      }

      calc <- self$options$calc

      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      effN <- n1 * n2 / (n1 + n2)
      df <- n1 + n2 - 2
      ncp <- sqrt(effN) * d

      if (alt == "two.sided") {
        crit <- qt(p = 1 - alpha / 2, df = df) / sqrt(effN)
      } else {
        crit <- qt(p = 1 - alpha, df = df) / sqrt(effN)
      }

      if (lst$es > 0) {
        xlims <- c(qt(.001, df), qt(.999, df, ncp)) / sqrt(effN)
      } else {
        xlims <- c(qt(.001, df, ncp), qt(.999, df)) / sqrt(effN)
      }

      y.max <- dt(0, df) / sqrt(effN)

      xx <- seq(xlims[1], xlims[2], len = 100)
      yy.null <- dt(xx * sqrt(effN), df) / sqrt(effN)
      yy.alt <- dt(xx * sqrt(effN), df, ncp) / sqrt(effN)

      curves <- data.frame(
        x = rep(xx, 2),
        ymin = rep(0, length(xx) * 2),
        ymax = c(yy.null, yy.alt),
        group = rep(c("Null", "Alt"), each = length(xx))
      )

      if (alt == "two.sided") {
        rect <- data.frame(
          x1 = -crit, x2 = crit,
          y1 = 0, y2 = y.max * 1.1
        )
      } else {
        rect <- data.frame(
          x1 = xlims[1] - 1, x2 = crit,
          y1 = 0, y2 = y.max * 1.1
        )
      }

      lims <- data.frame(
        xlim = c(xlims[1], xlims[2]),
        ylim = c(0, y.max * 1.1)
      )

      state = list(curves = curves, rect = rect, lims = lims)
      image$plotObject <- private$.powerDist(state = state, ggtheme = jmvTheme())
    },
    .populatePowerCurveNText = function(r, lst) {
      html <- self$jaspResults[["curveNText"]]

      ## Get options from interface
      calc <- self$options$calc
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- ifelse(n1 == n2,
        paste0("sample sizes of at least ", n1, " in each group"),
        paste0("group sample sizes of at least ", n1, " and ", n2, ", respectively")
      )

      if (alt == "two.sided") {
        tail_text <- "two-sided"
        null_text <- "<i>\u03B4\u2264</i>0,"
        alt_text <- "<i>|\u03B4|\u003E</i>0,"
        crit_text <- "criteria"
      } else {
        tail_text <- "one-sided"
        null_text <- "<i>\u03B4=</i>0,"
        alt_text <- "<i>\u03B4\u2260</i>0,"
        crit_text <- "criterion"
      }

      str <- paste0(
        "<p>The power curve above shows how the sensitivity of the test and design ",
        "is larger for larger effect sizes. In order for our test and design to have sufficient sensitivity ",
        "(power > ", round(power, 3), ") to detect that ", alt_text, " when the effect size is ", d, " or larger, ",
        "we would need ", n_text, "."
      )

      html[["text"]] <- str
    },
    .powerDist = function(state, ggtheme, ...) {
      ps <- ttestPlotSettings

      if (is.null(state)) {
        return(FALSE)
      }

      curves <- state$curves
      rect <- state$rect
      lims <- state$lims
      alt <- self$options$alt

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
        ggplot2::labs(x = "Observed standardized effect size (d)", y = "Probability Density") +
        ggtheme +
        themeSpec +
        ggplot2::scale_fill_manual(values = ps$pal(5)[c(4, 1)])

      return(p)
    },
    .populateDistText = function(r, lst) {
      html <- self$jaspResults[["distText"]]

      ## Get options from interface
      calc <- self$options$calc
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "n", r$n1, lst$n1)
      n2 <- ifelse(calc == "n", r$n2, lst$n2)
      d <- ifelse(calc == "es", r$es, lst$es)
      d <- round(d, 2)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- ifelse(n1 == n2,
        paste0("a sample size of ", n1, " in each group"),
        paste0("group sample sizes of ", n1, " and ", n2, ", respectively")
      )

      if (alt == "two.sided") {
        tail_text <- "two-sided"
        null_text <- "<i>\u03B4=</i>0,"
        alt_text <- "<i>|\u03B4|\u2265</i>"
        crit_text <- "criteria"
      } else {
        tail_text <- "one-sided"
        null_text <- "<i>\u03B4\u2264</i>0,"
        alt_text <- "<i>\u03B4\u2265</i"
        crit_text <- "criterion"
      }

      str <- paste0(
        "<p>The figure above shows two sampling distributions: the sampling distribution ",
        "of the <i>estimated</i> effect size when <i>\u03B4=</i>0 (left), and when <i>\u03B4=</i>", d,
        " (right). Both assume ", n_text, ".",
        "<p>The vertical dashed lines show the ", crit_text, " we would set for a ", tail_text,
        " test with <i>α=</i>", alpha, ". When the observed effect size is far enough ",
        "away from 0 to be more extreme than the ", crit_text, " we say we 'reject' the null hypothesis. ",
        "If the null hypothesis were true and ", null_text,
        " the evidence would lead us to wrongly reject the null hypothesis at most ", 100 * alpha, "% of the time. ",
        "<p>On the other hand, if <i>\u03B4\u2265</i>", d, ", the evidence would exceed the criterion ",
        " &mdash; and hence we would correctly claim that <i>\u03B4\u2265</i>0 &mdash; at least ",
        100 * round(power, 3), "% of the time. The design's power for detecting effects of ", alt_text, d,
        " is thus ", round(power, 3), "."
      )


      html[["text"]] <- str
    }
  )
)