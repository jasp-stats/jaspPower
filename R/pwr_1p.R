# Originally based on https://github.com/richarddmorey/jpower

test1PClass <- R6::R6Class(
  "test1PClass",
  inherit = tTestBaseClass,
  private = list(
    #### Member variables ----
    probs_es = NULL,

    #### Compute results ----
    .compute = function(stats) {
      ## Compute numbers for table
      pow.n <- ceiling(pwr.p.test(p0 = stats$p0, p = stats$p1, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)$n)
      pow.p <- pwr.p.test(n = stats$n, p0 = stats$p0, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$p
      pow.pow <- pwr.p.test(n = stats$n, p0 = stats$p0, p = stats$p1, sig.level = stats$alpha, alternative = stats$alt)$power

      # Calculate probs_es here to have access to stats list
      probs <- c(.5, .8, .95)
      probs_es <- sapply(probs, function(pr) {
        pwr.p.test(
          n = stats$n, p0 = stats$p0, sig.level = stats$alpha, power = pr,
          alternative = stats$alt
        )$p
      })
      private$probs_es <- probs_es

      return(list(n = pow.n, p = pow.p, power = pow.pow))
    },

    #### Init table ----
    .initPowerTab = function(results) {
      table <- self$jaspResults[["powertab"]]
      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = gettext("A Priori Power Analysis"))
        table$dependOn(c(
          "test",
          "p0",
          "p1",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        table$position <- 2
        self$jaspResults[["powertab"]] <- table
      } else {
        return()
      }

      calc <- self$options$calc

      if (calc == "n") {
        order <- c(1, 2, 3, 4, 5, 6)
      } else if (calc == "es") {
        order <- c(3, 4, 1, 2, 5, 6)
      } else if (calc == "power") {
        order <- c(5, 1, 2, 3, 4, 6)
      } else {
        order <- c(6, 1, 2, 3, 4, 5)
      }

      colNames <- c("n", "p0", "p1","h", "power", "alpha")
      colLabels <- c(
        "N",
        gettext("Hypothesized proportion"),
        gettext("Comparison proportion"),
        gettext("Cohen's <i>h</i>"),
        gettext("Power"),
        "\u03B1"
      )
      colType <- c("integer", "number", "number", "number", "number", "number")

      for (i in seq_along(order)) {
        table$addColumnInfo(colNames[order[i]],
                            title = colLabels[order[i]],
                            overtitle = ifelse((calc == "es" && i > 2) || (calc != "es" && i > 1), gettext("User Defined"), NULL),
                            type = colType[order[i]]
        )
      }

      row <- list()
      for (i in 2:4) {
        row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]
      }

      table$addRows(rowNames = 1, row)

      private$.populatePowerTab(results)
    },
    .initPowerESTab = function(results, stats) {
      table <- self$jaspResults[["powerEStab"]]
      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = gettext("Power by effect size"))
        table$dependOn(c(
          "test",
          "p0",
          "p1",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "text"
        ))
        table$position <- 4
        self$jaspResults[["powerEStab"]] <- table
      } else {
        return()
      }

      table$addColumnInfo(
        name = "es",
        title = ifelse(self$options$esType == "h", gettext("True effect size (Cohen's <i>h</i>)"), gettext("True difference in proportions (p\u2080 - p\u2081")),
        type = "string"
      )
      table$addColumnInfo(
        name = "power",
        title = gettext("Power to detect"),
        type = "string"
      )
      table$addColumnInfo(
        name = "desc",
        title = gettext("Description"),
        type = "string"
      )

      pow <- c("\u226450%", "50% \u2013 80%", "80% \u2013 95%", "\u226595%")
      desc <- c(
        gettext("Likely miss"),
        gettext("Good chance of missing"),
        gettext("Probably detect"),
        gettext("Almost surely detect")
      )

      for (i in 1:4) {
        row <- list("power" = pow[i], "desc" = desc[i])
        table$addRows(rowNames = i, row)
      }

      private$.populatePowerESTab()
    },

    #### Populate texts ----
    .populateTabText = function(r, lst) {
      html <- self$jaspResults[["tabText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text"))
        html$position <- 3
        self$jaspResults[["tabText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      p0 <- lst$p0
      p1 <- ifelse(calc == "es", r$p, lst$p1)
      d <- ifelse(self$options$esType == "h", 2 * (asin(sqrt(p0)) - asin(sqrt(p1))), abs(p1 - p0))
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- gettextf("a sample size of ", n)

      tail_text <- ifelse(alt == "two.sided",
                          gettext("two-sided"),
                          gettext("one-sided")
      )
      dType_text <- ifelse(self$options$esType == "h",
                           gettext("effect sizes of <i>h</i>"),
                           gettext("absolute differences in proportions of p\u2080 - p\u2081"))

      if (calc == "n") {
        str <- gettextf(
          "We would need %s to reliably (with probability greater than %s) detect %s<i>%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, power, dType_text, "\u2265", d, tail_text, alpha
        )
      } else if (calc == "es") {
        str <- gettextf(
          "A design with %s will reliably (with probability greater than %s) detect  %s<i>%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, power, dType_text, "\u2265", round(d, 3), tail_text, alpha
        )
      } else if (calc == "power") {
        str <- gettextf(
          "A design with %s can detect %s<i>%s</i>%s with a probability of at least %s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, dType_text, "\u2265", d, round(power, 3), tail_text, alpha
        )
      }

      hypo_text <- ifelse(alt == "two.sided",
                          "<i>|\u03B4|>0</i>",
                          "<i>\u03B4>0</i>"
      )
      dType_text <- ifelse(self$options$esType == "h",
                           gettext("effects"),
                           gettext("absolute differences in proportions"))

      str <- paste0(
        str,
        gettextf(
          "<p>To evaluate the design specified in the table, we can consider how sensitive it is to true %s of increasing sizes; that is, are we likely to correctly conclude that %s when the effect size is large enough to care about?",
          dType_text, hypo_text
        )
      )

      html[["text"]] <- str
    },
    .populateContourText = function(r, lst) {
      html <- self$jaspResults[["contourText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerContour"))
        html$position <- 6
        self$jaspResults[["contourText"]] <- html
      }

      calc <- self$options$calc

      ## Get options from interface
      power <- ifelse(calc == "power", r$power, lst$pow)
      dType_text <- ifelse(self$options$esType == "h",
                           gettext("effect size"),
                           gettext("absolute difference in proportions"))

      str <- gettextf(
        "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical %s and the sample sizes in the design. As we increase the sample sizes, smaller effect sizes become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only larger effect sizes, smaller sample sizes are needed. The solid black curve on the contour plot shows sample size/%s combinations with a power of %s. The point shows the specified  design and %s.",
        dType_text, dType_text, round(power, 3), dType_text
      )

      html[["text"]] <- str
    },
    .populatePowerCurveESText = function(r, lst) {
      html <- self$jaspResults[["curveESText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerCurveES"))
        html$position <- 8
        self$jaspResults[["curveESText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      p0 <- lst$p0
      p1 <- ifelse(calc == "es", r$p, lst$p1)
      d <- ifelse(self$options$esType == "h", 2 * (asin(sqrt(p0)) - asin(sqrt(p1))), abs(p1 - p0))
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- gettextf("sample sizes of %s", n)

      dType_text <- ifelse(self$options$esType == "h",
                           gettext("effect sizes"),
                           gettext("absolute differences in proportions"))

      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- "<i>\u03B4\u2264</i>0,"
        alt_text <- "<i>|\u03B4|\u003E</i>"
        crit_text <- "criteria"
      } else {
        tail_text <- gettext("one-sided")
        null_text <- "<i>\u03B4=</i>0,"
        alt_text <- "<i>\u03B4\u003E</i>"
        crit_text <- "criterion"
      }

      if (calc == "power") {
        pwr_string <- gettextf("have power of at least %s", round(power, 3))
      } else {
        pwr_string <- gettextf("only be sufficiently sensitive (power >%s)", round(power, 3))
      }

      p50 <- pwr.p.test(n = n, sig.level = alpha, power = .5, alternative = alt)$p
      d50 <- ifelse(self$options$esType == "h", 2 * (asin(sqrt(p0)) - asin(sqrt(p50))), abs(p50 - p0))

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger %s. If we obtained %s our test and design would %s to %s of %s%s. <p>We would be more than likely to miss (power less than 50%%) %s less than <i>%s=</i>%s.",
        dType_text, n_text, pwr_string, dType_text, alt_text, d,dType_text, "\u03B4", round(d50, 3)
      )

      html[["text"]] <- str
    },
    .populatePowerCurveNText = function(r, lst) {
      html <- self$jaspResults[["curveNText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerCurveN"))
        html$position <- 10
        self$jaspResults[["curveNText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      p0 <- lst$p0
      p1 <- ifelse(calc == "es", r$p, lst$p1)
      d <- ifelse(self$options$esType == "h", 2 * (asin(sqrt(p0)) - asin(sqrt(p1))), abs(p1 - p0))
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alt <- lst$alt

      n_text <- gettextf("sample sizes of at least %s", n)

      dType_text <- ifelse(self$options$esType == "h",
                           gettext("effect sizes"),
                           gettext("absolute differences in proportions"))

      dType_text_2 <- ifelse(self$options$esType == "h",
                           gettext("effect size"),
                           gettext("absolute difference in proportions"))


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

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger %s. In order for our test and design to have sufficient sensitivity (power > %s) to detect that %s when the %s is %s or larger, we would need %s.",
        dType_text, round(power, 3), alt_text,dType_text_2, d, n_text
      )

      html[["text"]] <- str
    },
    .populateDistText = function(r, lst) {
      html <- self$jaspResults[["distText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerDist"))
        html$position <- 12
        self$jaspResults[["distText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      p0 <- lst$p0
      p1 <- ifelse(calc == "es", r$p, lst$p1)
      d <- ifelse(self$options$esType == "h", 2 * (asin(sqrt(p0)) - asin(sqrt(p1))), abs(p1 - p0))
      d <- round(d, 2)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- gettextf("a sample size of %s", n)

      dType_text <- ifelse(self$options$esType == "h",
                           gettext("effect sizes"),
                           gettext("absolute differences in proportions"))

      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- "<i>\u03B4=</i>0,"
        alt_text <- "<i>|\u03B4|\u2265</i>"
        crit_text <- gettext("criteria")
      } else {
        tail_text <- gettext("one-sided")
        null_text <- "<i>\u03B4\u2264</i>0,"
        alt_text <- "<i>|\u03B4|\u2265</i>"
        crit_text <- gettext("criterion")
      }

      str <- paste(
        "<p>",
        gettextf("The figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> effect size when <i>%s=</i>0 (left), and when <i>%s=</i>%s (right).", "\u03B4", "\u03B4", d),
        gettextf("Both assume %s.", n_text),
        "</p><p>",
        gettextf("The vertical dashed lines show the %s we would set for a %s test with <i>α=</i>%s.", crit_text, tail_text, alpha),
        gettextf("When the observed effect size is far enough away from 0 to be more extreme than the %s we say we 'reject' the null hypothesis.", crit_text),
        gettextf("If the null hypothesis were true and %s the evidence would lead us to wrongly reject the null hypothesis at most %s%% of the time.", null_text, 100 * alpha),
        "</p><p>",
        gettextf("On the other hand, if <i>%s%s</i>%s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%s%s</i>0 &mdash; at least %s%% of the time.", "\u03B4", "\u2265", d, "\u03B4", "\u2265", 100 * round(power, 3)),
        gettextf("The design's power for detecting effects of %s%s is thus %s.", alt_text, d, round(power, 3)),
        "</p>"
      )


      html[["text"]] <- str
    },

    #### Populate table ----
    .populatePowerTab = function(results) {
      table <- self$jaspResults[["powertab"]]
      if (self$options$calc == "es") {
        calc <- "p1"
        calc2 <- "h"
        p0 <- self$options$p0
        p1 <- results$p
        d <- 2 * (asin(sqrt(p0)) - asin(sqrt(p1)))

        row <- list()
        row2 <- list()
        row[[calc]] <- p1
        row2[[calc2]] <- d
        table$addColumns(row)
        table$addColumns(row2)

      } else {
        calc <- self$options$calc

        row <- list()
        row[[calc]] <- results[[calc]]
        table$addColumns(row)
      }
    },
    .populatePowerESTab = function() {
      table <- self$jaspResults[["powerEStab"]]

      p0 <- rep(self$options$p0, 4)
      probs_es <- ifelse(self$options$esType == "h", 2 * (asin(sqrt(private$probs_es)) - asin(sqrt(p0))), abs(private$probs_es - p0))

      dType_text <- ifelse(self$options$esType == "h", gettext("<i>h</i>"), gettext("|\u0394p|"))

      esText <- c(
        gettextf("0 < %s %s  %s", dType_text, "\u2264", format(round(probs_es[1], 3), nsmall = 3)),
        gettextf("%s < %s %s %s", format(round(probs_es[1], 3), nsmall = 3), dType_text, "\u2264", format(round(probs_es[2], 3), nsmall = 3)),
        gettextf("%s < %s %s %s",format(round(probs_es[2], 3), nsmall = 3), dType_text, "\u2264", format(round(probs_es[3], 3), nsmall = 3)),
        gettextf("%s %s %s", dType_text, "\u2265", format(round(probs_es[3], 3), nsmall = 3))
      )

      cols <- list("es" = esText)
      table$addColumns(cols)
    },

    #### Plot functions ----
    .preparePowerContour = function(r, lst) {
      image <- self$jaspResults[["powerContour"]]
      if (is.null(image)) {
        image <- createJaspPlot(title=gettext("Power Contour"), width=400, height=350)
        image$dependOn(c(
          "test",
          "p0",
          "p1",
          "es"
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerContour"
        ))
        image$position <- 5
        self$jaspResults[["powerContour"]] <- image
      }

      ps <- ttestPlotSettings
      ps$maxd <- 1

      calc <- self$options$calc

      n <- ifelse(calc == "n", r$n, lst$n)
      p0 <- lst$p0
      p1 <- ifelse(calc == "es", r$p, lst$p1)
      d <- 2 * (asin(sqrt(p0)) - asin(sqrt(p1)))
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      if (n >= ps$maxn) {
        maxn <- ceiling(n * ps$max.scale)
      } else {
        maxn <- ps$maxn
      }

      minn <- 3

      nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))
      dd <- seq(ps$mind, ps$maxd, len = ps$lens)

      z.pwr <- sapply(dd, function(delta) {
        pwr.p.test(n = nn, p = delta, sig.level = alpha, alternative = alt)$power
      })

      z.delta <- sapply(nn, function(N) {
        pwr.p.test(n = N, sig.level = alpha, power = power, alternative = alt)$p
      })

      state = list(
        z.pwr = z.pwr,
        z.delta = z.delta,
        ps = ps,
        nn = nn,
        dd = dd,
        n = n,
        delta = d,
        alpha = alpha,
        minn = minn,
        maxn = maxn
      )
      image$plotObject <- private$.powerContour(state = state, ggtheme = pwr_plot_theme())
    },
    .preparePowerCurveES = function(r, lst) {
      image <- self$jaspResults[["powerCurveES"]]
      if (is.null(image)) {
        image <- createJaspPlot(
          title = gettext("Power Curve by Effect Size"),
          width = 400,
          height = 350
        )
        image$dependOn(c(
          "test",
          "p0",
          "p1",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerCurveES"
        ))
        image$position <- 7
        self$jaspResults[["powerCurveES"]] <- image
      }

      ps <- ttestPlotSettings
      ps$maxd <- 1

      calc <- self$options$calc

      n <- ifelse(calc == "n", r$n, lst$n)
      d <- ifelse(calc == "es", r$p, lst$p1)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      dd <- seq(ps$mind, ps$maxd, len = ps$curve.n)

      y <- pwr.p.test(n = n, p = dd, sig.level = alpha, alternative = alt)$power
      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      state = list(cols = cols, dd = dd, y = y, yrect = yrect, n = n, alpha = alpha, delta = d, pow = power)
      image$plotObject <- private$.powerCurveES(state = state, ggtheme = pwr_plot_theme())
    },
    .preparePowerCurveN = function(r, lst) {
      image <- self$jaspResults[["powerCurveN"]]
      if (is.null(image)) {
        image <- createJaspPlot(
          title = gettext("Power Curve by N"),
          width = 400,
          height = 350
        )
        image$dependOn(c(
          "test",
          "p0",
          "p1",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerCurveN"
        ))
        image$position <- 9
        self$jaspResults[["powerCurveN"]] <- image
      }

      calc <- self$options$calc

      ps <- ttestPlotSettings

      n <- ifelse(calc == "n", r$n, lst$n)
      d <- ifelse(calc == "es", r$p, lst$p1)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      if (n >= ps$maxn) {
        maxn <- ceiling(n * ps$max.scale)
      } else {
        maxn <- ps$maxn
      }

      minn <- 3

      nn <- seq(minn, maxn)

      y <- pwr::pwr.p.test(n = nn, h = d, sig.level = alpha, alternative = alt)$power

      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      lims <- data.frame(
        xlim = c(minn, maxn),
        ylim = c(0, 1)
      )

      state = list(
        n = n,
        cols = cols,
        nn = nn,
        y = y,
        yrect = yrect,
        lims = lims,
        delta = d,
        alpha = alpha,
        pow = power
      )
      image$plotObject <- private$.powerCurveN(state = state, ggtheme = pwr_plot_theme())
    },
    .preparePowerDist = function(r, lst) {
      image <- self$jaspResults[["powerDist"]]
      if (is.null(image)) {
        image <- createJaspPlot(
          title = gettext("Power Demonstration"),
          width = 400,
          height = 300
        )
        image$dependOn(c(
          "test",
          "p1",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerDist"
        ))
        image$position <- 11
        self$jaspResults[["powerDist"]] <- image
      }

      calc <- self$options$calc

      n <- ifelse(calc == "n", r$n, lst$n)
      d <- ifelse(calc == "es", r$p, lst$p1)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      effN <- n
      df <- n - 1
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
      image$plotObject <- private$.powerDist(state = state, ggtheme = pwr_plot_theme())
    }
  )
)
