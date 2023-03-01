# Originally based on https://github.com/richarddmorey/jpower

test2PClass <- R6::R6Class(
  "test2PClass",
  inherit = baseClass,
  private = list(
    # Functions are called from .run in the parent class

    #### Compute results ----
    .compute = function(stats) {
      ## Compute numbers for table
      pow.n <- NULL
      pow.p1 <- NULL
      pow.pow <- NULL
      if(self$options$calculation == "sampleSize")
        pow.n <- ceiling(pwr.2p2n.test(n.ratio = stats$n_ratio, p0 = stats$p0, p1 = stats$p1, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)$n)
      if(self$options$calculation == "effectSize")
        pow.p1 <- pwr.2p2n.test(p0 = stats$p0, n = stats$n1, n.ratio = stats$n2/stats$n1, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$p1
      if(self$options$calculation == "power")
        pow.pow <- pwr.2p2n.test(n = stats$n1, n.ratio = stats$n2/ stats$n1, p0 = stats$p0, p1 = stats$p1, sig.level = stats$alpha, alternative = stats$alt)$power

      return(list(n1 = pow.n, n2 = ceiling(pow.n * stats$n_ratio), p1 = pow.p1, power = pow.pow))
    },

    #### Init table ----
    .initPowerTab = function(results, stats) {
      table <- self$jaspResults[["powertab"]]

      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = gettext("A Priori Power Analysis"))
        table$dependOn(c(
          "test",
          "baselineProportion",
          "comparisonProportion",
          "effectSize",
          "effectDirection",
          "power",
          "sampleSize",
          "alternative",
          "alpha",
          "calculation",
          "sampleSizeRatio"
        ))
        table$position <- 2
        self$jaspResults[["powertab"]] <- table
      } else {
        return()
      }

      calc <- self$options$calculation

      if (calc == "sampleSize") {
        order <- c(1, 2, 3, 4, 5, 6, 7)
      } else if (calc == "effectSize") {
        order <- c(3, 5, 1, 2, 4, 6, 7)
      } else if (calc == "power") {
        order <- c(6, 1, 2, 3, 4, 5, 7)
      } else {
        order <- c(7, 1, 2, 3, 4, 5, 6)
      }

      colNames <- c("n1", "n2", "comparisonProportion", "baselineProportion","effectSize", "power", "alpha")
      colLabels <- c(
        "N\u2081",
        "N\u2082",
        gettext("p\u2081"),
        gettext("p\u2082"),
        gettext("Cohen's |<i>h</i>|"),
        gettext("Power"),
        "\u03B1"
      )
      colType <- c("integer", "integer","number", "number", "number", "number", "number")

      for (i in seq_along(order)) {
        table$addColumnInfo(colNames[order[i]],
                            title = colLabels[order[i]],
                            overtitle = if (((calc == "sampleSize" || calc == "effectSize") && i > 2) || ((calc != "sampleSize" && calc != "effectSize") && i > 1)) "User Defined" else NULL,
                            type = colType[order[i]]
        )
      }

      self$options$effectSize <- abs(2 * (asin(sqrt(self$options$comparisonProportion)) - asin(sqrt(self$options$baselineProportion))))

      row <- list()
      for (i in 2:length(order)) {
        row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]
      }

      if (self$options$calculation != "sampleSize") {
        row[["n1"]] <- self$options[["sampleSize"]]
        row[["n2"]] <- ceiling(self$options[["sampleSize"]] * self$options[["sampleSizeRatio"]])
      }

      table$addRows(rowNames = 1, row)

      private$.populatePowerTab(results, stats)
    },
    .initPowerESTab = function(results, stats) {
      table <- self$jaspResults[["powerEStab"]]

      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = gettext("Power by Effect Size"))
        table$dependOn(c(
          "test",
          "baselineProportion",
          "comparisonProportion",
          "effectSize",
          "effectDirection",
          "power",
          "sampleSize",
          "alternative",
          "alpha",
          "calculation",
          "sampleSizeRatio",
          "text"
        ))
        table$position <- 4
        self$jaspResults[["powerEStab"]] <- table
      } else {
        return()
      }

      table$addColumnInfo(
        name = "es",
        title = gettext("True effect size"),
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

      private$.populatePowerESTab(results, stats)
    },
    #### Populate table ----
    .populatePowerESTab = function(r, lst) {
      html <- self$jaspResults[["tabText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text"))
        html$position <- 3
        self$jaspResults[["tabText"]] <- html
      }

      # This table is created in init
      table <- self$jaspResults[["powerEStab"]]

      ## Get options from interface
      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- ifelse(n1 == n2,
                       gettextf("a sample size of %1$s in each group ", n1),
                       gettextf("group sample sizes of %1$s and %2$s respectively, ", n1, n2)
      )
      tail_text <- ifelse(alt == "two.sided",
                          "two-sided",
                          "one-sided"
      )

      if (calc == "sampleSize") {
        str <- gettextf(
          "We would need %1$s to reliably (with probability greater than or equal to %2$s) detect an effect size of <i>%3$s%4$s</i>%5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
          n_text, power, "|h|", "\u2265", round(d, 3) , tail_text, alpha
        )
      } else if (calc == "effectSize") {
        str <- gettextf(
          "A design with %1$s will reliably (with probability greater than or equal to %2$s) detect effect sizes of <i>%3$s%4$s</i>%5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
          n_text, power, "|h|", "\u2265", round(d, 3), tail_text, alpha
        )
      } else if (calc == "power") {
        str <- gettextf(
          "A design with %1$s can detect effect sizes of %2$s<i>%3$s</i>%4$s with a probability of at least %5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
          n_text, "|h|", "\u2265", round(d, 3), round(power, 3), tail_text, alpha
        )
      }

      hypo_text <- "|<i>h</i>|><i>0</i>"

      str <- paste0(
        str,
        gettextf(
          "<p>To evaluate the design specified in the table, we can consider how sensitive it is to true effects of increasing sizes; that is, are we likely to correctly conclude that %1$s when the effect size is large enough to care about?",
          hypo_text
        )
      )

      html[["text"]] <- str

      probs <- c(.5, .8, .95)
        probs_es <- try(sapply(probs, function(p) {
          abs(2 * (asin(sqrt(pwr.2p2n.test(
            n = n1, n.ratio = n_ratio, p0 = p0,
            sig.level = alpha, power = p,
            alternative = alt
          )$p1[1])) - asin(sqrt(p0))))
        }))
        if(inherits(probs_es, "try-error")) {
          table$setError(gettext("The specified design leads to (an) unsolvable equation(s) while computing the values for this power table. Try to enter less extreme values for the parameters."))
          return()
        }

      esText <- c(
        gettextf("0 < %1$s %2$s  %3$s", "|h|", "\u2264", format(round(probs_es[1], 3), nsmall = 3)),
        gettextf("%1$s < %2$s %3$s %4$s", format(round(probs_es[1], 3), nsmall = 3), "|h|", "\u2264", format(round(probs_es[2], 3), nsmall = 3)),
        gettextf("%1$s < %2$s %3$s %4$s",format(round(probs_es[2], 3), nsmall = 3), "|h|", "\u2264", format(round(probs_es[3], 3), nsmall = 3)),
        gettextf("%1$s %2$s %3$s", "|h|", "\u2265", format(round(probs_es[3], 3), nsmall = 3))
      )

      cols <- list("es" = esText)
      table$addColumns(cols)
    },

    .populatePowerTab = function(r, lst) {
      table <- self$jaspResults[["powertab"]]

      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      if (calc == "sampleSize") {
        table$addColumns(list(n1 = n1))
        table$addColumns(list(n2 = n2))
      } else if (calc == "effectSize") {
        table$addColumns(list(comparisonProportion = p1))
        table$addColumns(list(effectSize = d))
      } else {
        row <- list()
        row[[calc]] <- r[[switch(calc, "sampleSize" = "n", calc)]]
        table$addColumns(row)
      }
      if (calc == "sampleSize") {
        if(round(pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power, 3) == 1) {
          table$addFootnote(gettext("Due to the rounding of the sample size, the actual power can deviate from the target power. <b>Actual power: >0.999"))
        } else {
          table$addFootnote(gettextf("Due to the rounding of sample sizes, the actual power can deviate from the target power. <b>Actual power: %1$s</b>",
                                     round(pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power, 3)
          ))
        }
      }
    },

    #### Plot functions ----
    .preparePowerContour = function(r, lst) {
      image <- self$jaspResults[["powerContour"]]
      if (is.null(image)) {
        image <- createJaspPlot(
          title = gettext("Power Contour"),
          width=400,
          height=350
        )
        image$dependOn(c(
          "test",
          "baselineProportion",
          "comparisonProportion",
          "effectSize",
          "effectDirection",
          "power",
          "sampleSize",
          "alternative",
          "alpha",
          "calculation",
          "sampleSizeRatio",
          "powerContour"
        ))
        image$position <- 5
        self$jaspResults[["powerContour"]] <- image
      }

      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "sampleSize",
                             pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      ps <- ttestPlotSettings
      if (alt == "less" || (alt == "two.sided" && p1 < p0)) {
        ps$maxd <- abs(2 * (asin(sqrt(min(0.0001, p1))) - asin(sqrt(p0))))
      } else {
        ps$maxd <- abs(2 * (asin(sqrt(max(0.9999, p1))) - asin(sqrt(p0))))
      }

      maxn <- try(ceiling(pwr.2p2n.test(
        n.ratio = n_ratio,
        power = max(0.99, power),
        p0 = p0, p1 = p1,
        sig.level = alpha,
        alternative = alt
      )$n))
      if(inherits(maxn, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
        return()
      }

      if (n1 >= maxn && n1 >= ps$maxn) {
        maxn <- ceiling(n1 * ps$max.scale)
      } else if (maxn < ps$maxn) {
        if ((ps$maxn - n1) < 20) {
          maxn <- ps$maxn * ps$max.scale
        } else {
          maxn <- ps$maxn
        }
      }

      minn <- 2
      try <- try(pwr.2p2n.test(n = minn, n.ratio = n_ratio, p0 = p0, sig.level = alpha, power = power, alternative = alt))
      while (inherits(try, "try-error")) {
        minn <- minn + 1
        try <- try(pwr.2p2n.test(n = minn, n.ratio = n_ratio, p0 = p0, sig.level = alpha, power = power, alternative = alt))
      }

      nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))
      dd <- seq(ps$mind, ps$maxd, len = ps$lens)

      if (alt == "less" || (alt == "two.sided" && p1 < p0)) {
        pp <- sin(0.5*dd - asin(sqrt(p0)))^2
      } else {
        pp <- sin(0.5*dd + asin(sqrt(p0)))^2
      }

      z.pwr <- try(sapply(pp, function(p1) {
        pwr.2p2n.test(n = nn, n.ratio = n_ratio,
                     p0 = p0, p1 = p1,
                     sig.level = alpha,
                     alternative = alt
        )$power
      }))
      if(inherits(z.pwr, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
        return()
      }

      z.delta <- try(sapply(nn, function(N) {
        abs(2 * (asin(sqrt(pwr.2p2n.test(
          n = N, n.ratio = n_ratio, p0 = p0,
          sig.level = alpha, power = power,
          alternative = alt
        )$p1)) - asin(sqrt(p0))))
      }))
      if(inherits(z.delta, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
        return()
      }

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
      image$plotObject <- private$.powerContour(state = state, ggtheme = pwr_plot_theme())
    },
    .populateContourText = function(r, lst) {
      html <- self$jaspResults[["contourText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerContour"))
        html$position <- 6
        self$jaspResults[["contourText"]] <- html
      }

      str <- gettext(
        "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical effect size and the sample sizes in the design. As we increase the sample sizes, smaller effect sizes become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only larger effect sizes, smaller sample sizes are needed The point shows the power of the specified  design and effect size."
      )

      html[["text"]] <- str
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
          "baselineProportion",
          "comparisonProportion",
          "effectSize",
          "effectDirection",
          "power",
          "sampleSize",
          "alternative",
          "alpha",
          "calculation",
          "sampleSizeRatio",
          "powerByEffectSize"
        ))
        image$position <- 7
        self$jaspResults[["powerCurveES"]] <- image
      }

      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "sampleSize",
                             pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      ps <- ttestPlotSettings

      if (alt == "less" || (alt == "two.sided" && p1 < p0)) {
        ps$maxd <- abs(2 * (asin(sqrt(min(0.00001, p1))) - asin(sqrt(p0))))
      } else {
        ps$maxd <- abs(2 * (asin(sqrt(max(0.99999, p1))) - asin(sqrt(p0))))
      }

      dd <- seq(ps$mind, ps$maxd, len = ps$curve.n)
      if (alt == "less" || (alt == "two.sided" && p1 < p0)) {
        pp <- sin(0.5*dd - asin(sqrt(p0)))^2
      } else {
        pp <- sin(0.5*dd + asin(sqrt(p0)))^2
      }

      y <- try(pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = pp, sig.level = alpha, alternative = alt)$power)
      if(inherits(y, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the power curve. Try to enter less extreme values for the parameters"))
        return()
      }
      if (power < 0.999) {
        y <- y[y<0.999]
        dd <- dd[1:length(y)]
        dd <- seq(min(dd), max(dd), len = ps$curve.n)
        if (alt == "less" || (alt == "two.sided" && p1 < p0)) {
          pp <- sin(0.5*dd - asin(sqrt(p0)))^2
        } else {
          pp <- sin(0.5*dd + asin(sqrt(p0)))^2
        }
        y <- pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = pp, sig.level = alpha, alternative = alt)$power
      }
      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      state = list(cols = cols, dd = dd, y = y, yrect = yrect, n1 = n1, n2 = n2, alpha = alpha, delta = d, pow = power)
      image$plotObject <- private$.powerCurveES(state = state, ggtheme = pwr_plot_theme())
    },
    .populatePowerCurveESText = function(r, lst) {
      html <- self$jaspResults[["curveESText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerByEffectSize"))
        html$position <- 8
        self$jaspResults[["curveESText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      p1 <- ifelse(calc == "effectSize",
                   r$p1,
                   ifelse(calc == "sampleSize",
                          pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, sig.level = alpha, power = power, alternative = alt)$p1,
                          lst$p1))
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      d <- round(d, 3)

      n_text <- ifelse(n1 == n2,
                       gettextf("sample sizes of %1$s in each group", n1),
                       gettextf("group sample sizes of %1$s and %2$s, respectively", n1, n2)
      )

      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        alt_text <- gettext("<i>|h|\u003E</i>")
        crit_text <- gettext("criteria")
      } else {
        tail_text <- gettext("one-sided")
        alt_text <- gettext("<i>|h|\u003E</i>")
        crit_text <- gettext("criterion")
      }

      if (calc == "power") {
        pwr_string <- gettextf("have power of at least %1$s", round(power, 3))
      } else {
        pwr_string <- gettextf("only be sufficiently sensitive (power >%1$s)", round(power, 3))
      }

      p50 <- try(pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, sig.level = alpha, power = .5, alternative = alt)$p1)
      if (inherits(p50, "try-error"))
        return()
      d50 <- abs(2 * (asin(sqrt(p50)) - asin(sqrt(p0))))

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. If we obtained %1$s our test and design would %2$s to effect sizes of %3$s%4$s. <p>We would be more than likely to miss (power less than 50%%) effect sizes less than <i>|h|=</i>%5$s.",
        n_text, pwr_string, alt_text, d, round(d50, 3)
      )

      html[["text"]] <- str
    },
    .preparePowerCurveN = function(r, lst) {
      image <- self$jaspResults[["powerCurveN"]]
      if (is.null(image)) {
        image <- createJaspPlot(title="Power Curve by N", width=400, height=350)
        image$dependOn(c(
          "test",
          "baselineProportion",
          "comparisonProportion",
          "effectSize",
          "effectDirection",
          "power",
          "sampleSize",
          "alternative",
          "alpha",
          "calculation",
          "sampleSizeRatio",
          "powerBySampleSize"
        ))
        image$position <- 9
        self$jaspResults[["powerCurveN"]] <- image
      }

      calc <- self$options$calculation

      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      n_ratio <- lst$n_ratio
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "sampleSize",
                             pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      ps <- ttestPlotSettings

      if (alt == "less" || (alt == "two.sided" && p1 < p0)) {
        ps$maxd <- abs(2 * (asin(sqrt(min(0.01, p1))) - asin(sqrt(p0))))
      } else {
        ps$maxd <- abs(2 * (asin(sqrt(max(0.99, p1))) - asin(sqrt(p0))))
      }

      maxn <- try(ceiling(pwr.2p2n.test(
        n.ratio = n_ratio,
        power = max(0.9999, power),
        p0 = p0,
        p1 = p1,
        sig.level = alpha,
        alternative = alt
      )$n))

      if(inherits(maxn, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the 'Power Curve by N' plot. Try to enter less extreme values for the parameters"))
        return()
      } else if (n1 >= maxn && n1 >= ps$maxn) {
        maxn <- ceiling(n1 * ps$max.scale)
      }

      minn <- 2
      try <- try(pwr.2p2n.test(n = minn, n.ratio = n_ratio, p0 = p0, sig.level = alpha, power = power, alternative = alt))
      while (inherits(try, "try-error")) {
        minn <- minn + 1
        try <- try(pwr.2p2n.test(n = minn, n.ratio = n_ratio, p0 = p0, sig.level = alpha, power = power, alternative = alt))
      }

      nn <- seq(minn, maxn)

      y <- try(pwr.2p2n.test(
        n = nn,
        n.ratio = n_ratio,
        p0 = p0, p1 = p1, sig.level = alpha, alternative = alt
      )$power)
      if(inherits(y, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the 'Power Curve by N' plot. Try to enter less extreme values for the parameters"))
        return()
      }

      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      lims <- data.frame(
        xlim = c(minn, maxn),
        ylim = c(0, 1)
      )

      state = list(n = n1, cols = cols, nn = nn, y = y, yrect = yrect, lims = lims, delta = d, alpha = alpha, n_ratio = n_ratio, pow = power)
      image$plotObject <- private$.powerCurveN(state = state, ggtheme = pwr_plot_theme())
    },
    .preparePowerDist = function(r, lst) {
      image <- self$jaspResults[["powerDist"]]
      if (is.null(image)) {
        image <- createJaspPlot(title="Power Demonstration", width=400, height=300)
        image$dependOn(c(
          "test",
          "baselineProportion",
          "comparisonProportion",
          "effectSize",
          "effectDirection",
          "power",
          "sampleSize",
          "alternative",
          "alpha",
          "calculation",
          "sampleSizeRatio",
          "powerDemonstration"
        ))
        image$position <- 11
        self$jaspResults[["powerDist"]] <- image
      }

      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "sampleSize",
                             pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power,
                             lst$pow))


      effN <- n1 * n2 / (n1 + n2)
      ncp <- sqrt(effN) * d

      if (alt == "two.sided") {
        crit <- qnorm(p = 1 - alpha / 2) /sqrt(effN)
      } else {
        crit <- qnorm(p = 1 - alpha) /sqrt(effN)
      }

      if (lst$es > 0) {
        xlims <- c(qnorm(.001), qnorm(.999, mean = ncp)) / sqrt(effN)
      } else {
        xlims <- c(qnorm(.001, mean = ncp), qnorm(.999)) / sqrt(effN)
      }

      y.max <- dnorm(0) / sqrt(effN)

      xx <- seq(xlims[1], xlims[2], len = 100)
      yy.null <- dnorm(xx * sqrt(effN)) / sqrt(effN)
      yy.alt <- dnorm(xx * sqrt(effN), mean = ncp) / sqrt(effN)

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
    },
    .populatePowerCurveNText = function(r, lst) {
      html <- self$jaspResults[["curveNText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerBySampleSize"))
        html$position <- 10
        self$jaspResults[["curveNText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- ifelse(n1 == n2,
                       gettextf("sample sizes of at least %1$s in each group", n1),
                       gettextf("group sample sizes of at least %1$s and %2$s, respectively", n1, n2)
      )

      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- gettext("<i>h\u2264</i>0,")
        alt_text <- gettext("<i>|h|\u003E</i>0,")
        crit_text <- gettext("criteria")
      } else {
        tail_text <- gettext("one-sided")
        null_text <- gettext("<i>h=</i>0,")
        alt_text <- gettext("<i>|h|\u003E</i>0,")
        crit_text <- gettext("criterion")
      }

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. In order for our test and design to have sufficient sensitivity (power > %1$s) to detect that %2$s when the effect size is %3$s or larger, we would need %4$s.",
        round(power, 3), alt_text, d, n_text
      )

      html[["text"]] <- str
    },
    .populateDistText = function(r, lst) {
      html <- self$jaspResults[["distText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerDemonstration"))
        html$position <- 12
        self$jaspResults[["distText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calculation
      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      d <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      d <- round(d, 2)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "sampleSize",
                             pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      n_text <- ifelse(n1 == n2,
                       gettextf("a sample size of %1$s in each group", n1),
                       gettextf("group sample sizes of %1$s and %2$s, respectively", n1, n2)
      )

      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- gettext("<i>h=</i>0,")
        alt_text <- gettext("<i>|h|\u2265</i>")
        crit_text <- gettext("criteria")
      } else {
        tail_text <- gettext("one-sided")
        null_text <- gettext("<i>h\u2264</i>0,")
        alt_text <- gettext("<i>|h|\u2265</i>")
        crit_text <- gettext("criterion")
      }

      str <- paste(
        "<p>",
        gettextf("The figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> effect size when <i>%1$s=</i>0 (left), and when <i>%2$s=</i>%3$s (right).", "h", "|h|", d),
        gettextf("Both assume %1$s.", n_text),
        "</p><p>",
        gettextf("The vertical dashed lines show the %1$s we would set for a %2$s test with <i>\u03B1=</i>%3$s.", crit_text, tail_text, alpha),
        gettextf("When the observed effect size is far enough away from 0 to be more extreme than the %1$s we say we 'reject' the null hypothesis.", crit_text),
        gettextf("If the null hypothesis were true and %1$s the evidence would lead us to wrongly reject the null hypothesis at most %2$s%% of the time.", null_text, 100 * alpha),
        "</p><p>",
        gettextf("On the other hand, if <i>%1$s%2$s</i>%3$s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%4$s%5$s</i>0 &mdash; at least %6$s%% of the time.", "|h|", "\u2265", d, "|h|", ">", 100 * round(power, 3)),
        gettextf("The design's power for detecting effects of %1$s%2$s is thus %3$s.", alt_text, d, round(power, 3)),
        "</p>"
      )


      html[["text"]] <- str
    },

    #### Generate synthetic dataset ----
    .generateDataset = function(r, lst) {
      datasetContainer <- self$jaspResults[["datasetcont"]]
      if (is.null(datasetContainer)) {
        # Create Container if it doesn't exist yet
        datasetContainer <- createJaspContainer(title = gettext("Synthetic Dataset"))
        datasetContainer$dependOn(c(
          "test",
          "baselineProportion",
          "comparisonProportion",
          "effectSize",
          "effectDirection",
          "power",
          "sampleSize",
          "alternative",
          "alpha",
          "calculation",
          "sampleSizeRatio",
          "savePath",
          "firstGroupMean",
          "secondGroupMean",
          "firstGroupSd",
          "secondGroupSd",
          "populationSd",
          "effectDirectionSyntheticDataset",
          "testValue",
          "setSeed",
          "seed"
        ))
        datasetContainer$position <- 12
        self$jaspResults[["datasetcont"]] <- datasetContainer

        generatedDataset     <- createJaspState()
        characteristicsTable <- createJaspTable(title = gettext("Characteristics"))
        powerTable           <- createJaspTable(gettext("Post Hoc Power Analysis"))

      } else {
        return()
      }

      #Generate dataset
      if(!grepl(".csv", self$options[["savePath"]], fixed = TRUE) && !grepl(".txt", self$options[["savePath"]], fixed = TRUE))
        .quitAnalysis(gettext("The generated dataset must be saved as a .csv or .txt file."))

      calc <- self$options$calculation

      n_ratio <- lst$n_ratio
      n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
      n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
      p0 <- lst$p0
      if(calc == "effectSize") p1 <- ifelse(lst$alt == "two.sided" && self$options$effectDirection == "less", r$p1[2], r$p1[1]) else p1 <- lst$p1
      if(p1 < p0) {
        p1 <- floor(p1 * n1) / n1
        p0 <- ceiling(p0 * n2) / n2
      } else {
        p1 <- ceiling(p1 * n1) / n1
        p0 <- floor(p0 * n2) / n2
      }
      d  <- abs(2 * (asin(sqrt(p1)) - asin(sqrt(p0))))
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- pwr.2p2n.test(n = n1, n.ratio = n_ratio, p0 = p0, p1 = p1, sig.level = alpha, alternative = alt)$power

      if(self$options[["setSeed"]])
        set.seed(self$options[["seed"]])

      sample_indices_1 <- sample(x = seq(n1), size = p1 * n1, replace = FALSE)
      sample_indices_2 <- sample(x = seq(n2), size = p0 * n2, replace = FALSE)


      id        <- seq(n1 + n2)
      group_1 <- rep(0, n1)
      group_1[sample_indices_1] <- 1
      group_2 <- rep(0, n2)
      group_2[sample_indices_2] <- 1
      dependent <- c(group_1, group_2)
      group <- c(rep(1, n1), rep(2, n2))

      dataset <- data.frame(cbind(id, dependent, group))

      csv <- try(write.csv(dataset, self$options[["savePath"]], row.names = FALSE))
      if(inherits(csv, "try-error"))
        .quitAnalysis(gettext("The generated dataset could not be saved. Please make sure that the specified path exists and the specified csv file is closed."))

      generatedDataset <- dataset

      datasetContainer[["generatedData"]] <- generatedDataset

      #Characteristics tab
      colNames <- c("n1", "n2", "p1", "p2")
      colLabels <- c(
        "N\u2081",
        "N\u2082",
        gettext("p\u2081"),
        gettext("p\u2082")
      )
      colType <- c("integer", "integer", "number", "number")

      for (i in seq_along(colNames)) {
        characteristicsTable$addColumnInfo(colNames[i],
                                           title = colLabels[i],
                                           type = colType[i]
        )
      }

      characteristicsTable[["n1"]]    <- n1
      characteristicsTable[["n2"]]    <- n2
      characteristicsTable[["p1"]]    <- p1
      characteristicsTable[["p2"]]    <- p0
      characteristicsTable$addFootnote(gettextf("The synthetic dataset is saved as %s", self$options[["savePath"]]))

      datasetContainer[["characteristics"]] <- characteristicsTable

      #Post hoc power tab
      colNames <- c("es", "alt", "power", "alpha")
      colLabels <- c(
        gettext("Cohen's |<i>h</i>|"),
        "Alternative hypothesis",
        gettext("Power"),
        "\u03B1"
      )
      colType <- c("number", "string", "number", "number")

      for (i in seq_along(colNames)) {
        powerTable$addColumnInfo(colNames[i],
                                 title = colLabels[i],
                                 type = colType[i]
        )
      }

      powerTable[["es"]]    <- d
      powerTable[["alt"]]   <- switch(alt,
                                      "two.sided" = "Two-sided",
                                      "less" = "Less (One-sided)",
                                      "greater" = "Greater (One-sided)"
      )
      powerTable[["power"]] <- power
      powerTable[["alpha"]] <- alpha

      datasetContainer[["posthocpower"]] <- powerTable
    }
  )
)
