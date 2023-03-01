# Originally based on https://github.com/richarddmorey/jpower

test1PoisClass <- R6::R6Class(
  "test1PoisClass",
  inherit = tTestBaseClass,
  private = list(
    #### Member variables ----
    probs_es = NULL,

    #### Compute results ----
    .compute = function(stats) {
      ## Compute numbers for table
      pow.n <- ceiling(pwr.pois.test(t = stats$t1, lambda0 = stats$p0, lambda1 = stats$p1, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)$n)
      pow.lambda1 <- pwr.pois.test(t = stats$t1, n = stats$n, lambda0 = stats$p0, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$lambda1
      pow.pow <- pwr.pois.test(t = stats$t1, n = stats$n, lambda0 = stats$p0, lambda1 = stats$p1, sig.level = stats$alpha, alternative = stats$alt)$power

      # Calculate probs_es here to have access to stats list
      probs <- c(.5, .8, .95)
      probs_es <- sapply(probs, function(pr) {
        pwr.pois.test(t = stats$t1,
          n = stats$n, lambda0 = stats$p0, sig.level = stats$alpha, power = pr,
          alternative = stats$alt
        )$lambda1
      })
      private$probs_es <- probs_es

      return(list(n = pow.n, lambda1 = pow.lambda1, power = pow.pow))
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
        order <- c(1, 2, 3, 4, 5, 6, 7)
      } else if (calc == "es") {
        order <- c(4, 5, 1, 2, 3, 6, 7)
      } else if (calc == "power") {
        order <- c(6, 1, 2, 3, 4, 5, 7)
      } else {
        order <- c(7, 1, 2, 3, 4, 5, 6)
      }

      colNames <- c("n", "t", "p0", "p1","es", "power", "alpha")
      colLabels <- c(
        "N",
        "t",
        gettext("Hypothesized rate (\u03BB\u2080)"),
        gettext("Comparison rate (\u03BB\u2081)"),
        gettext("Effect size ((\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081)"),
        gettext("Power"),
        "\u03B1"
      )
      colType <- c("integer", "number", "number", "number", "number", "number", "number")

      for (i in seq_along(order)) {
        table$addColumnInfo(colNames[order[i]],
                            title = colLabels[order[i]],
                            overtitle = ifelse((calc == "es" && i > 2) || (calc != "es" && i > 1), gettext("User Defined"), NULL),
                            type = colType[order[i]]
        )
      }

      self$options$es <- (self$options$p1 - self$options$p0) / sqrt(self$options$p1)


      row <- list()
      if (calc != "es") {
        for (i in 2:6) {
          row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]
        }
      } else {
        for (i in 3:6) {
          row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]
        }
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
        title = gettext("True effect size ((\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081)"),
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
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- gettextf("a sample size of ", n)

      tail_text <- ifelse(alt == "two.sided",
                          gettext("two-sided"),
                          gettext("one-sided")
      )
      d_text <- gettext("effect sizes of <i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081</i>")

      if (calc == "n") {
        str <- gettextf(
          "We would need %s to reliably (with probability greater than %s) detect %s<i>%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, power, d_text, "\u2265", d, tail_text, alpha
        )
      } else if (calc == "es") {
        str <- gettextf(
          "A design with %s will reliably (with probability greater than %s) detect  %s<i>%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, power, d_text, "\u2265", round(d, 3), tail_text, alpha
        )
      } else if (calc == "power") {
        str <- gettextf(
          "A design with %s can detect %s<i>%s</i>%s with a probability of at least %s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, d_text, "\u2265", d, round(power, 3), tail_text, alpha
        )
      }

      hypo_text <- ifelse(alt == "two.sided",
                          "<i>|(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081|>0</i>",
                          ifelse(alt == "less",
                                 "<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081<0</i>",
                                 "<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081>0</i>"
                                 )
                          )

      str <- paste0(
        str,
        gettextf(
          "<p>To evaluate the design specified in the table, we can consider how sensitive it is to true effects of increasing sizes; that is, are we likely to correctly conclude that %s when the effect size is large enough to care about?",
          hypo_text
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
        "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical effect size and the sample sizes in the design. As we increase the sample sizes, smaller effect sizes become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only larger effect sizes, smaller sample sizes are needed. The solid black curve on the contour plot shows sample size/effect size combinations with a power of %s. The point shows the specified  design and %s.",
        round(power, 3), dType_text
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
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- gettextf("sample sizes of %s", n)

      dType_text <- ifelse(self$options$esType == "h",
                           gettext("effect sizes"),
                           gettext("absolute differences in proportions"))
      dType_symbol <- ifelse(self$options$esType == "h",
                             gettext("h"),
                             gettext("\u0394p"))



      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081\u2264</i>0,")
        alt_text <- gettext("<i>|(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081|\u003E</i>")
        crit_text <- "criteria"
      } else {
        tail_text <- gettext("one-sided")
        null_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081=</i>0,")
        alt_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081\u003E</i>")
        crit_text <- "criterion"
      }

      if (calc == "power") {
        pwr_string <- gettextf("have power of at least %s", round(power, 3))
      } else {
        pwr_string <- gettextf("only be sufficiently sensitive (power >%s)", round(power, 3))
      }

      l50 <- pwr.pois.test(t = t, n = n, lambda0 = p0, sig.level = alpha, power = .5, alternative = alt)$lambda1
      d50 <- (l50 - lambda0) / sqrt(l50)

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. If we obtained %s our test and design would %s to effect sizes of %s%s. <p>We would be more than likely to miss (power less than 50%%) effect sizes less than <i>%s=</i>%s.",
        n_text, pwr_string, alt_text, d, "(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081", round(d50, 3)
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
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alt <- lst$alt

      n_text <- gettextf("sample sizes of at least %s", n)



      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081\u2264</i>0,")
        alt_text <- gettext("<i>|(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081|\u003E</i>0,")
        crit_text <- gettext("criteria")
      } else {
        tail_text <- gettext("one-sided")
        null_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081=</i>0,")
        alt_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081\u2260</i>0,")
        crit_text <- gettext("criterion")
      }

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. In order for our test and design to have sufficient sensitivity (power > %s) to detect that %s when the effect size is %s or larger, we would need %s.",
        round(power, 3), alt_text, d, n_text
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
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
      d <- round(d, 2)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- gettextf("a sample size of %s", n)

      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081=</i>0,")
        alt_text <- gettext("<i>|(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081|\u2265</i>")
        crit_text <- gettext("criteria")
      } else {
        tail_text <- gettext("one-sided")
        null_text <- gettext("<i>(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081\u2264</i>0,")
        alt_text <- gettext("<i>|(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081|\u2265</i>")
        crit_text <- gettext("criterion")
      }

      str <- paste(
        "<p>",
        gettextf("The figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> effect size when <i>%s=</i>0 (left), and when <i>%s=</i>%s (right).", "(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081", "(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081", d),
        gettextf("Both assume %s.", n_text),
        "</p><p>",
        gettextf("The vertical dashed lines show the %s we would set for a %s test with <i>α=</i>%s.", crit_text, tail_text, alpha),
        gettextf("When the observed effect size is far enough away from 0 to be more extreme than the %s we say we 'reject' the null hypothesis.", crit_text),
        gettextf("If the null hypothesis were true and %s the evidence would lead us to wrongly reject the null hypothesis at most %s%% of the time.", null_text, 100 * alpha),
        "</p><p>",
        gettextf("On the other hand, if <i>%s%s</i>%s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%s%s</i>0 &mdash; at least %s%% of the time.", "(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081", "\u2265", d, "(\u03BB\u2081-\u03BB\u2080)/\u221A\u03BB\u2081", "\u2265", 100 * round(power, 3)),
        gettextf("The design's power for detecting effects of %s%s is thus %s.", alt_text, d, round(power, 3)),
        "</p>"
      )


      html[["text"]] <- str
    },

    #### Populate table ----
    .populatePowerTab = function(results) {
      table <- self$jaspResults[["powertab"]]
      if (calc == "es") {
        table$addColumns(list(p1 = results[["lambda1"]]))
        table$addColumns(list(es = (results[["lambda1"]] - self$options$p0) / sqrt(results[["lambda1"]])))
      } else {
        row <- list()
        row[[calc]] <- results[[calc]]
        table$addColumns(row)
      }
    },
    .populatePowerESTab = function() {
      table <- self$jaspResults[["powerEStab"]]

      p0 <- self$options$p0
      if(self$options$esType == "h") {
        probs_es <- sapply(private$probs_es, function(prob) {2 * (asin(sqrt(prob)) - asin(sqrt(p0)))})
      } else {
        probs_es <- sapply(private$probs_es, function (prob) {abs(prob - p0)})
      }

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
          "es",
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
      if(self$options$esType != "h")
        ps$maxd <- 1 - p0

      calc <- self$options$calc

      n <- ifelse(calc == "n", r$n, lst$n)
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      if (n >= ps$maxn) {
        maxn <- ceiling(n * ps$max.scale)
      } else {
        maxn <- ps$maxn
      }

      minn <- 2
      try <- try(pwr.pois.test(t = t, n = minn, lambda0 = p0, sig.level = alpha, power = power, alternative = alt))
      while (inherits(try, "try-error")) {
        minn <- minn + 1
        try <- try(pwr.pois.test(t = t, n = minn, lambda0 = p0, sig.level = alpha, power = power, alternative = alt))
      }

      nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))
      dd <- seq(ps$mind, ps$maxd, len = ps$lens)
      if(self$options$esType == "h"){
        pp <- sin(0.5*dd + asin(sqrt(p0)))^2
      } else {
        if (alt != "less") {
          pp <- p0 + dd
        } else {
          pp <- p0 - dd
        }
      }

      z.pwr <- sapply(pp, function(p1) {
        pwr.pois.test(t = t, n = nn, lambda0 = p0, lambda1 = p1, sig.level = alpha, alternative = alt)$power
      })

      z.delta <- sapply(nn, function(N) {
        if(self$options$esType == "h") {
          2 * (asin(sqrt(pwr.pois.test(t = t, n = N, lambda0 = p0, sig.level = alpha, power = power, alternative = alt)$lambda1)) - asin(sqrt(p0)))
        } else {
          abs(pwr.pois.test(t = t, n = N, lambda0 = p0, sig.level = alpha, power = power, alternative = alt)$lambda1 - p0)
        }
      })

      state = list(
        z.pwr = z.pwr,
        z.delta = z.delta,
        ps = ps,
        nn = nn,
        dd = dd,
        plambda1 = pp,
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
          "es",
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
      if(self$options$esType != "h")
        ps$maxd <- 1 - p0

      calc <- self$options$calc

      n <- ifelse(calc == "n", r$n, lst$n)
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      dd <- seq(ps$mind, ps$maxd, len = ps$curve.n)
      if(self$options$esType == "h"){
        pp <- sin(0.5*dd + asin(sqrt(p0)))^2
      } else {
        if (alt != "less") {
          pp <- p0 + dd
        } else {
          pp <- p0 - dd
        }
      }

      y <- pwr.pois.test(t = t, n = n, lambda0 = p0, lambda1 = pp, sig.level = alpha, alternative = alt)$power
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
          "es",
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
      if(self$options$esType != "h")
        ps$maxd <- 1 - p0

      n <- ifelse(calc == "n", r$n, lst$n)
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
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

      y <- pwr.pois.test(t = t, n = nn, lambda0 = p0, lambda1 = p1, sig.level = alpha, alternative = alt)$power

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
          "p0",
          "p1",
          "es",
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
      t <- lst$t1
      lambda0 <- lst$p0
      lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
      d <- (lambda1 - lambda0) / sqrt(lambda1)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      effN <- n
      ncp <- sqrt(effN) * d

      if (alt == "two.sided") {
        crit <- qnorm(lambda1 = 1 - alpha / 2)
      } else {
        crit <- qnorm(lambda1 = 1 - alpha)
      }

      if (d > 0) {
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
        groulambda1 = rep(c("Null", "Alt"), each = length(xx))
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
