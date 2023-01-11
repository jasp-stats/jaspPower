# Originally based on https://github.com/richarddmorey/jpower

ttestPSClass <- R6::R6Class(
    "ttestPSClass",
    inherit = baseClass,
    private = list(
        #### Member variables ----
        type = "paired",
        probs_es = NULL,

        #### Compute results ----
        .compute = function(stats) {
            ## Compute numbers for table
          pow.n <- NULL
          pow.es <- NULL
          pow.pow <- NULL
          if(self$options$calculation == "sampleSize")
            pow.n <- ceiling(pwr::pwr.t.test(d = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt, type = private$type)$n)
          if(self$options$calculation == "effectSize")
            pow.es <- pwr::pwr.t.test(n = stats$n, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt, type = private$type)$d
          if(self$options$calculation == "power")
            pow.pow <- pwr::pwr.t.test(n = stats$n, d = stats$es, sig.level = stats$alpha, alternative = stats$alt, type = private$type)$power

            return(list(n = pow.n, es = pow.es, power = pow.pow))
        },

        #### Init table ----
        .initPowerTab = function(results, stats) {
            table <- self$jaspResults[["powertab"]]
            if (is.null(table)) {
                # Create table if it doesn't exist yet
                table <- createJaspTable(title = gettext("A Priori Power Analysis"))
                table$dependOn(c(
                    "test",
                    "effectSize",
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
                  order <- c(1, 2, 3, 4)
              } else if (calc == "effectSize") {
                  order <- c(2, 1, 3, 4)
              } else if (calc == "power") {
                  order <- c(3, 1, 2, 4)
              } else {
                  order <- c(4, 1, 2, 3)
              }

            colNames <- c("sampleSize", "effectSize", "power", "alpha")
            colLabels <- c(
                "N",
                gettext("Cohen's |\u03B4|"),
                gettext("Power"),
                "\u03B1"
            )
            colType <- c("integer", "number", "number", "number")

            for (i in seq_along(order)) {
                table$addColumnInfo(colNames[order[i]],
                    title = colLabels[order[i]],
                    overtitle = if (i > 1) gettext("User Defined") else NULL,
                    type = colType[order[i]]
                )
            }

            row <- list()
            for (i in 2:4) {
                  row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]
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
                    "effectSize",
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

        #### Populate texts ----
        .populateContourText = function(r, lst) {
            html <- self$jaspResults[["contourText"]]
            if (is.null(html)) {
                html <- createJaspHtml()
                html$dependOn(c("test", "text", "powerContour"))
                html$position <- 6
                self$jaspResults[["contourText"]] <- html
            }

            calc <- self$options$calculation

            ## Get options from interface
            power <- ifelse(calc == "power", r$power, lst$pow)

            str <- gettext(
                "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical effect size and the sample sizes in the design. As we increase the sample sizes, smaller effect sizes become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only larger effect sizes, smaller sample sizes are needed. The point shows the power of the specified design and effect size."
            )

            html[["text"]] <- str
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
            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt
            d <- ifelse(calc == "effectSize",
                        r$es,
                        ifelse(calc == "sampleSize",
                               pwr::pwr.t.test(n = n, power = power, sig.level = alpha, alternative = alt, type = private$type)$d,
                               lst$es))
            d <- round(d, 3)

            n_text <- gettextf("sample sizes of %1$s", n)

            if (alt == "two.sided") {
                tail_text <- gettext("two-sided")
                alt_text <- "<i>|\u03B4|\u003E</i>"
                crit_text <- "criteria"
            } else {
                tail_text <- gettext("one-sided")
                alt_text <- "<i>|\u03B4|\u003E</i>"
                crit_text <- "criterion"
            }

            if (calc == "power") {
                pwr_string <- gettextf("have power of at least %1$s", round(power, 3))
            } else {
                pwr_string <- gettextf("only be sufficiently sensitive (power >%1$s)", round(power, 3))
            }

            d50 <- try(pwr::pwr.t.test(n = n, sig.level = alpha, power = .5, alternative = alt, type = private$type)$d)
            if (inherits(d50, "try-error"))
              return()

            str <- gettextf(
                "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. If we obtained %1$s our test and design would %2$s to effect sizes of %3$s%4$s. <p>We would be more than likely to miss (power less than 50%%) effect sizes less than <i>%5$s=</i>%6$s.",
                n_text, pwr_string, alt_text, d, "|\u03B4|", round(d50, 3)
            )

            html[["text"]] <- str
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
            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            d <- ifelse(calc == "effectSize", r$es, lst$es)
            d <- round(d, 3)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alt <- lst$alt

            n_text <- gettextf("sample sizes of at least %1$s", n)

            if (alt == "two.sided") {
                tail_text <- "two-sided"
                null_text <- "<i>|\u03B4|\u2264</i>0,"
                alt_text <- "<i>|\u03B4|\u003E</i>0,"
                crit_text <- "criteria"
            } else {
                tail_text <- "one-sided"
                null_text <- "<i>|\u03B4|=</i>0,"
                alt_text <- "<i>|\u03B4|\u2260</i>0,"
                crit_text <- "criterion"
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
            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            d <- ifelse(calc == "effectSize", r$es, lst$es)
            d <- round(d, 2)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt
            power <- ifelse(calc == "power",
                            r$power,
                            ifelse(calc == "sampleSize",
                                   pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power,
                                   lst$pow))

            n_text <- gettextf("a sample size of %1$s", n)

            if (alt == "two.sided") {
                tail_text <- gettext("two-sided")
                null_text <- "<i>|\u03B4|=</i>0,"
                alt_text <- "<i>|\u03B4|\u2265</i>"
                crit_text <- gettext("criteria")
            } else {
                tail_text <- gettext("one-sided")
                null_text <- "<i>|\u03B4|\u2264</i>0,"
                alt_text <- "<i>|\u03B4|\u2265</i>"
                crit_text <- gettext("criterion")
            }

            str <- paste(
                "<p>",
                gettextf("The figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> effect size when <i>%1$s=</i>0 (left), and when <i>%2$s=</i>%3$s (right).", "|\u03B4|", "|\u03B4|", d),
                gettextf("Both assume %1$s.", n_text),
                "</p><p>",
                gettextf("The vertical dashed lines show the %1$s we would set for a %2$s test with <i>\u03B1=</i>%3$s.", crit_text, tail_text, alpha),
                gettextf("When the observed effect size is far enough away from 0 to be more extreme than the %1$s we say we 'reject' the null hypothesis.", crit_text),
                gettextf("If the null hypothesis were true and %1$s the evidence would lead us to wrongly reject the null hypothesis at most %2$s%% of the time.", null_text, 100 * alpha),
                "</p><p>",
                gettextf("On the other hand, if <i>%1$s%2$s</i>%3$s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%4$s%5$s</i>0 &mdash; at least %6$s%% of the time.", "|\u03B4|", "\u2265", d, "|\u03B4|", ">", 100 * round(power, 3)),
                gettextf("The design's power for detecting effects of %1$s%2$s is thus %3$s.", alt_text, d, round(power, 3)),
                "</p>"
            )


            html[["text"]] <- str
        },

        #### Populate table ----
        .populatePowerTab = function(r, lst) {
            table <- self$jaspResults[["powertab"]]

            calc <- self$options$calculation
            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            d <- ifelse(calc == "effectSize", r$es, lst$es)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt

            row <- list()
            row[[calc]] <- r[[switch(calc,"effectSize" = "es", "sampleSize" = "n", calc)]]

            table$addColumns(row)
            if (calc == "sampleSize") {
              if (round(pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power, 3) == 1) {
                table$addFootnote(gettext("Due to the rounding of the sample size, the actual power can deviate from the target power. <b>Actual power: >0.999"))
              } else {
                table$addFootnote(gettextf("Due to the rounding of the sample size, the actual power can deviate from the target power. <b>Actual power: %1$s</b>",
                                           round(pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power, 3)
                ))
              }
            }
        },
        .populatePowerESTab = function(r, lst) {

          html <- self$jaspResults[["tabText"]]
          if (is.null(html)) {
            html <- createJaspHtml()
            html$dependOn(c("test", "text"))
            html$position <- 3
            self$jaspResults[["tabText"]] <- html
          }

          ## Get options from interface
          calc <- self$options$calculation
          n <- ifelse(calc == "sampleSize", r$n, lst$n)
          d <- ifelse(calc == "effectSize", r$es, lst$es)
          power <- ifelse(calc == "power", r$power, lst$pow)
          alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
          alt <- lst$alt

          n_text <- gettextf("a sample size of %1$s", n)

          tail_text <- ifelse(alt == "two.sided",
                              gettext("two-sided"),
                              gettext("one-sided")
          )

          if (calc == "sampleSize") {
            str <- gettextf(
              "We would need %1$s to reliably (with probability greater than or equal to %2$s) detect an effect size of <i>%3$s%4$s</i>%5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
              n_text, power, "|\u03B4|", "\u2265", d, tail_text, alpha
            )
          } else if (calc == "effectSize") {
            str <- gettextf(
              "A design with %1$s will reliably (with probability greater than or equal to %2$s) detect effect sizes of <i>%3$s%4$s</i>%5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
              n_text, power, "|\u03B4|", "\u2265", round(d, 3), tail_text, alpha
            )
          } else if (calc == "power") {
            str <- gettextf(
              "A design with %1$s can detect effect sizes of <i>%2$s%3$s</i>%4$s with a probability of at least %5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
              n_text, "|\u03B4|", "\u2265", d, round(power, 3), tail_text, alpha
            )
          }

          hypo_text <- "<i>|\u03B4|>0</i>"


          str <- paste0(
            str,
            gettextf(
              "<p>To evaluate the design specified in the table, we can consider how sensitive it is to true effects of increasing sizes; that is, are we likely to correctly conclude that %1$s when the effect size is large enough to care about?",
              hypo_text
            )
          )

          html[["text"]] <- str

            table <- self$jaspResults[["powerEStab"]]

            probs <- c(.5, .8, .95)
            probs_es <- try(sapply(probs, function(p) {
              pwr::pwr.t.test(
                n = n, sig.level = alpha, power = p,
                alternative = alt, type = private$type
              )$d
            }))
            if(inherits(probs_es, "try-error")) {
              table$setError(gettext("The specified design leads to (an) unsolvable equation(s) while computing the values for this power table. Try to enter less extreme values for the parameters."))
              return()
            }

            esText <- c(
              gettextf("0 < %1$s %2$s  %3$s", "|\u03B4|", "\u2264", format(round(probs_es[1], 3), nsmall = 3)),
              gettextf("%1$s < %2$s %3$s %4$s", format(round(probs_es[1], 3), nsmall = 3), "|\u03B4|", "\u2264", format(round(probs_es[2], 3), nsmall = 3)),
              gettextf("%1$s < %2$s %3$s %4$s",format(round(probs_es[2], 3), nsmall = 3), "|\u03B4|", "\u2264", format(round(probs_es[3], 3), nsmall = 3)),
              gettextf("%1$s %2$s %3$s", "|\u03B4|", "\u2265", format(round(probs_es[3], 3), nsmall = 3))
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
                    "effectSize",
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

            ps <- ttestPlotSettings

            calc <- self$options$calculation
            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            d <- ifelse(calc == "effectSize", r$es, lst$es)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt
            power <- ifelse(calc == "power",
                            r$power,
                            ifelse(calc == "sampleSize",
                                   pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power,
                                   lst$pow))

            maxn <- try(ceiling(pwr::pwr.t.test(
              d = d,
              sig.level = alpha,
              power = max(0.99, power),
              alternative = alt,
              type = private$type)$n))
            if(inherits(maxn, "try-error")) {
              image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
              return()
            }

            if (n >= maxn && n >= ps$maxn) {
              maxn <- ceiling(n * ps$max.scale)
            } else if (maxn < ps$maxn) {
              if ((ps$maxn - n) < 20) {
                maxn <- ps$maxn * ps$max.scale
              } else {
                maxn <- ps$maxn
              }
            }

            minn <- 3

            ps$maxd <- max(2, d * 1.2)

            nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))
            dd <- seq(ps$mind, ps$maxd, len = ps$lens)

            z.pwr <- try(sapply(dd, function(delta) {
                pwr::pwr.t.test(n = nn, d = delta, sig.level = alpha, alternative = alt, type = private$type)$power
            }))
            if(inherits(z.pwr, "try-error")) {
              image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
              return()
            }

            z.delta <- try(sapply(nn, function(N) {
                pwr::pwr.t.test(n = N, sig.level = alpha, power = power, alternative = alt, type = private$type)$d
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
                    "effectSize",
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

            ps <- ttestPlotSettings

            calc <- self$options$calculation
            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            d <- ifelse(calc == "effectSize", r$es, lst$es)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt
            power <- ifelse(calc == "power",
                            r$power,
                            ifelse(calc == "sampleSize",
                                   pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power,
                                   lst$pow))

            maxd <- try(pwr::pwr.t.test(n = n, power = max(0.999, power), sig.level = alpha, alternative = alt, type = private$type)$d)
            if (inherits(maxd, "try-error")) {
              maxd <- d
            }

            dd <- seq(ps$mind, maxd, len = ps$curve.n)

            y <- try(pwr::pwr.t.test(n = n, d = dd, sig.level = alpha, alternative = alt, type = private$type)$power)
            if(inherits(y, "try-error")) {
              image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the power curve. Try to enter less extreme values for the parameters"))
              return()
            }
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
                    "effectSize",
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

            ps <- ttestPlotSettings

            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            d <- ifelse(calc == "effectSize", r$es, lst$es)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt
            power <- ifelse(calc == "power",
                            r$power,
                            ifelse(calc == "sampleSize",
                                   pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power,
                                   lst$pow))

            maxn <- try(ceiling(pwr::pwr.t.test(
              d = d,
              sig.level = alpha,
              power = max(0.99999, power),
              alternative = alt,
              type = private$type)$n))

            if(inherits(maxn, "try-error")) {
              image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the 'Power Curve by N' plot. Try to enter less extreme values for the parameters"))
              return()
            } else if (n >= maxn && n >= ps$maxn) {
              maxn <- ceiling(n * ps$max.scale)
            }

            minn <- 3

            nn <- seq(minn, maxn)

            y <- try(pwr::pwr.t.test(n = nn, d = d, sig.level = alpha, alternative = alt, type = private$type)$power)
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
                    "effectSize",
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

            n <- ifelse(calc == "sampleSize", r$n, lst$n)
            d <- ifelse(calc == "effectSize", r$es, lst$es)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt
            power <- ifelse(calc == "power",
                            r$power,
                            ifelse(calc == "sampleSize",
                                   pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power,
                                   lst$pow))

            effN <- n
            df <- n - 1
            ncp <- sqrt(effN) * d

            if (alt == "two.sided") {
                crit <- qt(p = 1 - alpha / 2, df = df) / sqrt(effN)
            } else {
                crit <- qt(p = 1 - alpha, df = df) / sqrt(effN)
            }

            if (d <= 1)
              xlims <- c(qt(0.001, df), qt(0.999, df, ncp)) / sqrt(effN)
            if (d > 1)
              xlims <- c(qt(1 - 0.999^d, df), qt(0.999^(3*d^2), df, ncp)) / sqrt(effN)

            y.max <- dt(0, df) / sqrt(effN)

            xx <- seq(xlims[1], xlims[2], len = 500)
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
        },

        #### Generate synthetic dataset ----
        .generateDataset = function(r, lst) {
          datasetContainer <- self$jaspResults[["datasetcont"]]
          if (is.null(datasetContainer)) {
            # Create Container if it doesn't exist yet
            datasetContainer <- createJaspContainer(title = gettext("Synthetic Dataset"))
            datasetContainer$dependOn(c(
              "test",
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

          n <- ifelse(calc == "sampleSize", r$n, lst$n)
          d <- ifelse(calc == "effectSize", r$es, lst$es)
          alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
          alt <- lst$alt
          power <- ifelse(calc == "power",
                          r$power,
                          ifelse(calc == "sampleSize",
                                 pwr::pwr.t.test(n = n, d = d, sig.level = alpha, alternative = alt, type = private$type)$power,
                                 lst$pow))
          df <- n - 1

          if(private$type == "paired") {

            sd_1 <- self$options[["firstGroupSd"]]
            sd_2 <- self$options[["secondGroupSd"]]

            mean_2 <- self$options[["secondGroupMean"]]

            if(self$options[["setSeed"]])
              set.seed(self$options[["seed"]])

            group_1 <- rnorm(n, mean = 0, sd = sd_1)
            group_2 <- rnorm(n, mean = 0, sd = sd_2)

            group_1 <- group_1 - mean(group_1)
            group_2 <- group_2 - mean(group_2)

            group_1 <- group_1 * (sd_1 / sd(group_1))
            group_2 <- group_2 * (sd_2 / sd(group_2))

            group_2 <- group_2 + mean_2

            body <- quote({
              (mean(group_2) - mean_1) / sd(group_2 - (group_1 + mean_1))
            })
            if(self$options[["effectDirectionSyntheticDataset"]] == "less") {
              mean_1 <- uniroot(function(mean_1) eval(body) - d, c(-1e10, mean(group_2)))$root
              if(alt == "greater")
                alt <- "less"
            } else {
              mean_1 <- uniroot(function(mean_1) eval(body) + d, c(mean(group_2), 1e10))$root
              if(alt == "greater")
                alt <- "greater"
            }

            group_1 <- group_1 + mean_1


            id        <- seq.int(1, n)
            dependent_t1 <- group_1
            dependent_t2 <- group_2

            dataset <- data.frame(cbind(id, dependent_t1, dependent_t2))

            csv <- try(write.csv(dataset, self$options[["savePath"]], row.names = FALSE))
            if(inherits(csv, "try-error"))
              .quitAnalysis(gettext("The generated dataset could not be saved. Please make sure that the specified path exists and the specified csv file is closed."))

            generatedDataset <- dataset

            datasetContainer[["generatedData"]] <- generatedDataset
          } else {
            sd_1 <- self$options[["firstGroupSd"]]
            test_value <- self$options[["testValue"]]

            if(self$options[["effectDirectionSyntheticDataset"]] == "less") {
              mean_1 <- test_value - d * sd_1
              if(alt == "greater")
                alt <- "less"
            } else {
              mean_1 <- test_value + d * sd_1
              if(alt == "greater")
                alt <- "greater"
            }

            if(self$options[["setSeed"]])
              set.seed(self$options[["seed"]])

            group_1 <- rnorm(n, mean = 0, sd = sd_1)
            group_1 <- group_1 - mean(group_1)
            group_1 <- group_1 * (sd_1 / sd(group_1))
            group_1 <- group_1 + mean_1

            id        <- seq.int(1, n)
            dependent <- group_1

            dataset <- data.frame(cbind(id, dependent))

            csv <- try(write.csv(dataset, self$options[["savePath"]], row.names = FALSE))
            if(inherits(csv, "try-error"))
              .quitAnalysis(gettext("The generated dataset could not be saved. Please make sure that the specified path exists and the specified csv file is closed."))

            generatedDataset <- dataset

            datasetContainer[["generatedData"]] <- generatedDataset
          }

          #Characteristics tab
          if(private$type == "paired") {
            colNames <- c("n", "mean1", "mean2", "s1", "s2")
            colLabels <- c(
              gettext("N"),
              gettext("\u0078\u0305\u2081"),
              gettext("\u0078\u0305\u2082"),
              gettext("s\u2081"),
              gettext("s\u2082")
            )
            colType <- c("integer", "number", "number", "number", "number")

            for (i in seq_along(colNames)) {
              characteristicsTable$addColumnInfo(colNames[i],
                                  title = colLabels[i],
                                  type = colType[i]
              )
            }

            characteristicsTable[["n"]]      <- n
            characteristicsTable[["mean1"]]  <- mean_1
            characteristicsTable[["mean2"]]  <- mean_2
            characteristicsTable[["s1"]]     <- sd_1
            characteristicsTable[["s2"]]     <- sd_2
            characteristicsTable$addFootnote(gettextf("The synthetic dataset is saved as %s", self$options[["savePath"]]))

            datasetContainer[["characteristics"]] <- characteristicsTable
          } else {
            colNames <- c("n", "mean", "testValue", "s")
            colLabels <- c(
              gettext("N"),
              gettext("\u0078\u0305"),
              gettext("\u03BC\u2080"),
              gettext("s")
            )
            colType <- c("integer", "number", "number", "number")

            for (i in seq_along(colNames)) {
              characteristicsTable$addColumnInfo(colNames[i],
                                  title = colLabels[i],
                                  type = colType[i]
              )
            }

            characteristicsTable[["n"]]          <- n
            characteristicsTable[["mean"]]       <- mean_1
            characteristicsTable[["testValue"]]  <- test_value
            characteristicsTable[["s"]]          <- sd_1
            characteristicsTable$addFootnote(gettextf("The synthetic dataset is saved as %s", self$options[["savePath"]]))

            datasetContainer[["characteristics"]] <- characteristicsTable
          }

          #Post hoc power tab
          colNames <- c("es", "alt", "power", "alpha")
          colLabels <- c(
            gettext("Cohen's |\u03B4|"),
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

          powerTable[["es"]]     <- d
          powerTable[["alt"]]    <- switch(alt,
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
