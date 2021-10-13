# Originally based on https://github.com/richarddmorey/jpower

ttestPSClass <- R6::R6Class(
    "ttestPSClass",
    inherit = tTestBaseClass,
    private = list(
        #### Member variables ----
        probs_es = NULL,
        type = "paired",

        #### Compute results ----
        .compute = function(stats) {
            ## Compute numbers for table
            pow.n <- ceiling(pwr::pwr.t.test(d = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt, type = private$type)$n)
            pow.es <- pwr::pwr.t.test(n = stats$n, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt, type = private$type)$d
            pow.pow <- pwr::pwr.t.test(n = stats$n, d = stats$es, sig.level = stats$alpha, alternative = stats$alt, type = private$type)$power

            probs <- c(.5, .8, .95)
            probs_es <- sapply(probs, function(p) {
                pwr::pwr.t.test(
                    n = stats$n, sig.level = stats$alpha, power = stats$p,
                    alternative = stats$alt, type = private$type
                )$d
            })
            private$probs_es <- probs_es

            return(list(n = pow.n, es = pow.es, power = pow.pow))
        },

        #### Init table ----
        .initPowerTab = function(results) {
            table <- self$jaspResults[["powertab"]]
            if (is.null(table)) {
                # Create table if it doesn't exist yet
                table <- createJaspTable(title = "A Priori Power Analysis")
                table$dependOn(c(
                    "test",
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
                  order <- c(1, 2, 3, 4)
              } else if (calc == "es") {
                  order <- c(2, 1, 3, 4)
              } else if (calc == "power") {
                  order <- c(3, 1, 2, 4)
              } else {
                  order <- c(4, 1, 2, 3)
              }

            colNames <- c("n", "es", "power", "alpha")
            colLabels <- c("N", "Effect Size", "Power", "\u03B1")
            colType <- c("integer", "number", "number", "number")

            for (i in seq_along(order)) {
                table$addColumnInfo(colNames[order[i]],
                    title = colLabels[order[i]],
                    overtitle = if (i > 1) "User Defined" else NULL,
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
                table <- createJaspTable(title = "Power by Effect Size")
                table$dependOn(c(
                    "test",
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
                title = "True effect size",
                type = "string"
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

            pow <- c("\u226450%", "50% \u2013 80%", "80% \u2013 95%", "\u226595%")
            desc <- c("Likely miss", "Good chance of missing", "Probably detect", "Almost surely detect")

            for (i in 1:4) {
                row <- list("power" = pow[i], "desc" = desc[i])
                table$addRows(rowNames = i, row)
            }

            private$.populatePowerESTab()
        },

        #### Populate texts ----
        .populateIntro = function() {
            calc <- self$options$calc

            html <- self$jaspResults[["intro"]]
            if (is.null(html)) {
                html <- createJaspHtml(title = "Introduction")
                html$dependOn(c("test", "text"))
                html$position <- 1
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
                html <- createJaspHtml()
                html$dependOn(c("test", "text"))
                html$position <- 3
                self$jaspResults[["tabText"]] <- html
            }

            ## Get options from interface
            calc <- self$options$calc
            n <- ifelse(calc == "n", r$n, lst$n)
            d <- ifelse(calc == "es", r$es, lst$es)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt

            n_text <- paste0("a sample size of ", n)

            tail_text <- ifelse(alt == "two.sided",
                "two-sided",
                "one-sided"
            )

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
                    "<i>\u03B4\u2265</i>", d, " with a probability of at least",
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
            d <- ifelse(calc == "es", r$es, lst$es)
            d <- round(d, 3)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt

            n_text <- paste0("sample sizes of ", n)

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

            d50 <- pwr::pwr.t.test(n = n, sig.level = alpha, power = .5, alternative = alt, type = private$type)$d

            str <- paste0(
                "<p>The power curve above shows how the sensitivity of the test and design ",
                "is larger for larger effect sizes. If we obtained ", n_text,
                " our test and design would ", pwr_string, " to effect sizes of ", alt_text, d, ". ",
                "<p>We would be more than likely to miss (power less than 50%) effect sizes less than <i>\u03B4=</i>",
                round(d50, 3), "."
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
            d <- ifelse(calc == "es", r$es, lst$es)
            d <- round(d, 3)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alt <- lst$alt

            n_text <- paste0("sample sizes of at least ", n)

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
            d <- ifelse(calc == "es", r$es, lst$es)
            d <- round(d, 2)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt

            n_text <- paste0("a sample size of ", n)

            if (alt == "two.sided") {
                tail_text <- "two-sided"
                null_text <- "<i>\u03B4=</i>0,"
                alt_text <- "<i>|\u03B4|\u2265</i>"
                crit_text <- "criteria"
            } else {
                tail_text <- "one-sided"
                null_text <- "<i>\u03B4\u2264</i>0,"
                alt_text <- "<i>|\u03B4|\u2265</i>"
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
        },

        #### Populate table ----
        .populatePowerTab = function(results) {
            table <- self$jaspResults[["powertab"]]
            calc <- self$options$calc

            row <- list()
            row[[calc]] <- results[[calc]]

            table$addColumns(row)
        },
        .populatePowerESTab = function() {
            table <- self$jaspResults[["powerEStab"]]

            probs_es <- private$probs_es

            esText <- c(
                paste0("0 < \u03B4 \u2264 ", format(round(probs_es[1], 3), nsmall = 3)),
                paste0(format(round(probs_es[1], 3), nsmall = 3), " < \u03B4 \u2264 ", format(round(probs_es[2], 3), nsmall = 3)),
                paste0(format(round(probs_es[2], 3), nsmall = 3), " < \u03B4 \u2264 ", format(round(probs_es[3], 3), nsmall = 3)),
                paste0("\u03B4 \u2265 ", format(round(probs_es[3], 3), nsmall = 3))
            )

            cols <- list("es" = esText)
            table$addColumns(cols)
        },

        #### Plot functions ----
        .preparePowerContour = function(r, lst) {
            image <- self$jaspResults[["powerContour"]]
            if (is.null(image)) {
                image <- createJaspPlot(title="Power Contour", width=400, height=350)
                image$dependOn(c(
                    "test",
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

            calc <- self$options$calc

            n <- ifelse(calc == "n", r$n, lst$n)
            d <- ifelse(calc == "es", r$es, lst$es)
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
                pwr::pwr.t.test(nn, d = delta, sig.level = alpha, alternative = alt, type = private$type)$power
            })

            z.delta <- sapply(nn, function(N) {
                pwr::pwr.t.test(N, sig.level = alpha, power = power, alternative = alt, type = private$type)$d
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
                image <- createJaspPlot(title="Power Curve by Effect Size", width=400, height=350)
                image$dependOn(c(
                    "test",
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

            calc <- self$options$calc

            n <- ifelse(calc == "n", r$n, lst$n)
            d <- ifelse(calc == "es", r$es, lst$es)
            power <- ifelse(calc == "power", r$power, lst$pow)
            alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
            alt <- lst$alt

            dd <- seq(ps$mind, ps$maxd, len = ps$curve.n)

            y <- pwr::pwr.t.test(n = n, d = dd, sig.level = alpha, alternative = alt, type = private$type)$power
            cols <- ps$pal(ps$pow.n.levels)
            yrect <- seq(0, 1, 1 / ps$pow.n.levels)

            state = list(cols = cols, dd = dd, y = y, yrect = yrect, n = n, alpha = alpha, delta = d, pow = power)
            image$plotObject <- private$.powerCurveES(state = state, ggtheme = pwr_plot_theme())
        },
        .preparePowerCurveN = function(r, lst) {
            image <- self$jaspResults[["powerCurveN"]]
            if (is.null(image)) {
                image <- createJaspPlot(title="Power Curve by N", width=400, height=350)
                image$dependOn(c(
                    "test",
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

            n <- ifelse(calc == "n", r$n, lst$n)
            d <- ifelse(calc == "es", r$es, lst$es)
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

            y <- pwr::pwr.t.test(n = nn, d = d, sig.level = alpha, alternative = alt, type = private$type)$power

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
                image <- createJaspPlot(title="Power Demonstration", width=400, height=300)
                image$dependOn(c(
                    "test",
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
            d <- ifelse(calc == "es", r$es, lst$es)
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