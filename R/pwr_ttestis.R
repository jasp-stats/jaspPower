# Originally based on https://github.com/richarddmorey/jpower

.runTtestIS <- function(jaspResults, options) {
  stats <- .prepareStats(options)

  ## Compute results
  results <- try(.computeTtestIS(jaspResults, options, stats))
  if (inherits(results, "try-error")) {
    .quitAnalysis(gettext("Unable to compute the power results. Try to enter less extreme values for the input parameters."))
  }

  .initPowerTabTtestIS(jaspResults, options, results, stats)

  if (options$text) {
    .initPowerESTabTtestIS(jaspResults, options, results, stats)
  }

  ## Populate tables and plots
  if (options$text) {
    .populateIntro(jaspResults, options)
  }

  if (options$powerContour) {
    .preparePowerContourTtestIS(jaspResults, options, results, stats)
    if (options$text) {
      .populateContourTextTtestIS(jaspResults, options, results, stats)
    }
  }
  if (options$powerByEffectSize) {
    .preparePowerCurveESTtestIS(jaspResults, options, results, stats)
    if (options$text) {
      .populatePowerCurveESTextTtestIS(jaspResults, options, results, stats)
    }
  }
  if (options$powerBySampleSize) {
    .preparePowerCurveNTtestIS(jaspResults, options, results, stats)
    if (options$text) {
      .populatePowerCurveNTextTtestIS(jaspResults, options, results, stats)
    }
  }
  if (options$powerDemonstration) {
    .preparePowerDistTtestIS(jaspResults, options, results, stats)
    if (options$text) {
      .populateDistTextTtestIS(jaspResults, options, results, stats)
    }
  }
  if (options$saveDataset && options$savePath != "") {
    .generateDatasetTtestIS(jaspResults, options, results, stats)
  }
}

#### Compute results ----
.computeTtestIS <- function(jaspResults, options, stats) {
  ## Compute numbers for table
  pow.n <- NULL
  pow.es <- NULL
  pow.pow <- NULL
  if (options$calculation == "sampleSize") {
    pow.n <- ceiling(.pwrT2NRatio(n_ratio = stats$n_ratio, d = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt))
  }
  if (options$calculation == "effectSize") {
    pow.es <- .pwrT2NTest(n1 = stats$n1, n2 = stats$n2, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$d
  }
  if (options$calculation == "power") {
    pow.pow <- .pwrT2NTest(n1 = stats$n1, n2 = stats$n2, d = stats$es, sig.level = stats$alpha, alternative = stats$alt)$power
  }

  return(list(n1 = pow.n, n2 = ceiling(pow.n * stats$n_ratio), es = pow.es, power = pow.pow))
}

#### Init table ----
.initPowerTabTtestIS <- function(jaspResults, options, results, stats) {
  table <- jaspResults[["powertab"]]

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
    jaspResults[["powertab"]] <- table
  } else {
    return()
  }

  calc <- options$calculation

  if (calc == "sampleSize") {
    order <- c(1, 2, 3, 4, 5)
  } else if (calc == "effectSize") {
    order <- c(3, 1, 2, 4, 5)
  } else if (calc == "power") {
    order <- c(4, 1, 2, 3, 5)
  } else {
    order <- c(5, 1, 2, 3, 4)
  }

  colNames <- c("n1", "n2", "effectSize", "power", "alpha")
  colLabels <- c(
    "N\u2081",
    "N\u2082",
    gettext("Cohen's |\u03B4|"),
    gettext("Power"),
    "\u03B1"
  )
  colType <- c("integer", "integer", "number", "number", "number")

  for (i in seq_along(order)) {
    table$addColumnInfo(colNames[order[i]],
      title = colLabels[order[i]],
      overtitle = if (calc == "sampleSize" && i > 2 || calc != "sampleSize" && i > 1) "User Defined" else NULL,
      type = colType[order[i]]
    )
  }

  row <- list()
  for (i in 2:5) {
    row[[colNames[order[i]]]] <- options[[colNames[order[i]]]]
  }

  if (options$calculation != "sampleSize") {
    row[["n1"]] <- options[["sampleSize"]]
    row[["n2"]] <- ceiling(options[["sampleSize"]] * options[["sampleSizeRatio"]])
  }

  table$addRows(rowNames = 1, row)

  .populatePowerTabTtestIS(jaspResults, options, results, stats)
}
.initPowerESTabTtestIS <- function(jaspResults, options, results, stats) {
  table <- jaspResults[["powerEStab"]]

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
    jaspResults[["powerEStab"]] <- table
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

  .populatePowerESTabTtestIS(jaspResults, options, results, stats)
}
.populatePowerESTabTtestIS <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["tabText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text"))
    html$position <- 3
    jaspResults[["tabText"]] <- html
  }

  # This table is created in init
  table <- jaspResults[["powerEStab"]]

  ## Get options from interface
  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
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

  hypo_text <- "|<i>\u03B4</i>|><i>0</i>"

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
    .pwrT2NTest(
      n1 = n1, n2 = n2,
      sig.level = alpha, power = p,
      alternative = alt
    )$d
  }))
  if (inherits(probs_es, "try-error")) {
    table$setError(gettext("The specified design leads to (an) unsolvable equation(s) while computing the values for this power table. Try to enter less extreme values for the parameters."))
    return()
  }

  esText <- c(
    gettextf("0 < %1$s %2$s  %3$s", "|\u03B4|", "\u2264", format(round(probs_es[1], 3), nsmall = 3)),
    gettextf("%1$s < %2$s %3$s %4$s", format(round(probs_es[1], 3), nsmall = 3), "|\u03B4|", "\u2264", format(round(probs_es[2], 3), nsmall = 3)),
    gettextf("%1$s < %2$s %3$s %4$s", format(round(probs_es[2], 3), nsmall = 3), "|\u03B4|", "\u2264", format(round(probs_es[3], 3), nsmall = 3)),
    gettextf("%1$s %2$s %3$s", "|\u03B4|", "\u2265", format(round(probs_es[3], 3), nsmall = 3))
  )

  cols <- list("es" = esText)
  table$addColumns(cols)
}
#### Populate table ----
.populatePowerTabTtestIS <- function(jaspResults, options, r, lst) {
  table <- jaspResults[["powertab"]]

  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  if (calc == "sampleSize") {
    table$addColumns(list(n1 = n1))
    table$addColumns(list(n2 = n2))
    if (round(.pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power, 3) == 1) {
      table$addFootnote(gettextf("Due to the rounding of sample sizes, the actual power can deviate from the target power. <b>Actual power: >0.999"))
    } else {
      table$addFootnote(gettextf(
        "Due to the rounding of sample sizes, the actual power can deviate from the target power. <b>Actual power: %1$s</b>",
        round(.pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power, 3)
      ))
    }
  } else {
    row <- list()
    row[[calc]] <- r[[switch(calc,
      "effectSize" = "es",
      calc
    )]]
    table$addColumns(row)
  }
}

#### Plot functions ----
.preparePowerContourTtestIS <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerContour"]]
  if (is.null(image)) {
    image <- createJaspPlot(
      title = gettext("Power Contour"),
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
      "powerContour"
    ))
    image$position <- 5
    jaspResults[["powerContour"]] <- image
  }

  ps <- .pwrPlotDefaultSettings

  calc <- options$calculation

  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )



  maxn <- try(.pwrT2NRatio(
    n_ratio = n_ratio,
    power = max(0.99, power),
    d = d,
    sig.level = alpha,
    alternative = alt
  ))
  if (inherits(maxn, "try-error")) {
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

  minn <- ifelse(n_ratio < 1,
    max(ceiling(3 / (1 + n_ratio)), 2 / n_ratio),
    max(ceiling(3 / (1 + n_ratio)), 2 * n_ratio)
  )

  ps$maxd <- max(2, d * 1.2)

  nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))
  dd <- seq(ps$mind, ps$maxd, len = ps$lens)
  nn2 <- ceiling(n_ratio * nn)

  z.pwr <- try(sapply(dd, function(delta) {
    .pwrT2NTest(
      n1 = nn, n2 = nn2,
      d = delta,
      sig.level = alpha,
      alternative = alt
    )$power
  }))
  if (inherits(z.pwr, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
    return()
  }

  z.delta <- try(sapply(nn, function(N) {
    n2 <- ceiling(n_ratio * N)
    .pwrT2NTest(
      n1 = N, n2 = n2,
      sig.level = alpha,
      power = power,
      alternative = alt
    )$d
  }))
  if (inherits(z.delta, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
    return()
  }

  state <- list(
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
  image$plotObject <- .plotPowerContour(options, state = state, ggtheme = .pwrPlotTheme())
}
.populateContourTextTtestIS <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["contourText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerContour"))
    html$position <- 6
    jaspResults[["contourText"]] <- html
  }

  str <- gettext(
    "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical effect size and the sample sizes in the design. As we increase the sample sizes, smaller effect sizes become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only larger effect sizes, smaller sample sizes are needed. The point shows the power of the specified design and effect size."
  )

  html[["text"]] <- str
}
.preparePowerCurveESTtestIS <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerCurveES"]]
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
    jaspResults[["powerCurveES"]] <- image
  }

  ps <- .pwrPlotDefaultSettings

  calc <- options$calculation

  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  maxd <- try(.pwrT2NTest(n1 = n1, n2 = n2, power = max(0.999, power), sig.level = alpha, alternative = alt)$d)
  if (inherits(maxd, "try-error")) {
    maxd <- d
  }

  dd <- seq(ps$mind, maxd, len = ps$curve.n)

  y <- try(.pwrT2NTest(n1 = n1, n2 = n2, d = dd, sig.level = alpha, alternative = alt)$power)
  if (inherits(y, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the power curve. Try to enter less extreme values for the parameters"))
    return()
  }
  cols <- ps$pal(ps$pow.n.levels)
  yrect <- seq(0, 1, 1 / ps$pow.n.levels)

  state <- list(cols = cols, dd = dd, y = y, yrect = yrect, n1 = n1, n2 = n2, alpha = alpha, delta = d, pow = power)
  image$plotObject <- .plotPowerCurveES(options, state = state, ggtheme = .pwrPlotTheme())
}
.populatePowerCurveESTextTtestIS <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["curveESText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerByEffectSize"))
    html$position <- 8
    jaspResults[["curveESText"]] <- html
  }

  ## Get options from interface
  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  d <- ifelse(calc == "effectSize",
    r$es,
    ifelse(calc == "sampleSize",
      .pwrT2NTest(n1 = n1, n2 = n2, power = power, sig.level = alpha, alternative = alt)$d,
      lst$es
    )
  )
  d <- round(d, 3)

  n_text <- ifelse(n1 == n2,
    gettextf("sample sizes of %1$s in each group", n1),
    gettextf("group sample sizes of %1$s and %2$s, respectively", n1, n2)
  )

  if (alt == "two.sided") {
    tail_text <- gettext("two-sided")
    alt_text <- gettext("|<i>\u03B4</i>|<i>\u003E</i>")
    crit_text <- gettext("criteria")
  } else {
    tail_text <- gettext("one-sided")
    alt_text <- gettext("|<i>\u03B4</i>|<i>\u003E</i>")
    crit_text <- gettext("criterion")
  }

  if (calc == "power") {
    pwr_string <- gettextf("have power of at least %1$s", round(power, 3))
  } else {
    pwr_string <- gettextf("only be sufficiently sensitive (power >%1$s)", round(power, 3))
  }

  d50 <- try(.pwrT2NTest(n1 = n1, n2 = n2, sig.level = alpha, power = .5, alternative = alt)$d)
  if (inherits(d50, "try-error")) {
    return()
  }

  str <- gettextf(
    "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. If we obtained %1$s our test and design would %2$s to effect sizes of %3$s%4$s. <p>We would be more than likely to miss (power less than 50%%) effect sizes less than <i>%5$s=</i>%6$s.",
    n_text, pwr_string, alt_text, d, "|\u03B4|", round(d50, 3)
  )

  html[["text"]] <- str
}
.preparePowerCurveNTtestIS <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerCurveN"]]
  if (is.null(image)) {
    image <- createJaspPlot(title = "Power Curve by N", width = 400, height = 350)
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
    jaspResults[["powerCurveN"]] <- image
  }

  calc <- options$calculation

  ps <- .pwrPlotDefaultSettings

  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  n_ratio <- lst$n_ratio
  d <- ifelse(calc == "effectSize", r$es, lst$es)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  maxn <- try(.pwrT2NRatio(
    n_ratio = n_ratio,
    power = max(0.99999, power),
    d = d,
    sig.level = alpha,
    alternative = alt
  ))

  if (inherits(maxn, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the 'Power Curve by N' plot. Try to enter less extreme values for the parameters"))
    return()
  } else if (n1 >= maxn && n1 >= ps$maxn) {
    maxn <- ceiling(n1 * ps$max.scale)
  }

  minn <- ifelse(n_ratio < 1,
    max(ceiling(3 / (1 + n_ratio)), 2 / n_ratio),
    max(ceiling(3 / (1 + n_ratio)), 2 * n_ratio)
  )

  nn <- seq(minn, maxn)

  y <- try(.pwrT2NTest(
    n1 = nn,
    n2 = ceiling(nn * lst$n_ratio),
    d = d, sig.level = alpha, alternative = alt
  )$power)
  if (inherits(y, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the 'Power Curve by N' plot. Try to enter less extreme values for the parameters"))
    return()
  }

  cols <- ps$pal(ps$pow.n.levels)
  yrect <- seq(0, 1, 1 / ps$pow.n.levels)

  lims <- data.frame(
    xlim = c(minn, maxn),
    ylim = c(0, 1)
  )

  state <- list(n = n1, cols = cols, nn = nn, y = y, yrect = yrect, lims = lims, delta = d, alpha = alpha, n_ratio = n_ratio, pow = power)
  image$plotObject <- .plotPowerCurveN(options, state = state, ggtheme = .pwrPlotTheme())
}
.preparePowerDistTtestIS <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerDist"]]
  if (is.null(image)) {
    image <- createJaspPlot(title = "Power Demonstration", width = 400, height = 300)
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
    jaspResults[["powerDist"]] <- image
  }

  calc <- options$calculation

  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  effN <- n1 * n2 / (n1 + n2)
  df <- n1 + n2 - 2
  ncp <- sqrt(effN) * d

  if (alt == "two.sided") {
    crit <- qt(p = 1 - alpha / 2, df = df) / sqrt(effN)
  } else {
    crit <- qt(p = 1 - alpha, df = df) / sqrt(effN)
  }

  xlims <- c(qt(.001, df), qt(ifelse(d > 1, .999^(d^2), 0.999), df, ncp)) / sqrt(effN)

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

  state <- list(curves = curves, rect = rect, lims = lims)
  image$plotObject <- .plotPowerDist(options, state = state, ggtheme = .pwrPlotTheme())
}
.populatePowerCurveNTextTtestIS <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["curveNText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerBySampleSize"))
    html$position <- 10
    jaspResults[["curveNText"]] <- html
  }

  ## Get options from interface
  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
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
    alt_text <- gettext("|<i>\u03B4</i>|<i>\u003E</i>0,")
    crit_text <- gettext("criteria")
  } else {
    tail_text <- gettext("one-sided")
    alt_text <- gettext("|<i>\u03B4</i>|<i>\u003E</i>0,")
    crit_text <- gettext("criterion")
  }

  str <- gettextf(
    "<p>The power curve above shows how the sensitivity of the test and design is larger for larger sample sizes. In order for our test and design to have sufficient sensitivity (power > %1$s) to detect that %2$s when the effect size is %3$s or larger, we would need %4$s.",
    round(power, 3), alt_text, d, n_text
  )

  html[["text"]] <- str
}
.populateDistTextTtestIS <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["distText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerDemonstration"))
    html$position <- 12
    jaspResults[["distText"]] <- html
  }

  ## Get options from interface
  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
  d <- round(d, 2)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  n_text <- ifelse(n1 == n2,
    gettextf("a sample size of %1$s in each group", n1),
    gettextf("group sample sizes of %1$s and %2$s, respectively", n1, n2)
  )

  if (alt == "two.sided") {
    tail_text <- gettext("two-sided")
    null_text <- gettext("<i>|\u03B4|=</i>0,")
    alt_text <- gettext("<i>|\u03B4|\u2265</i>")
    crit_text <- gettext("criteria")
  } else {
    tail_text <- gettext("one-sided")
    null_text <- gettext("<i>|\u03B4|\u2264</i>0,")
    alt_text <- gettext("<i>|\u03B4|\u2265</i>")
    crit_text <- gettext("criterion")
  }

  str <- paste(
    "<p>",
    gettextf(
      "The figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> effect size when <i>%1$s=</i>0 (left), and when <i>%2$s=</i>%3$s (right).",
      "|\u03B4|", "|\u03B4|", d
    ),
    gettextf("Both assume %1$s.", n_text),
    "</p><p>",
    gettextf("The vertical dashed lines show the %1$s we would set for a %2$s test with <i>\u03B1=</i>%3$s.", crit_text, tail_text, alpha),
    gettextf("When the observed effect size is far enough away from 0 to be more extreme than the %1$s we say we 'reject' the null hypothesis.", crit_text),
    gettextf("If the null hypothesis were true and %1$s the evidence would lead us to wrongly reject the null hypothesis at most %2$s%% of the time.", null_text, 100 * alpha),
    "</p><p>",
    gettextf(
      "On the other hand, if <i>%1$s%2$s</i>%3$s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%4$s%5$s</i>0 &mdash; at least %6$s%% of the time.",
      "|\u03B4|", "\u2265", d, "|\u03B4|", ">", 100 * round(power, 3)
    ),
    gettextf("The design's power for detecting effects of %1$s%2$s is thus %3$s.", alt_text, d, round(power, 3)),
    "</p>"
  )


  html[["text"]] <- str
}

#### Generate synthetic dataset ----
.generateDatasetTtestIS <- function(jaspResults, options, r, lst) {
  datasetContainer <- jaspResults[["datasetcont"]]
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
    jaspResults[["datasetcont"]] <- datasetContainer

    generatedDataset <- createJaspState()
    characteristicsTable <- createJaspTable(title = gettext("Characteristics"))
    powerTable <- createJaspTable(gettext("Post Hoc Power Analysis"))
  } else {
    return()
  }

  # Generate dataset
  if (!grepl(".csv", options[["savePath"]], fixed = TRUE) && !grepl(".txt", options[["savePath"]], fixed = TRUE)) {
    .quitAnalysis(gettext("The generated dataset must be saved as a .csv or .txt file."))
  }

  calc <- options$calculation

  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  d <- ifelse(calc == "effectSize", r$es, lst$es)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwrT2NTest(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )
  df <- n1 + n2 - 2

  sd_1 <- options[["firstGroupSd"]]
  sd_2 <- options[["secondGroupSd"]]

  mean_2 <- options[["secondGroupMean"]]
  if (options[["effectDirectionSyntheticDataset"]] == "less") {
    mean_1 <- mean_2 - d * sqrt(((n1 - 1) * sd_1^2 + (n2 - 1) * sd_2^2) / df)
    if (alt == "greater") {
      alt <- "less"
    }
  } else {
    mean_1 <- mean_2 + d * sqrt(((n1 - 1) * sd_1^2 + (n2 - 1) * sd_2^2) / df)
    if (alt == "greater") {
      alt <- "greater"
    }
  }

  if (options[["setSeed"]]) {
    set.seed(options[["seed"]])
  }

  group_1 <- rnorm(n1, mean = 0, sd = sd_1)
  group_2 <- rnorm(n2, mean = 0, sd = sd_2)

  group_1 <- group_1 - mean(group_1)
  group_2 <- group_2 - mean(group_2)

  group_1 <- group_1 * (sd_1 / sd(group_1))
  group_2 <- group_2 * (sd_2 / sd(group_2))

  group_1 <- group_1 + mean_1
  group_2 <- group_2 + mean_2

  id <- seq.int(1, length(group_1) + length(group_2))
  dependent <- c(group_1, group_2)
  group <- c(rep(1, length(group_1)), rep(2, length(group_2)))

  dataset <- data.frame(cbind(id, dependent, group))

  csv <- try(write.csv(dataset, options[["savePath"]], row.names = FALSE))
  if (inherits(csv, "try-error")) {
    .quitAnalysis(gettext("The generated dataset could not be saved. Please make sure that the specified path exists and the specified csv file is closed."))
  }

  generatedDataset <- dataset

  datasetContainer[["generatedData"]] <- generatedDataset

  # Characteristics tab
  colNames <- c("n1", "n2", "mean1", "mean2", "s1", "s2")
  colLabels <- c(
    gettext("N\u2081"),
    gettext("N\u2082"),
    gettext("\u0078\u0305\u2081"),
    gettext("\u0078\u0305\u2082"),
    gettext("s\u2081"),
    gettext("s\u2082")
  )
  colType <- c("integer", "integer", "number", "number", "number", "number")

  for (i in seq_along(colNames)) {
    characteristicsTable$addColumnInfo(colNames[i],
      title = colLabels[i],
      type = colType[i]
    )
  }

  characteristicsTable[["n1"]] <- n1
  characteristicsTable[["n2"]] <- n2
  characteristicsTable[["mean1"]] <- mean_1
  characteristicsTable[["mean2"]] <- mean_2
  characteristicsTable[["s1"]] <- sd_1
  characteristicsTable[["s2"]] <- sd_2
  characteristicsTable$addFootnote(gettextf("The synthetic dataset is saved as %s", options[["savePath"]]))

  datasetContainer[["characteristics"]] <- characteristicsTable

  # Post hoc Power tab
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

  powerTable[["es"]] <- d
  powerTable[["alt"]] <- switch(alt,
    "two.sided" = "Two-sided",
    "less" = "Less (One-sided)",
    "greater" = "Greater (One-sided)"
  )
  powerTable[["power"]] <- power
  powerTable[["alpha"]] <- alpha

  datasetContainer[["posthocpower"]] <- powerTable
}
