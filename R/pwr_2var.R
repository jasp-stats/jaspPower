# Originally based on https://github.com/richarddmorey/jpower

.runTest2Var <- function(jaspResults, options) {
  stats <- .prepareStats(options)

  ## Compute results
  results <- try(.computeTest2Var(jaspResults, options, stats))
  if (inherits(results, "try-error")) {
    .quitAnalysis(gettext("Unable to compute the power results. Try to enter less extreme values for the input parameters."))
  }

  .initPowerTabTest2Var(jaspResults, options, results, stats)

  if (options$text) {
    .initPowerESTabTest2Var(jaspResults, options, results, stats)
  }

  ## Populate tables and plots
  if (options$text) {
    .populateIntro(jaspResults, options)
  }

  if (options$powerContour) {
    .preparePowerContourTest2Var(jaspResults, options, results, stats)
    if (options$text) {
      .populateContourTextTest2Var(jaspResults, options, results, stats)
    }
  }
  if (options$powerByEffectSize) {
    .preparePowerCurveESTest2Var(jaspResults, options, results, stats)
    if (options$text) {
      .populatePowerCurveESTextTest2Var(jaspResults, options, results, stats)
    }
  }
  if (options$powerBySampleSize) {
    .preparePowerCurveNTest2Var(jaspResults, options, results, stats)
    if (options$text) {
      .populatePowerCurveNTextTest2Var(jaspResults, options, results, stats)
    }
  }
  if (options$powerDemonstration) {
    .preparePowerDistTest2Var(jaspResults, options, results, stats)
    if (options$text) {
      .populateDistTextTest2Var(jaspResults, options, results, stats)
    }
  }
  if (options$saveDataset && options$savePath != "") {
    .generateDatasetTest2Var(jaspResults, options, results, stats)
  }
}

#### Compute results ----
.computeTest2Var <- function(jaspResults, options, stats) {
  ## Compute numbers for table
  pow.n <- NULL
  pow.es <- NULL
  pow.pow <- NULL
  if (options$calculation == "sampleSize") {
    pow.n <- ceiling(.pwr2Var2NTest(n.ratio = stats$n_ratio, rho = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)$n)
  }
  if (options$calculation == "effectSize") {
    pow.es <- .pwr2Var2NTest(n = stats$n, n.ratio = stats$n2 / stats$n1, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$rho
  }
  if (options$calculation == "power") {
    pow.pow <- .pwr2Var2NTest(n = stats$n1, n.ratio = stats$n2 / stats$n1, rho = stats$es, sig.level = stats$alpha, alternative = stats$alt)$power
  }

  return(list(n1 = pow.n, n2 = ceiling(pow.n * stats$n_ratio), es = pow.es, power = pow.pow))
}

#### Init table ----
.initPowerTabTest2Var <- function(jaspResults, options, results, stats) {
  table <- jaspResults[["powertab"]]

  if (is.null(table)) {
    # Create table if it doesn't exist yet
    table <- createJaspTable(title = gettext("A Priori Power Analysis"))
    table$dependOn(c(
      "test",
      "effectSize",
      "varianceRatio",
      "effectDirection",
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
    gettext("Variance ratio (\u03C1)"),
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

  .populatePowerTabTest2Var(jaspResults, options, results, stats)
}
.initPowerESTabTest2Var <- function(jaspResults, options, results, stats) {
  table <- jaspResults[["powerEStab"]]

  if (is.null(table)) {
    # Create table if it doesn't exist yet
    table <- createJaspTable(title = gettext("Power by variance ratio"))
    table$dependOn(c(
      "test",
      "effectSize",
      "varianceRatio",
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
    jaspResults[["powerEStab"]] <- table
  } else {
    return()
  }

  table$addColumnInfo(
    name = "es",
    title = gettext("True variance ratio"),
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

  .populatePowerESTabTest2Var(jaspResults, options, results, stats)
}
.populatePowerESTabTest2Var <- function(jaspResults, options, r, lst) {
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
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
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

  sign <- ifelse(d > 1, "\u2265", "\u2264")


  if (calc == "sampleSize") {
    str <- gettextf(
      "We would need %1$s to reliably (with probability greater than %2$s) detect a variance ratio of <i>%3$s%4$s</i>%5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
      n_text, power, "\u03C1", sign, d, tail_text, alpha
    )
  } else if (calc == "effectSize") {
    str <- gettextf(
      "A design with %1$s will reliably (with probability greater than %2$s) detect variance ratios of <i>%3$s%4$s</i>%5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
      n_text, power, "\u03C1", sign, round(d, 3), tail_text, alpha
    )
  } else if (calc == "power") {
    str <- gettextf(
      "A design with %1$s can detect variance ratios of %2$s<i>%3$s</i>%4$s with a probability of at least %5$s, assuming a %6$s criterion for detection that allows for a maximum Type I error rate of <i>\u03B1=</i>%7$s.",
      n_text, "\u03C1", sign, round(d, 3), round(power, 3), tail_text, alpha
    )
  }

  hypo_text <- ifelse(alt == "two.sided",
    gettextf("<i>%1$s</i>%2$s1", "\u03C1", "\u2260"),
    ifelse(alt == "less", gettext("<i>\u03C1<1</i>"), gettext("<i>\u03C1>1</i>"))
  )

  str <- paste0(
    str,
    gettextf(
      "<p>To evaluate the design specified in the table, we can consider how sensitive it is to true effects of increasing sizes; that is, are we likely to correctly conclude that %1$s when the variance ratio is extreme enough to care about?",
      hypo_text
    )
  )

  html[["text"]] <- str


  probs <- c(.5, .8, .95)
  probs_es <- try(sapply(probs, function(p) {
    if (alt == "two.sided" && d < 1) {
      .pwr2Var2NTest(
        n = n1, n.ratio = n_ratio, sig.level = alpha, power = p,
        alternative = alt
      )$rho[2]
    } else {
      .pwr2Var2NTest(
        n = n1, n.ratio = n_ratio, sig.level = alpha, power = p,
        alternative = alt
      )$rho[1]
    }
  }))
  if (inherits(probs_es, "try-error")) {
    table$setError(gettext("The specified design leads to (an) unsolvable equation(s) while computing the values for this power table. Try to enter less extreme values for the parameters."))
    return()
  }
  sign1 <- ifelse(alt == "less" || (alt == "two.sided" && d < 1), ">", "<")
  sign2 <- ifelse(alt == "less" || (alt == "two.sided" && d < 1), "\u2265", "\u2264")
  sign3 <- ifelse(alt == "less" || (alt == "two.sided" && d < 1), "\u2264", "\u2265")

  esText <- c(
    gettextf("1 %1$s %2$s %3$s  %4$s", sign1, "\u03C1", sign2, format(round(as.numeric(probs_es[1]), 3), nsmall = 3)),
    gettextf("%1$s %2$s %3$s %4$s %5$s", format(round(as.numeric(probs_es[1]), 3), nsmall = 3), sign1, "\u03C1", sign2, format(round(as.numeric(probs_es[2]), 3), nsmall = 3)),
    gettextf("%1$s %2$s %3$s %4$s %5$s", format(round(as.numeric(probs_es[2]), 3), nsmall = 3), sign1, "\u03C1", sign2, format(round(as.numeric(probs_es[3]), 3), nsmall = 3)),
    gettextf("%1$s %2$s %3$s", "\u03C1", sign3, format(round(as.numeric(probs_es[3]), 3), nsmall = 3))
  )

  cols <- list("es" = esText)
  table$addColumns(cols)
  if (alt == "two.sided" && calc != "effectSize") {
    table$addFootnote(gettext("To improve readability of this power table for a 'Two-sided' alternative hypothesis, only the intervals in the direction of the specified variance ratio are displayed under 'True variance ratio'"))
  }
}
#### Populate table ----
.populatePowerTabTest2Var <- function(jaspResults, options, r, lst) {
  table <- jaspResults[["powertab"]]

  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt


  if (calc == "sampleSize") {
    table$addColumns(list(n1 = r[["n1"]]))
    table$addColumns(list(n2 = r[["n2"]]))
  } else if (calc == "effectSize") {
    row <- list()
    row[[calc]] <- d
    table$addColumns(row)
  } else {
    row <- list()
    row[[calc]] <- r[[switch(calc,
      "sampleSize" = "n",
      calc
    )]]
    table$addColumns(row)
  }
  if (calc == "sampleSize") {
    if (round(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power, 3) == 1) {
      table$addFootnote(gettext("Due to the rounding of the sample size, the actual power can deviate from the target power. <b>Actual power: >0.999"))
    } else {
      table$addFootnote(gettextf(
        "Due to the rounding of the sample size, the actual power can deviate from the target power. <b>Actual power: %1$s</b>",
        round(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power, 3)
      ))
    }
  }
}

#### Plot functions ----
.preparePowerContourTest2Var <- function(jaspResults, options, r, lst) {
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
      "varianceRatio",
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
    jaspResults[["powerContour"]] <- image
  }

  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  ps <- .pwrPlotDefaultSettings

  maxn <- try(ceiling(.pwr2Var2NTest(
    n.ratio = n_ratio,
    power = max(0.99, power),
    rho = d,
    sig.level = alpha,
    alternative = alt
  )$n))
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

  minn <- 2
  try <- try(.pwr2Var2NTest(n = minn, n.ratio = n_ratio, sig.level = alpha, power = power, alternative = alt))
  while (inherits(try, "try-error")) {
    minn <- minn + 1
    try <- try(.pwr2Var2NTest(n = minn, n.ratio = n_ratio, sig.level = alpha, power = power, alternative = alt))
  }

  nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))

  mind <- ifelse(alt == "two.sided" || alt == "less", min(d, 0.1), 1)
  maxd <- ifelse(alt == "less", 1,
    ifelse(alt == "two.sided" && d < 1, max(2, ((1 / d) * 1.2)), max(2, (d * 1.2)))
  )

  dd <- seq(mind, maxd, len = 20)


  z.pwr <- try(sapply(dd, function(delta) {
    .pwr2Var2NTest(
      n = nn, n.ratio = n_ratio,
      rho = delta,
      sig.level = alpha,
      alternative = alt
    )$power
  }))
  if (inherits(z.pwr, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
    return()
  }


  z.delta <- try(sapply(nn, function(N) {
    .pwr2Var2NTest(
      n = N, n.ratio = n_ratio,
      sig.level = alpha,
      power = power,
      alternative = alt
    )$rho
  }))
  if (inherits(z.delta, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
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
.populateContourTextTest2Var <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["contourText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerContour"))
    html$position <- 6
    jaspResults[["contourText"]] <- html
  }

  str <- gettext(
    "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical variance ratio and the sample sizes in the design. As we increase the sample sizes, less extreme variance ratios become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only more extreme variance ratios, smaller sample sizes are needed. The point shows the power of the specified design and variance ratio."
  )

  html[["text"]] <- str
}
.preparePowerCurveESTest2Var <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerCurveES"]]
  if (is.null(image)) {
    image <- createJaspPlot(
      title = gettext("Power Curve by variance ratio"),
      width = 400,
      height = 350
    )
    image$dependOn(c(
      "test",
      "effectSize",
      "varianceRatio",
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
    jaspResults[["powerCurveES"]] <- image
  }

  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  ps <- .pwrPlotDefaultSettings

  maxd <- try(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, power = max(0.99, power), sig.level = alpha, alternative = alt)$rho)
  if (inherits(maxd, "try-error")) {
    mind <- ifelse(d < 1, d, 1)
    maxd <- ifelse(d < 1, 1, d)
  } else {
    if (alt == "two.sided") {
      mind <- min(as.numeric(maxd[2]), d)
      maxd <- max(as.numeric(maxd[1]), d)
    } else if (alt == "less") {
      mind <- min(as.numeric(maxd), d)
      maxd <- 1
    } else {
      mind <- 1
      maxd <- max(as.numeric(maxd), d)
    }
  }


  dd <- seq(mind, maxd, len = ps$curve.n)

  y <- try(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = dd, sig.level = alpha, alternative = alt)$power)
  if (inherits(y, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the power curve. Try to enter less extreme values for the parameters"))
    return()
  }
  cols <- ps$pal(ps$pow.n.levels)
  yrect <- seq(0, 1, 1 / ps$pow.n.levels)

  state <- list(cols = cols, dd = dd, y = y, yrect = yrect, n1 = n1, n2 = n2, alpha = alpha, delta = d, pow = power)
  image$plotObject <- .plotPowerCurveES(options, state = state, ggtheme = .pwrPlotTheme())
}
.populatePowerCurveESTextTest2Var <- function(jaspResults, options, r, lst) {
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
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  if (alt == "two.sided" && d < 1) {
    d <- ifelse(calc == "effectSize",
      d,
      ifelse(calc == "sampleSize",
        try(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, sig.level = alpha, power = power, alternative = alt)$rho[2]),
        lst$es
      )
    )
  } else {
    d <- ifelse(calc == "effectSize",
      d,
      ifelse(calc == "sampleSize",
        try(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, sig.level = alpha, power = power, alternative = alt)$rho[1]),
        lst$es
      )
    )
  }
  if (inherits(d, "try-error")) {
    return()
  } else {
    d <- round(as.numeric(d), 3)
  }


  n_text <- ifelse(n1 == n2,
    gettextf("sample sizes of %1$s in each group", n1),
    gettextf("group sample sizes of %1$s and %2$s, respectively", n1, n2)
  )

  if (d > 1) {
    alt_text <- "<i>\u03C1\u003E</i>"
  } else {
    alt_text <- "<i>\u03C1\u003C</i>"
  }

  if (calc == "power") {
    pwr_string <- gettextf("have power of at least %1$s", round(power, 3))
  } else {
    pwr_string <- gettextf("only be sufficiently sensitive (power > %1$s)", round(power, 3))
  }

  if (alt == "two.sided" && d < 1) {
    d50 <- try(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, sig.level = alpha, power = .5, alternative = alt)$rho[2])
    if (inherits(d50, "try-error")) {
      return()
    }
    interval <- gettextf("1 > %1$s > %2$s", "\u03C1", round(d50, 3))
  } else {
    d50 <- try(.pwr2Var2NTest(n = n1, n.ratio = n_ratio, sig.level = alpha, power = .5, alternative = alt)$rho[1])
    if (inherits(d50, "try-error")) {
      return()
    }
    if (alt == "less" || alt == "two.sided" && d < 1) {
      interval <- gettextf("1 > %1$s > %2$s", "\u03C1", round(as.numeric(d50), 3))
    } else {
      interval <- gettextf("1 < %1$s < %2$s", "\u03C1", round(as.numeric(d50), 3))
    }
  }

  str <- gettextf(
    "<p>The power curve above shows how the sensitivity of the test and design is larger for more extreme variance ratios. If we obtained %1$s our test and design would %2$s to variance ratios of %3$s%4$s. <p>We would be more than likely to miss (power less than 50%%) variance ratios of %5$s.",
    n_text, pwr_string, alt_text, d, interval
  )

  html[["text"]] <- str
}
.preparePowerCurveNTest2Var <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerCurveN"]]
  if (is.null(image)) {
    image <- createJaspPlot(title = "Power Curve by N", width = 400, height = 350)
    image$dependOn(c(
      "test",
      "effectSize",
      "varianceRatio",
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
    jaspResults[["powerCurveN"]] <- image
  }

  calc <- options$calculation
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  n_ratio <- lst$n_ratio
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  ps <- .pwrPlotDefaultSettings

  maxn <- try(ceiling(.pwr2Var2NTest(
    n.ratio = n_ratio,
    power = max(0.99999, power),
    rho = d,
    sig.level = alpha,
    alternative = alt
  )$n))
  if (inherits(maxn, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the 'Power Curve by N' plot. Try to enter less extreme values for the parameters"))
    return()
  } else if (n1 >= maxn && n1 >= ps$maxn) {
    maxn <- ceiling(n1 * ps$max.scale)
  }


  minn <- 2
  try <- try(.pwr2Var2NTest(n = minn, n.ratio = n_ratio, sig.level = alpha, power = power, alternative = alt))
  while (inherits(try, "try-error")) {
    minn <- minn + 1
    try <- try(.pwr2Var2NTest(n = minn, n.ratio = n_ratio, sig.level = alpha, power = power, alternative = alt))
  }

  nn <- seq(minn, maxn)

  y <- try(.pwr2Var2NTest(
    n = nn,
    n.ratio = n_ratio,
    rho = d, sig.level = alpha, alternative = alt
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
.preparePowerDistTest2Var <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerDist"]]
  if (is.null(image)) {
    image <- createJaspPlot(title = "Power Demonstration", width = 400, height = 300)
    image$dependOn(c(
      "test",
      "effectSize",
      "varianceRatio",
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
    jaspResults[["powerDist"]] <- image
  }

  calc <- options$calculation
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  df1 <- n1 - 1
  df2 <- n2 - 1
  if (d < 1) {
    d_copy <- d
    d <- 1 / d
    df1_1 <- df1
    df1 <- df2
    df2 <- df1_1
  } else {
    d_copy <- d
  }
  if (alt == "two.sided") {
    ncp.body <- quote({
      pf(q = qf(p = alpha / 2, df1 = df1, df2 = df2, lower.tail = FALSE), df1 = df1, df2 = df2, ncp = ncp, lower.tail = FALSE) +
        pf(q = qf(p = alpha / 2, df1 = df1, df2 = df2, lower.tail = TRUE), df1 = df1, df2 = df2, ncp = ncp, lower.tail = TRUE)
    })
  } else {
    ncp.body <- quote({
      pf(q = qf(p = alpha, df1 = df1, df2 = df2, lower.tail = FALSE), df1 = df1, df2 = df2, ncp = ncp, lower.tail = FALSE)
    })
  }

  ncp <- try(uniroot(function(ncp) eval(ncp.body) - power, c(1e-10, 1e+10))$root)
  if (inherits(ncp, "try-error")) {
    image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the curves for the Power Demonstration. Try to enter less extreme values for the parameters"))
    return()
  }

  if (alt == "two.sided") {
    crit_upper <- qf(p = 1 - alpha / 2, df1 = df1, df2 = df2)
    crit_lower <- qf(p = alpha / 2, df1 = df1, df2 = df2)
  } else {
    crit <- qf(p = 1 - alpha, df1 = df1, df2 = df2)
  }


  xlims <- c(0, qf(.999^(d), df1 = df1, df2 = df2, ncp = ncp))

  xx <- seq(xlims[1], xlims[2], len = 1000)
  yy.null <- df(xx, df1 = df1, df2 = df2)
  yy.alt <- df(xx, df1 = df1, df2 = df2, ncp)

  y.max <- min(max(yy.null), max(yy.alt) * 5)

  curves <- data.frame(
    x = rep(xx, 2),
    ymin = rep(0, length(xx) * 2),
    ymax = c(yy.null, yy.alt),
    group = rep(c("Null", "Alt"), each = length(xx))
  )

  if (alt == "two.sided") {
    rect <- data.frame(
      x1 = crit_lower, x2 = crit_upper,
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

  state <- list(curves = curves, rect = rect, lims = lims, d = d_copy)
  image$plotObject <- .plotPowerDist(options, state = state, ggtheme = .pwrPlotTheme())
}
.populatePowerCurveNTextTest2Var <- function(jaspResults, options, r, lst) {
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
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  d <- round(d, 3)
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  n_text <- ifelse(n1 == n2,
    gettextf("sample sizes of at least %1$s in each group", n1),
    gettextf("group sample sizes of at least %1$s and %2$s, respectively", n1, n2)
  )

  if (alt == "two.sided") {
    alt_text <- "<i>\u03C1\u2260</i>1"
  } else {
    if (d > 1) {
      alt_text <- "<i>\u03C1\u003E</i>1"
    } else {
      alt_text <- "<i>\u03C1\u003C</i>1"
    }
  }

  str <- gettextf(
    "<p>The power curve above shows how the sensitivity of the test and design is larger for larger sample sizes. In order for our test and design to have sufficient sensitivity (power > %1$s) to detect that %2$s when the variance ratio is %3$s or more extreme, we would need %4$s.",
    round(power, 3), alt_text, d, n_text
  )

  html[["text"]] <- str
}
.populateDistTextTest2Var <- function(jaspResults, options, r, lst) {
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
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  d <- round(d, 2)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  if (d < 1) {
    n1_1 <- n1
    n1 <- n2
    n2 <- n1_1
  }
  n_text <- ifelse(n1 == n2,
    gettextf("a sample size of %1$s in each group", n1),
    gettextf("group sample sizes of %1$s and %2$s, respectively", n1, n2)
  )
  if (d < 1) {
    ratio_text <- gettext("<i>Note.</i> When the variance ratio is less than 1, the inverse of the variance ratio is displayed. Therefore, the first and second degrees of freedom of both F-distributions switched places.\n\n")
  }
  if (d > 1) {
    ratio_text <- gettext("")
  }

  es_text <- ifelse(d < 1, gettextf("1/%1$s", "\u03C1"), gettextf("%1$s", "\u03C1"))
  d <- ifelse(d < 1, round(1 / d, 2), d)
  if (alt == "two.sided") {
    tail_text <- gettext("two-sided")
    null_text <- gettextf("<i>%1$s=</i>1,", es_text)
    alt_text <- gettextf("<i>%1$s</i><i>\u2260</i>", es_text)
    crit_text <- gettext("criteria")
  } else if (alt == "less") {
    tail_text <- gettext("one-sided")
    null_text <- gettextf("<i>%1$s\u2264</i>1,", es_text)
    alt_text <- gettextf("<i>%1$s\u003E</i", es_text)
    crit_text <- gettext("criterion")
  } else {
    tail_text <- gettext("one-sided")
    null_text <- gettextf("<i>%1$s\u2264</i>1,", es_text)
    alt_text <- gettextf("<i>%1$s\u003E</i", es_text)
    crit_text <- gettext("criterion")
  }


  str <- paste(
    "<p>",
    gettextf("%1$sThe figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> variance ratio when <i>%2$s=</i>1 (left), and when <i>%3$s=</i>%4$s (right).", ratio_text, es_text, es_text, d),
    gettextf("Both assume %1$s.", n_text),
    "</p><p>",
    gettextf("The vertical dashed lines show the %1$s we would set for a %2$s test with <i>\u03B1=</i>%3$s.", crit_text, tail_text, alpha),
    gettextf("When the observed variance ratio is far enough away from 1 to be more extreme than the %1$s we say we 'reject' the null hypothesis.", crit_text),
    gettextf("If the null hypothesis were true and %1$s the evidence would lead us to wrongly reject the null hypothesis at most %2$s%% of the time.", null_text, 100 * alpha),
    "</p><p>",
    gettextf("On the other hand, if <i>%1$s%2$s</i>%3$s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%4$s</i>1 &mdash; at least %5$s%% of the time.", es_text, "\u2265", d, alt_text, 100 * round(power, 3)),
    gettextf("The design's power for detecting effects of %1$s%2$s%3$s is thus %4$s.", es_text, "\u2265", d, round(power, 3)),
    "</p>"
  )


  html[["text"]] <- str
}

#### Generate synthetic dataset ----
.generateDatasetTest2Var <- function(jaspResults, options, r, lst) {
  datasetContainer <- jaspResults[["datasetcont"]]
  if (is.null(datasetContainer)) {
    # Create Container if it doesn't exist yet
    datasetContainer <- createJaspContainer(title = gettext("Synthetic Dataset"))
    datasetContainer$dependOn(c(
      "test",
      "effectSize",
      "varianceRatio",
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

  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "sampleSize", r$n1, lst$n1)
  n2 <- ifelse(calc == "sampleSize", r$n2, lst$n2)
  if (calc == "effectSize") d <- ifelse(lst$alt == "two.sided" && options$effectDirection == "less", r$es[2], r$es[1]) else d <- lst$es
  d <- round(d, 2)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt
  power <- ifelse(calc == "power",
    r$power,
    ifelse(calc == "sampleSize",
      .pwr2Var2NTest(n = n1, n.ratio = n_ratio, rho = d, sig.level = alpha, alternative = alt)$power,
      lst$pow
    )
  )

  sd_2 <- options[["secondGroupSd"]]
  var_2 <- sd_2^2
  var_1 <- var_2 * d
  sd_1 <- sqrt(var_1)



  mean_1 <- options[["firstGroupMean"]]
  mean_2 <- options[["secondGroupMean"]]

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
    gettext("Variance ratio (\u03C1)"),
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
