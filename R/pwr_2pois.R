# Originally based on https://github.com/richarddmorey/jpower

.runTest2Pois <- function(jaspResults, options) {
  stats <- .prepareStats(options)

  ## Compute results
  results <- try(.computeTest2Pois(jaspResults, options, stats))
  if (inherits(results, "try-error")) {
    .quitAnalysis(gettext("Unable to compute the power results. Try to enter less extreme values for the input parameters."))
  }

  .initPowerTabTest2Pois(jaspResults, options, results, stats)

  if (options$text) {
    .initPowerESTabTest2Pois(jaspResults, options, results, stats)
  }

  ## Populate tables and plots
  if (options$text) {
    .populateIntro(jaspResults, options)
  }

  if (options$powerContour) {
    .preparePowerContourTest2Pois(jaspResults, options, results, stats)
    if (options$text) {
      .populateContourTextTest2Pois(jaspResults, options, results, stats)
    }
  }
  if (options$powerByEffectSize) {
    .preparePowerCurveESTest2Pois(jaspResults, options, results, stats)
    if (options$text) {
      .populatePowerCurveESTextTest2Pois(jaspResults, options, results, stats)
    }
  }
  if (options$powerBySampleSize) {
    .preparePowerCurveNTest2Pois(jaspResults, options, results, stats)
    if (options$text) {
      .populatePowerCurveNTextTest2Pois(jaspResults, options, results, stats)
    }
  }
  if (options$powerDemonstration) {
    .preparePowerDistTest2Pois(jaspResults, options, results, stats)
    if (options$text) {
      .populateDistTextTest2Pois(jaspResults, options, results, stats)
    }
  }
  if (options$saveDataset && options$savePath != "") {
    .generateDatasetTest2Pois(jaspResults, options, results, stats)
  }
}

.computeTest2Pois <- function(jaspResults, options, stats) {
  ## Compute numbers for table
  pow.n <- try(ceiling(.pwr2Pois2NTest(n_ratio = stats$n_ratio, lambda2 = stats$p0, lambda1 = stats$p1, t1 = stats$t1, t2 = stats$t2, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)$n1), silent = TRUE)
  pow.lambda1 <- try(.pwr2Pois2NTest(n1 = stats$n1, n.ratio = stats$n_ratio, lambda2 = stats$p0, t1 = stats$t1, t2 = stats$t2, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$lambda1, silent = TRUE)
  pow.pow <- try(.pwr2Pois2NTest(n1 = stats$n1, n.ratio = stats$n_ratio, lambda2 = stats$p0, lambda1 = stats$p1, t1 = stats$t1, t2 = stats$t2, sig.level = stats$alpha, alternative = stats$alt)$power, silent = TRUE)


  d50 <- .pwr2Pois2NTest(
    n1 = stats$n1, n.ratio = stats$n_ratio,
    lambda2 = stats$p0,
    t1 = stats$t1, t2 = stats$t2,
    sig.level = stats$alpha,
    power = .5, alternative = stats$alt
  )$lambda1

  return(list(n1 = pow.n, n2 = ceiling(pow.n * stats$n_ratio), lambda1 = pow.lambda1, power = pow.pow, d50 = d50))
}

#### Init table ----
.initPowerTabTest2Pois <- function(jaspResults, options, results, stats) {
  table <- jaspResults[["powertab"]]

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
    jaspResults[["powertab"]] <- table
  } else {
    return()
  }

  calc <- options$calc

  if (calc == "n") {
    order <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  } else if (calc == "es") {
    order <- c(6, 7, 1, 2, 3, 4, 5, 8, 9)
  } else if (calc == "power") {
    order <- c(8, 1, 2, 3, 4, 5, 6, 7, 9)
  } else {
    order <- c(9, 1, 2, 3, 4, 5, 6, 7, 8)
  }

  colNames <- c("n1", "n2", "t1", "t2", "p0", "p1", "es", "power", "alpha")
  colLabels <- c(
    "N\u2081",
    "N\u2082",
    "t\u2081",
    "t\u2082",
    "\u03BB\u2082",
    "\u03BB\u2081",
    "\u03BB\u2081/\u03BB\u2082",
    gettext("Power"),
    "\u03B1",
  )
  colType <- c("integer", "integer", "number", "number", "number", "number", "number", "number", "number")

  for (i in seq_along(order)) {
    table$addColumnInfo(colNames[order[i]],
      title = colLabels[order[i]],
      overtitle = if (((calc == "n" || calc == "es") && i > 2) || ((calc != "n" && calc != "es") && i > 1)) "User Defined" else NULL,
      type = colType[order[i]]
    )
  }

  options$es <- p1 / p0

  row <- list()
  for (i in 2:length(order)) {
    row[[colNames[order[i]]]] <- options[[colNames[order[i]]]]
  }

  if (options$calc != "n") {
    row[["n1"]] <- options[["n"]]
    row[["n2"]] <- ceiling(options[["n"]] * options[["n_ratio"]])
  }

  table$addRows(rowNames = 1, row)

  .populatePowerTabTest2Pois(jaspResults, options, results, stats)
}
.initPowerESTabTest2Pois <- function(jaspResults, options, results, stats) {
  table <- jaspResults[["powerEStab"]]

  if (is.null(table)) {
    # Create table if it doesn't exist yet
    table <- createJaspTable(title = gettext("Power by Poisson Rate Ratio"))
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
    jaspResults[["powerEStab"]] <- table
  } else {
    return()
  }

  table$addColumnInfo(
    name = "es",
    title = gettext("True Poisson rate ratio"),
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

  .populatePowerESTabTest2Pois(jaspResults, options, results, stats)
}
.populatePowerESTabTest2Pois <- function(jaspResults, options, r, lst) {
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
  calc <- options$calc
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  n_text <- ifelse(n1 == n2,
    gettextf("a sample size of %s in each group ", n1),
    gettextf("group sample sizes of %s and %s respectively, ", n1, n2)
  )
  tail_text <- ifelse(alt == "two.sided",
    "two-sided",
    "one-sided"
  )

  print("===> pre-calc")
  probs <- c(.5, .8, .95)
  probs_es <- sapply(probs, function(p) {
    .pwr2Pois2NTest(
      n1 = n1, n.ratio = n_ratio,
      lambda2 = lambda2,
      t1 = t1, t2 = t2,
      sig.level = alpha, power = p,
      alternative = alt
    )$lambda1 / lambda2
  })
  print("===> post-calc")
  print(probs_es)

  if (calc == "n") {
    str <- gettextf(
      "We would need %s to reliably (with probability greater than %s) detect a Poisson rate ratio of <i>%s%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
      n_text, power, "\u03BB\u2081/\u03BB\u2082", "\u2265", d, tail_text, alpha
    )
  } else if (calc == "es") {
    str <- gettextf(
      "A design with %s will reliably (with probability greater than %s) detect Poisson rate ratios of <i>%s%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
      n_text, power, "\u03BB\u2081/\u03BB\u2082", "\u2265", round(d, 3), tail_text, alpha
    )
  } else if (calc == "power") {
    str <- gettextf(
      "A design with %s can detect Poisson rate ratios of %s<i>%s%s</i>%s with a probability of at least %s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
      n_text, d, "\u03BB\u2081/\u03BB\u2082", "\u2265", round(power, 3), tail_text, alpha
    )
  }

  hypo_text <- ifelse(alt == "less",
    "<i>\u03BB\u2081/\u03BB\u2082</i><<i>1</i>",
    ifelse(alt == "greater",
      "<i>\u03BB\u2081/\u03BB\u2082</i>><i>1</i>",
      "<i>\u03BB\u2081/\u03BB\u2082</i>\u2260<i>1</i>"
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

  esText <- c(
    gettextf("0 < %s %s  %s", "\u03BB\u2081/\u03BB\u2082", "\u2264", format(round(probs_es[1], 3), nsmall = 3)),
    gettextf("%s < %s %s %s", format(round(probs_es[1], 3), nsmall = 3), "\u03BB\u2081/\u03BB\u2082", "\u2264", format(round(probs_es[2], 3), nsmall = 3)),
    gettextf("%s < %s %s %s", format(round(probs_es[2], 3), nsmall = 3), "\u03BB\u2081/\u03BB\u2082", "\u2264", format(round(probs_es[3], 3), nsmall = 3)),
    gettextf("%s %s %s", "\u03BB\u2081/\u03BB\u2082", "\u2265", format(round(probs_es[3], 3), nsmall = 3))
  )

  cols <- list("es" = esText)
  table$addColumns(cols)
}
#### Populate table ----
.populatePowerTabTest2Pois <- function(jaspResults, options, results, stats) {
  table <- jaspResults[["powertab"]]

  calc <- options$calc

  # Note: It is unclear what this value actualyl corresponds to
  # row[["d50"]] <- results[["d50"]]

  if (calc == "n") {
    table$addColumns(list(n1 = results[["n1"]]))
    table$addColumns(list(n2 = results[["n2"]]))
  } else if (calc == "es") {
    table$addColumns(list(p1 = results[["lambda1"]]))
    table$addColumns(list(es = results[["lambda1"]] / options$p0))
  } else {
    row <- list()
    row[[calc]] <- results[[calc]]
    table$addColumns(row)
  }
}

#### Plot functions ----
.preparePowerContourTest2Pois <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerContour"]]
  if (is.null(image)) {
    image <- createJaspPlot(
      title = gettext("Power Contour"),
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
      "powerContour"
    ))
    image$position <- 5
    jaspResults[["powerContour"]] <- image
  }

  ps <- .pwrPlotDefaultSettings
  ps$mind <- 0.5

  calc <- options$calc

  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt


  maxn <- ceiling(.pwr2Pois2NTest(
    n_ratio = n_ratio,
    lambda2 = lambda2, lambda1 = lambda1,
    t1 = t1, t2 = t2,
    power = max(0.9, power),
    sig.level = alpha,
    alternative = alt
  )$n1)

  if (n1 > maxn && n1 >= ps$maxn) {
    maxn <- ceiling(n1 * ps$max.scale)
  } else if (maxn < ps$maxn) {
    maxn <- ps$maxn
  }


  minn <- ceiling(.pwr2Pois2NTest(
    n_ratio = n_ratio,
    lambda2 = lambda2, lambda1 = lambda1,
    t1 = t1, t2 = t2,
    power = min(0.1, power),
    sig.level = alpha,
    alternative = alt
  )$n1)

  nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))
  dd <- seq(ps$mind, ps$maxd, len = ps$lens)


  z.pwr <- sapply(dd, function(delta) {
    .pwr2Pois2NTest(
      n1 = nn, n.ratio = n_ratio,
      lambda2 = lambda2, lambda1 = lambda2 * delta,
      t1 = t1, t2 = t2,
      sig.level = alpha,
      alternative = alt
    )$power
  })

  z.delta <- sapply(nn, function(N) {
    .pwr2Pois2NTest(
      n1 = N, n.ratio = n_ratio,
      lambda2 = lambda2,
      t1 = t1, t2 = t2,
      sig.level = alpha,
      power = power,
      alternative = alt
    )$lambda1 / lambda2
  })

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
.populateContourTextTest2Pois <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["contourText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerContour"))
    html$position <- 6
    jaspResults[["contourText"]] <- html
  }

  ## Get options from interface
  calc <- options$calc
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  str <- gettextf(
    "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical Poisson rate ratio and the sample sizes in the design. As we increase the sample sizes, Poisson rate ratios closer to one become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only larger effects, smaller sample sizes are needed. The solid black curve on the contour plot shows sample size/Poisson rate ratios combinations with a power of %s. The point shows the specified  design and Poisson rate ratio.",
    round(power, 3)
  )

  html[["text"]] <- str
}
.preparePowerCurveESTest2Pois <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerCurveES"]]
  if (is.null(image)) {
    image <- createJaspPlot(
      title = gettext("Power Curve by Effect Size"),
      width = 400,
      height = 350
    )
    image$dependOn(c(
      "test",
      "es",
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
    jaspResults[["powerCurveES"]] <- image
  }

  ps <- .pwrPlotDefaultSettings
  ps$mind <- 0.5

  calc <- options$calc
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  dd <- seq(ps$mind, ps$maxd, len = ps$curve.n)

  y <- .pwr2Pois2NTest(n1 = n1, n.ratio = n_ratio, lambda2 = lambda2, lambda1 = lambda2 * dd, t1 = t1, t2 = t2, sig.level = alpha, alternative = alt)$power
  cols <- ps$pal(ps$pow.n.levels)
  yrect <- seq(0, 1, 1 / ps$pow.n.levels)

  state <- list(cols = cols, dd = dd, y = y, yrect = yrect, n1 = n1, n2 = n2, alpha = alpha, delta = d, pow = power)
  image$plotObject <- .plotPowerCurveES(options, state = state, ggtheme = .pwrPlotTheme())
}
.populatePowerCurveESTextTest2Pois <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["curveESText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerCurveES"))
    html$position <- 8
    jaspResults[["curveESText"]] <- html
  }

  ## Get options from interface
  calc <- options$calc
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  d <- round(d, 3)
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  n_text <- ifelse(n1 == n2,
    gettextf("sample sizes of %s in each group", n1),
    gettextf("group sample sizes of %s and %s, respectively", n1, n2)
  )

  if (alt == "two.sided") {
    tail_text <- "two-sided"
    null_text <- "<i>\u03BB\u2081/\u03BB\u2082\u2264</i>0,"
    alt_text <- "|<i>\u03BB\u2081/\u03BB\u2082</i>|<i>\u003E</i>"
    crit_text <- "criteria"
  } else {
    tail_text <- "one-sided"
    null_text <- "<i>\u03BB\u2081/\u03BB\u2082=</i>0,"
    alt_text <- "<i>\u03BB\u2081/\u03BB\u2082\u003E</i>"
    crit_text <- "criterion"
  }

  if (calc == "power") {
    pwr_string <- gettextf("have power of at least %s", round(power, 3))
  } else {
    pwr_string <- gettextf("only be sufficiently sensitive (power >%s)", round(power, 3))
  }

  d50 <- .pwr2Pois2NTest(
    n1 = n1, n.ratio = n_ratio,
    lambda2 = lambda2,
    t1 = t1, t2 = t2,
    sig.level = alpha,
    power = 0.5,
    alternative = alt
  )$lambda1 / lambda2

  str <- gettextf(
    "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. If we obtained %s our test and design would %s to effect sizes of %s%s. <p>We would be more than likely to miss (power less than 50%%) effect sizes less than <i>%s=</i>%s.",
    n_text, pwr_string, alt_text, d, "\u03BB\u2081/\u03BB\u2082", round(d50, 3)
  )

  html[["text"]] <- str
}
.preparePowerCurveNTest2Pois <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerCurveN"]]
  if (is.null(image)) {
    image <- createJaspPlot(title = "Power Curve by N", width = 400, height = 350)
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
    jaspResults[["powerCurveN"]] <- image
  }

  calc <- options$calc

  ps <- .pwrPlotDefaultSettings

  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  n_ratio <- lst$n_ratio
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  maxn <- ceiling(.pwr2Pois2NTest(
    n_ratio = n_ratio,
    lambda2 = lambda2, lambda1 = lambda1,
    t1 = t1, t2 = t2,
    power = max(0.9, power),
    sig.level = alpha,
    alternative = alt
  )$n1)

  if (n1 > maxn && n1 >= ps$maxn) {
    maxn <- ceiling(n1 * ps$max.scale)
  } else if (maxn < ps$maxn) {
    maxn <- ps$maxn
  }


  minn <- ceiling(.pwr2Pois2NTest(
    n_ratio = n_ratio,
    lambda2 = lambda2, lambda1 = lambda1,
    t1 = t1, t2 = t2,
    power = min(0.1, power),
    sig.level = alpha,
    alternative = alt
  )$n1)

  nn <- seq(minn, maxn)

  y <- .pwr2Pois2NTest(
    n1 = nn, n.ratio = n_ratio,
    lambda2 = lambda2, lambda1 = lambda1,
    t1 = t1, t2 = t2,
    sig.level = alpha,
    alternative = alt
  )$power

  cols <- ps$pal(ps$pow.n.levels)
  yrect <- seq(0, 1, 1 / ps$pow.n.levels)

  lims <- data.frame(
    xlim = c(minn, maxn),
    ylim = c(0, 1)
  )

  state <- list(n = n1, cols = cols, nn = nn, y = y, yrect = yrect, lims = lims, delta = d, alpha = alpha, n_ratio = n_ratio, pow = power)
  image$plotObject <- .plotPowerCurveN(options, state = state, ggtheme = .pwrPlotTheme())
}
.preparePowerDistTest2Pois <- function(jaspResults, options, r, lst) {
  image <- jaspResults[["powerDist"]]
  if (is.null(image)) {
    image <- createJaspPlot(title = "Power Demonstration", width = 400, height = 300)
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
    jaspResults[["powerDist"]] <- image
  }

  calc <- options$calc

  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt


  ncp <- log(d)
  sd <- sqrt(1 / (lambda1 * n1 * t1) + 1 / (lambda2 * n2 * t2))

  if (alt == "two.sided") {
    crit <- qnorm(p = 1 - alpha / 2, sd = sd)
  } else {
    crit <- qnorm(p = 1 - alpha, sd = sd)
  }

  if (d > 1) {
    xlims <- c(qnorm(.001, sd = sd), qnorm(.999, sd = sd, mean = ncp))
  } else {
    xlims <- c(qnorm(.001, sd = sd, mean = ncp), qnorm(.999, sd = sd))
  }

  y.max <- dnorm(0, sd = sd)

  xx <- seq(xlims[1], xlims[2], len = 100)
  yy.null <- dnorm(xx, sd = sd)
  yy.alt <- dnorm(xx, sd = sd, mean = ncp)

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
.populatePowerCurveNTextTest2Pois <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["curveNText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerCurveN"))
    html$position <- 10
    jaspResults[["curveNText"]] <- html
  }

  ## Get options from interface
  calc <- options$calc
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  d <- round(d, 3)
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  n_text <- ifelse(n1 == n2,
    gettextf("sample sizes of at least %s in each group", n1),
    gettextf("group sample sizes of at least %s and %s, respectively", n1, n2)
  )

  if (alt == "two.sided") {
    tail_text <- "two-sided"
    null_text <- "<i>\u03BB\u2081/\u03BB\u2082\u2264</i>0,"
    alt_text <- "|<i>\u03BB\u2081/\u03BB\u2082</i>|<i>\u003E</i>0,"
    crit_text <- "criteria"
  } else {
    tail_text <- "one-sided"
    null_text <- "<i>\u03BB\u2081/\u03BB\u2082=</i>0,"
    alt_text <- "<i>\u03BB\u2081/\u03BB\u2082\u2260</i>0,"
    crit_text <- "criterion"
  }

  str <- gettextf(
    "<p>The power curve above shows how the sensitivity of the test and design is larger for larger effect sizes. In order for our test and design to have sufficient sensitivity (power > %s) to detect that %s when the effect size is %s or larger, we would need %s.",
    round(power, 3), alt_text, d, n_text
  )

  html[["text"]] <- str
}
.populateDistTextTest2Pois <- function(jaspResults, options, r, lst) {
  html <- jaspResults[["distText"]]
  if (is.null(html)) {
    html <- createJaspHtml()
    html$dependOn(c("test", "text", "powerDist"))
    html$position <- 12
    jaspResults[["distText"]] <- html
  }

  ## Get options from interface
  calc <- options$calc
  n_ratio <- lst$n_ratio
  n1 <- ifelse(calc == "n", r$n1, lst$n1)
  n2 <- ifelse(calc == "n", r$n2, lst$n2)
  t1 <- lst$t1
  t2 <- lst$t2
  lambda2 <- lst$p0
  lambda1 <- ifelse(calc == "es", r$lambda1, lst$p1)
  d <- lambda1 / lambda2
  d <- round(d, 2)
  power <- ifelse(calc == "power", r$power, lst$pow)
  alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
  alt <- lst$alt

  n_text <- ifelse(n1 == n2,
    gettextf("a sample size of %s in each group", n1),
    gettextf("group sample sizes of %s and %s, respectively", n1, n2)
  )

  if (alt == "two.sided") {
    tail_text <- gettext("two-sided")
    null_text <- "<i>\u03BB\u2081/\u03BB\u2082=</i>0,"
    alt_text <- "|<i>\u03BB\u2081/\u03BB\u2082</i>|<i>\u2265</i>"
    crit_text <- gettext("criteria")
  } else {
    tail_text <- gettext("one-sided")
    null_text <- "<i>\u03BB\u2081/\u03BB\u2082\u2264</i>0,"
    alt_text <- "<i>\u03BB\u2081/\u03BB\u2082\u2265</i"
    crit_text <- gettext("criterion")
  }

  str <- paste(
    "<p>",
    gettextf("The figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> effect size when <i>%s=</i>0 (left), and when <i>%s=</i>%s (right).", "\u03BB\u2081/\u03BB\u2082", "\u03BB\u2081/\u03BB\u2082", d),
    gettextf("Both assume %s.", n_text),
    "</p><p>",
    gettextf("The vertical dashed lines show the %s we would set for a %s test with <i>α=</i>%s.", crit_text, tail_text, alpha),
    gettextf("When the observed effect size is far enough away from 0 to be more extreme than the %s we say we 'reject' the null hypothesis.", crit_text),
    gettextf("If the null hypothesis were true and %s the evidence would lead us to wrongly reject the null hypothesis at most %s%% of the time.", null_text, 100 * alpha),
    "</p><p>",
    gettextf("On the other hand, if <i>%s%s</i>%s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%s%s</i>0 &mdash; at least %s%% of the time.", "\u03BB\u2081/\u03BB\u2082", "\u2265", d, "\u03BB\u2081/\u03BB\u2082", "\u2265", 100 * round(power, 3)),
    gettextf("The design's power for detecting effects of %s%s is thus %s.", alt_text, d, round(power, 3)),
    "</p>"
  )


  html[["text"]] <- str
}
