# ==== Major Helper Functions ====

# Prepare the statistics in a common format
.prepareStats <- function(options) {
  ## Get options from interface
  n <- options$sampleSize
  n_ratio <- options$sampleSizeRatio
  pow <- options$power
  alt <- options$alternative
  p0 <- options$baselineProportion
  p1 <- options$comparisonProportion
  es <- options$effectSize
  alpha <- options$alpha

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
      alt
    ),
    alpha = alpha
  )
  return(stats)
}

# Helper to populate the initial text that is shown
.populateIntro <- function(jaspResults, options) {
  if (!options$text) {
    return()
  }

  calc <- options$calculation

  html <- jaspResults[["intro"]]
  if (is.null(html)) {
    html <- createJaspHtml(title = "Introduction")
    html$dependOn(c("test", "text"))
    html$position <- 1
    jaspResults[["intro"]] <- html
  }

  str <- gettextf(
    "The purpose of a %s is to evaluate the sensitivity of a design and test. ",
    paste0("<i>", gettext("power analysis"), "</i>")
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
  test_sentence_end <- paste0(
    " ", gettext("when using"), " ", "<i>", test_names[[options$test]], "</i>."
  )

  mid_sentence <- switch(calc,
    sampleSize = gettext(
      "You have chosen to calculate the minimum sample size needed to have an experiment sensitive enough to consistently detect the specified hypothetical effect size"
    ),
    effectSize = gettext(
      "You have chosen to calculate the minimum hypothetical effect size for which the chosen design will have the specified sensitivity"
    ),
    power = gettext(
      "You have chosen to calculate the sensitivity of the chosen design for detecting the specified effect size"
    )
  )

  str <- paste0(
    str,
    mid_sentence,
    test_sentence_end
  )

  html[["text"]] <- str
}

# ==== Small Helper Functions ====

.checkResults <- function(results) {
  if (jaspBase::isTryError(results)) {
    .quitAnalysis(gettext("Unable to compute the power results. Try to enter less extreme values for the input parameters."))
  }
}

# Transform a contour matrix (z) and vectors for it's columns and rows (x, y)
# into a dataframe with 3 columns (x, y, z) to be used for ggplot.
.transformContourMatrix <- function(x, y, z) {
  return(data.frame(
    # Ncol and nrow are different here then one would expect (!)
    x = rep(x, ncol(z)),
    y = rep(y, each = nrow(z)),
    z = as.numeric(z)
  ))
}

# Based upon: https://github.com/richarddmorey/jpower/blob/3825ec1c368669c3cb1168e292b465e1d5141a2f/jpower/R/utils.R#L19
# Original comment from jpower:
# Workaround for failure of pwr::pwr.t2n.test
# with large effect sizes - optimization here is
# better
.pwrT2NTest <- function(n1 = NULL, n2 = NULL, d = NULL, sig.level = .05, power = NULL, alternative = c("two.sided", "less", "greater")) {
  if (!is.null(power)) {
    if (power >= 1) stop("Power cannot be 1.")
  }
  if (is.null(d)) {
    if (power < sig.level) stop("power < alpha")
    x <- try(pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
    if (jaspBase::isTryError(x)) {
      effN <- n1 * n2 / (n1 + n2)
      df <- n1 + n2 - 2
      if (length(alternative) > 1) alternative == alternative[1]
      if (alternative == "two.sided") {
        crit <- qt(1 - sig.level / 2, df)
        es <- uniroot(function(x) {
          d <- exp(log(x) - log1p(-x))
          ncp <- d * sqrt(effN)
          pow <- pt(-crit, df, ncp) + 1 - pt(crit, df, ncp)
          log(pow) - log(power)
        }, interval = c(0, 1))$root
        d <- exp(log(es) - log1p(-es))
      } else if (alternative %in% c("greater", "less")) {
        crit <- qt(1 - sig.level, df)
        es <- uniroot(function(x) {
          d <- exp(log(x) - log1p(-x))
          ncp <- d * sqrt(effN)
          pow <- 1 - pt(crit, df, ncp)
          log(pow) - log(power)
        }, interval = c(0, 1))$root
        d <- exp(log(es) - log1p(-es))
        d <- ifelse(alternative == "less", -d, d)
      } else {
        stop("Invalid alternative")
      }
      METHOD <- c("t test power calculation")
      ret <- structure(
        list(
          n1 = n1, n2 = n2, d = d, sig.level = sig.level,
          power = power, alternative = alternative, method = METHOD
        ),
        class = "power.htest"
      )
      return(ret)
    } else {
      return(x)
    }
  } else {
    return(pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = sig.level, power = power, alternative = alternative))
  }
}

.pwrT2NRatio <- function(n_ratio = 1, d, sig.level, power, alternative) {
  if (power >= 1) {
    stop(gettext("Power cannot be 1"))
  }
  fn <- Vectorize(function(n1) {
    effN <- n1 * n_ratio / (1 + n_ratio)
    df <- n1 * (1 + n_ratio) - 2
    ncp <- sqrt(effN) * d
    if (alternative == "two.sided") {
      if (d == 0) {
        stop(gettext("Effect size can't be 0 with a two-sided alternative hypothesis."))
      }
      critt <- qt(sig.level / 2, df)
      pow <- pt(critt, df, ncp) + 1 - pt(-critt, df, ncp)
    } else if (alternative == "less") {
      if (d >= 0) {
        stop(gettext("Effect size has to be lower than 0 with an alternative of lesser."))
      }
      critt <- qt(sig.level, df)
      pow <- pt(critt, df, ncp)
    } else if (alternative == "greater") {
      if (d <= 0) {
        stop(gettext("Effect size has to be greater than 0 with an alternative of greater."))
      }
      critt <- qt(1 - sig.level, df)
      pow <- 1 - pt(critt, df, ncp)
    } else {
      stop(gettext("Invalid alternative."))
    }
    return(log(pow) - log(power))
  }, "n1")
  rt <- uniroot(fn, c(ceiling(3 / (1 + n_ratio)), 1e+09))$root
  return(ceiling(rt))
}

.pwrPTest <- function(p0 = NULL, p = NULL, n = NULL, sig.level = 0.05, power = NULL,
                      alternative = c("two.sided", "less", "greater")) {
  if (is.null(p0)) {
    stop("p0 is a required argument")
  }
  if (sum(sapply(list(p, n, power, sig.level), is.null)) != 1) {
    stop("exactly one of p, n, power, and sig.level must be NULL")
  }
  if (any(!is.null(n) & n < 2)) {
    stop("number of observations in the first group must be at least 2")
  }
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1))) {
    stop("sig.level must be between 0 and 1")
  }
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1))) {
    stop("power must be between 0 and 1")
  }
  if (!is.null(p) && any(0 > p | p > 1)) {
    stop("proportion must be between 0 and 1")
  }
  alternative <- match.arg(alternative)
  tside <- switch(alternative,
    less = 1,
    two.sided = 2,
    greater = 3
  )

  if (tside == 2) {
    p.body <- quote({
      1 - pnorm(2 * (asin(sqrt(p)) - asin(sqrt(p0))) * sqrt(n) + qnorm(sig.level / 2, lower.tail = FALSE)) +
        pnorm(2 * (asin(sqrt(p)) - asin(sqrt(p0))) * sqrt(n) - qnorm(sig.level / 2, lower.tail = FALSE))
    })
  }
  if (tside == 3) {
    p.body <- quote({
      pnorm(2 * (asin(sqrt(p)) - asin(sqrt(p0))) * sqrt(n) - qnorm(sig.level, lower.tail = FALSE))
    })
  }
  if (tside == 1) {
    p.body <- quote({
      1 - pnorm(2 * (asin(sqrt(p)) - asin(sqrt(p0))) * sqrt(n) + qnorm(sig.level, lower.tail = FALSE))
    })
  }
  if (is.null(power)) {
    power <- eval(p.body)
  } else if (is.null(p)) {
    if (tside == 3) {
      p <- uniroot(function(p) eval(p.body) - power, c(p0, 1 - 1e-10))$root
    }
    if (tside == 1) {
      p <- uniroot(function(p) eval(p.body) - power, c(1e-10, p0))$root
    }
    if (tside == 2) {
      p_1 <- try(uniroot(function(p) eval(p.body) - power, c(p0, 1 - 1e-10))$root)
      p_2 <- try(uniroot(function(p) eval(p.body) - power, c(1e-10, p0))$root)
      if (jaspBase::isTryError(p_1) && jaspBase::isTryError(p_2)) {
        stop("no solution found")
      } else {
        if (jaspBase::isTryError(p_1)) {
          p <- c(NA, p_2)
        } else if (jaspBase::isTryError(p_2)) {
          p <- c(p_1, NA)
        } else {
          p <- c(p_1, p_2)
        }
      }
    }
  } else if (is.null(n)) {
    n <- uniroot(function(n) eval(p.body) - power, c(1e-10, 1e+09))$root
  } else if (is.null(sig.level)) {
    sig.level <- uniroot(function(sig.level) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
  } else {
    stop("internal error")
  }
  METHOD <- "proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(
    p = p, n = n, sig.level = sig.level, power = power,
    alternative = alternative, method = METHOD
  ), class = "power.htest")
}

.pwr2P2NTest <- function(p0 = NULL, p1 = NULL, n = NULL, n.ratio = 1, sig.level = 0.05, power = NULL,
                         alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(p1, n, n.ratio, power, sig.level), is.null)) !=
    1) {
    stop("exactly one of p1, n, n.ratio, power, and sig.level must be NULL")
  }

  if (any(!is.null(n) & n < 2)) {
    stop("number of observations in the first group must be at least 2")
  }
  if (!is.null(n.ratio) && n.ratio <= 0) {
    stop("ratio between group sizes must be positive")
  }
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1))) {
    stop("sig.level must be between 0 and 1")
  }
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1))) {
    stop("power must be between 0 and 1")
  }
  if ((!is.null(p0) || !is.null(p1)) && (any(0 > p0 | p0 > 1) || any(0 > p1 | p1 > 1))) {
    stop("proportions must be between 0 and 1")
  }
  alternative <- match.arg(alternative)
  tside <- switch(alternative,
    less = 1,
    two.sided = 2,
    greater = 3
  )

  if (tside == 3) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = FALSE) - 2 * (asin(sqrt(p1)) - asin(sqrt(p0))) * sqrt((n * (n * n.ratio)) / (n + (n * n.ratio))), lower = FALSE)
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = TRUE) - 2 * (asin(sqrt(p1)) - asin(sqrt(p0))) * sqrt((n * (n * n.ratio)) / (n + (n * n.ratio))), lower = TRUE)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      pnorm(qnorm(sig.level / 2, lower = FALSE) - 2 * abs(asin(sqrt(p1)) - asin(sqrt(p0))) * sqrt((n * (n * n.ratio)) / (n + (n * n.ratio))), lower = FALSE) +
        pnorm(qnorm(sig.level / 2, lower = TRUE) - 2 * abs(asin(sqrt(p1)) - asin(sqrt(p0))) * sqrt((n * (n * n.ratio)) / (n + (n * n.ratio))), lower = TRUE)
    })
  }
  if (is.null(power)) {
    power <- eval(p.body)
  } else if (is.null(p1)) {
    if (tside == 2) {
      p_1 <- try(uniroot(function(p1) eval(p.body) - power, c(p0, 1 - 1e-10))$root)
      p_2 <- try(uniroot(function(p1) eval(p.body) - power, c(1e-10, p0))$root)
      if (jaspBase::isTryError(p_1) && jaspBase::isTryError(p_2)) {
        stop("no solution found")
      } else {
        if (jaspBase::isTryError(p_1)) {
          p1 <- c(NA, p_2)
        } else if (jaspBase::isTryError(p_2)) {
          p1 <- c(p_1, NA)
        } else {
          p1 <- c(p_1, p_2)
        }
      }
    } else {
      p1 <- uniroot(function(p1) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
    }
  } else if (is.null(n)) {
    if (n.ratio >= 1) {
      n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
    }
    if (n.ratio < 1) {
      n <- uniroot(function(n) eval(p.body) - power, c((2 / n.ratio), 1e+09))$root
    }
  } else if (is.null(n.ratio)) {
    n.ratio <- uniroot(function(n.ratio) eval(p.body) - power, c(1e-10, 1e+09))$root
  } else if (is.null(sig.level)) {
    sig.level <- uniroot(function(sig.level) {
      eval(p.body) -
        power
    }, c(1e-10, 1 - 1e-10))$root
  } else {
    stop("internal error")
  }
  NOTE <- "different sample sizes"
  METHOD <- "difference of proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(
    p0 = p0, p1 = p1, n = n, n.ratio = n.ratio, sig.level = sig.level,
    power = power, alternative = alternative, method = METHOD,
    note = NOTE
  ), class = "power.htest")
}

.pwrVarTest <- function(rho = NULL, n = NULL, sig.level = 0.05, power = NULL,
                        alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(rho, n, power, sig.level), is.null)) !=
    1) {
    stop("exactly one of rho, n, power, and sig.level must be NULL")
  }

  if (any(!is.null(n) & n < 2)) {
    stop("number of observations in the first group must be at least 2")
  }
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1))) {
    stop("sig.level must be between 0 and 1")
  }
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1))) {
    stop("power must be between 0 and 1")
  }
  if (any(!is.null(rho) & 0 > rho)) {
    stop("rho must be positive")
  }
  alternative <- match.arg(alternative)
  tside <- switch(alternative,
    less = 1,
    two.sided = 2,
    greater = 3
  )

  if (tside == 3) {
    p.body <- quote({
      1 - pchisq(qchisq(sig.level, df = n - 1, lower.tail = FALSE) / rho, df = n - 1)
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pchisq(qchisq(sig.level, df = n - 1, lower.tail = TRUE) / rho, df = n - 1)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      1 - pchisq(qchisq(sig.level / 2, df = n - 1, lower.tail = FALSE) / rho, df = n - 1) +
        pchisq(qchisq(sig.level / 2, df = n - 1, lower.tail = TRUE) / rho, df = n - 1)
    })
  }
  if (is.null(power)) {
    power <- eval(p.body)
  } else if (is.null(rho)) {
    if (tside == 2) {
      rho1 <- try(uniroot(function(rho) eval(p.body) - power, c(1 + 1e-10, 1e+09))$root)
      rho2 <- try(uniroot(function(rho) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root)
      if (jaspBase::isTryError(rho1) && jaspBase::isTryError(rho2)) {
        stop("no solution found")
      } else {
        if (jaspBase::isTryError(rho1)) {
          rho <- c(NA, rho2)
        } else if (jaspBase::isTryError(rho2)) {
          rho <- c(rho1, NA)
        } else {
          rho <- c(rho1, rho2)
        }
      }
    }

    if (tside == 1) {
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
    }

    if (tside == 3) {
      rho <- uniroot(function(rho) eval(p.body) - power, c(1 + 1e-10, 1e+09))$root
    }
  } else if (is.null(n)) {
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
  } else if (is.null(sig.level)) {
    sig.level <- uniroot(function(sig.level) {
      eval(p.body) -
        power
    }, c(1e-10, 1 - 1e-10))$root
  } else {
    stop("internal error")
  }
  NOTE <- "one sample"
  METHOD <- "variance ratio power calculation"
  structure(list(
    rho = rho, n = n, sig.level = sig.level,
    power = power, alternative = alternative, method = METHOD,
    note = NOTE
  ), class = "power.htest")
}

.pwr2Var2NTest <- function(rho = NULL, n = NULL, n.ratio = 1, sig.level = 0.05, power = NULL,
                           alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(rho, n, n.ratio, power, sig.level), is.null)) !=
    1) {
    stop("exactly one of rho, n, n.ratio, power, and sig.level must be NULL")
  }

  if (any(!is.null(n) & n < 2)) {
    stop("number of observations in the first group must be at least 2")
  }
  if (!is.null(n.ratio) && n.ratio <= 0) {
    stop("ratio between group sizes must be positive")
  }
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1))) {
    stop("sig.level must be between 0 and 1")
  }
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1))) {
    stop("power must be between 0 and 1")
  }
  if (any(!is.null(rho) & 0 > rho)) {
    stop("rho must be positive")
  }
  alternative <- match.arg(alternative)
  tside <- switch(alternative,
    less = 1,
    two.sided = 2,
    greater = 3
  )

  if (tside == 3) {
    p.body <- quote({
      1 - pf(qf(1 - sig.level, df1 = n - 1, df2 = (n * n.ratio) - 1) / rho, df1 = n - 1, df2 = (n * n.ratio) - 1)
    })
  }

  if (tside == 1) {
    p.body <- quote({
      pf(qf(sig.level, df1 = n - 1, df2 = (n * n.ratio) - 1) / rho, df1 = n - 1, df2 = (n * n.ratio) - 1)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      1 - pf(qf(1 - (sig.level / 2), df1 = n - 1, df2 = (n * n.ratio) - 1) / rho, df1 = n - 1, df2 = (n * n.ratio) - 1) +
        pf(qf(sig.level / 2, df1 = n - 1, df2 = (n * n.ratio) - 1) / rho, df1 = n - 1, df2 = (n * n.ratio) - 1)
    })
  }
  if (is.null(power)) {
    power <- eval(p.body)
  } else if (is.null(rho)) {
    if (tside == 2) {
      rho1 <- try(uniroot(function(rho) eval(p.body) - power, c(1 + 1e-10, 1e+09))$root)
      rho2 <- try(uniroot(function(rho) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root)
      if (jaspBase::isTryError(rho1) && jaspBase::isTryError(rho2)) {
        stop("no solution found")
      } else {
        if (jaspBase::isTryError(rho1)) {
          rho <- c(NA, rho2)
        } else if (jaspBase::isTryError(rho2)) {
          rho <- c(rho1, NA)
        } else {
          rho <- c(rho1, rho2)
        }
      }
    }
    if (tside == 1) {
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
    }

    if (tside == 3) {
      rho <- uniroot(function(rho) eval(p.body) - power, c(1 + 1e-10, 1e+09))$root
    }
  } else if (is.null(n)) {
    if (n.ratio >= 1) {
      n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
    }
    if (n.ratio < 1) {
      n <- uniroot(function(n) eval(p.body) - power, c((2 / n.ratio), 1e+09))$root
    }
  } else if (is.null(n.ratio)) {
    n.ratio <- uniroot(function(n.ratio) eval(p.body) - power, c(1e-10, 1e+09))$root
  } else if (is.null(sig.level)) {
    sig.level <- uniroot(function(sig.level) {
      eval(p.body) -
        power
    }, c(1e-10, 1 - 1e-10))$root
  } else {
    stop("internal error")
  }
  NOTE <- "different sample sizes"
  METHOD <- "variance ratio power calculation"
  structure(list(
    rho = rho, n = n, n.ratio = n.ratio, sig.level = sig.level,
    power = power, alternative = alternative, method = METHOD,
    note = NOTE
  ), class = "power.htest")
}

.pwrPoisTest <- function(n = NULL, power = NULL, sig.level = 0.05,
                         lambda1 = NULL, lambda0 = NULL, t = 1, alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(n, lambda1, lambda0, power, sig.level), is.null)) != 1) {
    stop("exactly one of n, lambda1, lambda0, power, and sig.level must be NULL")
  }
  if (!is.null(sig.level) && !is.numeric(sig.level) ||
    any(0 > sig.level | sig.level > 1)) {
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  }
  if (!is.null(power) && !is.numeric(power) || any(0 >
    power | power > 1)) {
    stop(sQuote("power"), " must be numeric in [0, 1]")
  }
  alternative <- match.arg(alternative)
  test.side <- switch(alternative,
    less = 1,
    two.sided = 2,
    greater = 3
  )

  if (test.side == 3) {
    p.body <- quote({
      1 - pnorm((qnorm(sig.level, lower.tail = FALSE) * sqrt(lambda0 / (n * t)) + lambda0 - lambda1) / sqrt(lambda1 / (n * t)))
    })
  }
  if (test.side == 1) {
    p.body <- quote({
      1 - pnorm((qnorm(sig.level, lower.tail = FALSE) * sqrt(lambda0 / (n * t)) + lambda1 - lambda0) / sqrt(lambda1 / (n * t)))
    })
  }
  if (test.side == 2) {
    p.body <- quote({
      (1 - pnorm((qnorm(sig.level / 2, lower.tail = FALSE) * sqrt(lambda0 / (n * t)) + lambda0 - lambda1) / sqrt(lambda1 / (n * t)))) +
        (1 - pnorm((qnorm(sig.level / 2, lower.tail = FALSE) * sqrt(lambda0 / (n * t)) + lambda1 - lambda0) / sqrt(lambda1 / (n * t))))
    })
  }

  if (is.null(power)) {
    power <- eval(p.body)
  } else if (is.null(n)) {
    n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+07))$root
  } else if (is.null(lambda1)) {
    if (test.side == 2) {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(lambda0 + 1e-10, 1e+07))$root
    } else {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(1e-10, 1e+07))$root
    }
  }

  METHOD <- "One-sample Poisson Rate Test"
  return(structure(list(
    n = n, t = t, lambda1 = lambda1,
    lambda0 = lambda0, sig.level = sig.level, power = power,
    alternative = alternative, method = METHOD
  ), class = "power.htest"))
}

.pwr2Pois2NTest <- function(n1 = NULL, n.ratio = 1, power = NULL, sig.level = 0.05,
                            lambda1 = NULL, lambda2 = NULL, t1 = 1, t2 = 1, alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(n1, lambda1, lambda2, power, sig.level), is.null)) != 1) {
    stop("exactly one of n1, lambda1, lambda2, power, and sig.level must be NULL")
  }
  if (!is.null(sig.level) && !is.numeric(sig.level) ||
    any(0 > sig.level | sig.level > 1)) {
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  }
  if (!is.null(power) && !is.numeric(power) || any(0 >
    power | power > 1)) {
    stop(sQuote("power"), " must be numeric in [0, 1]")
  }
  alternative <- match.arg(alternative)
  test.side <- switch(alternative,
    less = 1,
    two.sided = 2,
    greater = 3
  )

  if (test.side == 1) {
    p.body <- quote({
      pnorm((lambda2 - lambda1) / sqrt(lambda1 / (n1 * t1) + lambda2 / (n1 * n.ratio * t2)) - qnorm(sig.level, lower.tail = FALSE))
    })
  }
  if (test.side == 3) {
    p.body <- quote({
      1 - pnorm((lambda2 - lambda1) / sqrt(lambda1 / (n1 * t1) + lambda2 / (n1 * n.ratio * t2)) + qnorm(sig.level, lower.tail = FALSE))
    })
  }
  if (test.side == 2) {
    p.body <- quote({
      1 - pnorm((lambda2 - lambda1) / sqrt(lambda1 / (n1 * t1) + lambda2 / (n1 * n.ratio * t2)) + qnorm(sig.level / 2, lower.tail = FALSE)) +
        pnorm((lambda2 - lambda1) / sqrt(lambda1 / (n1 * t1) + lambda2 / (n1 * n.ratio * t2)) - qnorm(sig.level / 2, lower.tail = FALSE))
    })
  }

  if (is.null(power)) {
    power <- eval(p.body)
  } else if (is.null(n1)) {
    n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
  } else if (is.null(lambda1)) {
    if (test.side == 2) {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(lambda2 + 1e-10, 1e+07))$root
    } else {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(1e-10, 1e+07))$root
    }
  }


  if (lambda1 * n1 < 30 || lambda2 * n1 * n.ratio < 30) {
    if (test.side == 1) {
      p.body <- quote({
        pnorm((sqrt(lambda2) - sqrt(lambda1)) / (1 / 2 * sqrt(1 / (n1 * t1) + 1 / (n1 * n.ratio * t2))) - qnorm(sig.level, lower.tail = FALSE))
      })
    }
    if (test.side == 3) {
      p.body <- quote({
        1 - pnorm((sqrt(lambda2) - sqrt(lambda1)) / (1 / 2 * sqrt(1 / (n1 * t1) + 1 / (n1 * n.ratio * t2))) + qnorm(sig.level, lower.tail = FALSE))
      })
    }
    if (test.side == 2) {
      p.body <- quote({
        1 - pnorm((sqrt(lambda2) - sqrt(lambda1)) / (1 / 2 * sqrt(1 / (n1 * t1) + 1 / (n1 * n.ratio * t2))) + qnorm(sig.level / 2, lower.tail = FALSE)) +
          pnorm((sqrt(lambda2) - sqrt(lambda1)) / (1 / 2 * sqrt(1 / (n1 * t1) + 1 / (n1 * n.ratio * t2))) - qnorm(sig.level / 2, lower.tail = FALSE))
      })
    }

    if (is.null(power)) {
      power <- eval(p.body)
    } else if (is.null(n1)) {
      n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
    } else if (is.null(lambda1)) {
      if (test.side == 2) {
        lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(lambda2 + 1e-10, 1e+07))$root
      } else {
        lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(1e-10, 1e+07))$root
      }
    }
  }

  METHOD <- "Two-sample Poisson Ratio Tests (Different Sizes)"
  return(structure(list(
    n1 = n1, n.ratio = n.ratio, t1 = t1, t2 = t2, lambda1 = lambda1,
    lambda2 = lambda2, sig.level = sig.level, power = power,
    alternative = alternative, method = METHOD
  ), class = "power.htest"))
}

.errorMessageUnsolvable <- function (errorObject) {
  return(paste(
    gettext("The specified design leads to (an) unsolvable equation(s) while constructing the power curve. Try to enter less extreme values for the parameters"),
    gettext("Error Message:"),
    errorObject,
    sep = "\n"
  ))
}
