# Originally based on https://github.com/richarddmorey/jpower

# New Utility functions

# Transform a contour matrix (z) and vectors for it's columns and rows (x, y)
# into a dataframe with 3 columns (x, y, z) to be used for ggplot.
transformContourMatrix <- function(x, y, z) {
  return(data.frame(
    # Ncol and nrow are different here then one would expect (!)
    x = rep(x, ncol(z)),
    y = rep(y, each = nrow(z)),
    z = as.numeric(z)
  ))
}

# Old utility functions below

pwr_plot_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16, colour = "#333333"),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.margin = ggplot2::margin(15, 15, 15, 15),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, 0, 0, 0)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 5, 0, 0)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10, 0, 0, 0)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 10, 0, 0)),
      plot.title = ggplot2::element_text(margin = ggplot2::margin(0, 0, 15, 0)),
      legend.background = ggplot2::element_rect("transparent"),
      legend.key = ggplot2::element_rect(fill = "#E8E8E8")
    )
}

# Workaround for failure of pwr::pwr.t2n.test
# with large effect sizes - optimization here is
# better
pwr.t2n.test <- function(n1 = NULL, n2 = NULL, d = NULL, sig.level = .05, power = NULL, alternative = c("two.sided", "less", "greater")) {
  if (!is.null(power)) {
    if (power >= 1) stop("Power cannot be 1.")
  }
  if (is.null(d)) {
    if (power < sig.level) stop("power < alpha")
    x <- try(pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
    if (inherits(x, "try-error")) {
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
      ret <- structure(list(
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

pwr.t2n.ratio <- function(n_ratio = 1, d, sig.level, power, alternative) {
  if (power >= 1) {
    return(Inf)
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

pwr.p.test <- function (p0 = NULL, p = NULL, n = NULL, sig.level = 0.05, power = NULL,
                        alternative = c("two.sided", "less", "greater"))
{
  if (sum(sapply(list(p, n, power, sig.level), is.null)) !=
      1)
    stop("exactly one of p, n, power, and sig.level must be NULL")
  if (!is.null(n) && n < 2)
    stop("number of observations in the first group must be at least 2")
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)))
    stop("sig.level must be between 0 and 1")
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1)))
    stop("power must be between 0 and 1")
  if (!is.null(p) && any(0 > p | p > 1))
    stop("proportion must be between 0 and 1")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)

  if (tside == 2) {
    p.body <- quote({
      1 - pnorm(((p0 - p) + qnorm(sig.level) * sqrt(p0 * (1 - p0) / n)) / sqrt(p * (1 - p) / n)) +
        pnorm(((p0 - p) - qnorm(sig.level) * sqrt(p0 * (1 - p0) / n)) / sqrt(p * (1 - p) / n))
    })
  }
  if (tside == 3) {
    p.body <- quote({
      1 - pnorm(((p0 - p) + qnorm(sig.level) * sqrt(p0 * (1 - p0) / n)) / sqrt(p * (1 - p) / n))
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pnorm(((p0 - p) - qnorm(sig.level) * sqrt(p0 * (1 - p0) / n)) / sqrt(p * (1 - p) / n))
    })
  }
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(p)) {
    p <- uniroot(function(p) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
  }
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  METHOD <- "proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(p = p, n = n, sig.level = sig.level, power = power,
                 alternative = alternative, method = METHOD), class = "power.htest")
}

pwr.2p2n.test <- function (p1 = NULL, p2 = NULL, n = NULL, n.ratio = 1, sig.level = 0.05, power = NULL,
          alternative = c("two.sided", "less", "greater"))
{
  if (sum(sapply(list(p1, p2, n, n.ratio, power, sig.level), is.null)) !=
      1)
    stop("exactly one of p1, p2, n, n.ratio, power, and sig.level must be NULL")

  if (!is.null(n) && n < 2)
    stop("number of observations in the first group must be at least 2")
  if (!is.null(n.ratio) && n.ratio <= 0)
    stop("ratio between group sizes must be positive")
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)))
    stop("sig.level must be between 0 and 1")
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1)))
    stop("power must be between 0 and 1")
  if ((!is.null(p1) || !is.null(p2)) && (any(0 > p1 | p1 > 1) || any(0 > p2 | p2 > 1)))
    stop("proportions must be between 0 and 1")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)

  if (tside == 3) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = FALSE) - 2 * (asin(sqrt(p1))-asin(sqrt(p2))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = FALSE)
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = TRUE) - 2 * (asin(sqrt(p1))-asin(sqrt(p2))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = TRUE)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      pnorm(qnorm(sig.level/2, lower = FALSE) - 2 * abs(asin(sqrt(p1))-asin(sqrt(p2))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = FALSE) + pnorm(qnorm(sig.level/2, lower = TRUE) - 2 * abs(asin(sqrt(p1))-asin(sqrt(p2))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = TRUE)
    })
  }
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(p2)) {

    if (tside == 2) {
      p2 <- uniroot(function(p2) eval(p.body) - power, c(p1 + 1e-10, 1-1e-10))$root
      p2 <- c(p2, uniroot(function(p2) eval(p.body) - power, c(1e-10, p1-1e-10))$root)
    }


    if (tside == 1)
      p2 <- uniroot(function(p2) eval(p.body) - power, c(1e-10, 1-1e-10))$root

    if (tside == 3)
      p2 <- uniroot(function(p2) eval(p.body) - power, c(1e-10, 1-1e-10))$root
  }
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
  else if (is.null(n.ratio))
    n.ratio <- uniroot(function(n.ratio) eval(p.body) - power, c(1e-10, 1e+09))$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) -
                           power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  NOTE <- "different sample sizes"
  METHOD <- "difference of proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(p1 = p1, p2 = p2, n = n, n.ratio = n.ratio, sig.level = sig.level,
                 power = power, alternative = alternative, method = METHOD,
                 note = NOTE), class = "power.htest")
}

pwr.poisson.test <- function (n1 = NULL, n2 = NULL, power = NULL, sig.level = 0.05,
                              lambda1 = NULL, lambda2 = NULL, t1 = 1, t2 = 1, RR0 = 1,
                              equal.sample = TRUE, alternative = c("two.sided", "one.sided"))
{
  if (equal.sample == TRUE) {
    if (sum(sapply(list(n1, lambda1, lambda2, power, sig.level),
                   is.null)) != 1)
      stop("exactly one of n1, lambda1, lambda2, power, and sig.level must be NULL")
    if (!is.null(sig.level) && !is.numeric(sig.level) ||
        any(0 > sig.level | sig.level > 1))
      stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 >
                                                     power | power > 1))
      stop(sQuote("power"), " must be numeric in [0, 1]")
    alternative <- match.arg(alternative)
    test.side <- switch(alternative, one.sided = 1, two.sided = 2)
    p.body <- quote({pnorm((lambda2-lambda1) / sqrt(lambda1/(n1*t1) + lambda2/(n1*t2)) - qnorm(sig.level/test.side, lower.tail = FALSE))})

    if (is.null(power))
      power <- eval(p.body)
    else if (is.null(n1))
      n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
    else if (is.null(lambda1))
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(0, 1e+07))$root
    else if (is.null(lambda2))
      lambda2 <- uniroot(function(lambda2) eval(p.body) - power, c(0, 1e+07))$root

    if (lambda1*n1 < 30 || lambda2*n1 < 30) {
      p.body <- quote({pnorm((sqrt(lambda2)-sqrt(lambda1)) / (1/2 * sqrt(1/(n1 * t1) + 1/(n1 * t2))) - qnorm(sig.level/test.side, lower.tail = FALSE))})

      if (is.null(power))
        power <- eval(p.body)
      else if (is.null(n1))
        n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
      else if (is.null(n1))
        n2 <- uniroot(function(n2) eval(p.body) - power, c(2, 1e+07))$root
      else if (is.null(lambda1))
        lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(0, 1e+07))$root
      else if (is.null(lambda2))
        lambda2 <- uniroot(function(lambda2) eval(p.body) - power, c(0, 1e+07))$root
    }

    METHOD <- "Two-sample Poisson Ratio Tests (Equal Sizes)"
    NOTE <- "N is number in *each* group"
    return(structure(list(N = n1, lambda1 = lambda1, lambda2 = lambda2,
                          sig.level = sig.level, power = power, alternative = alternative,
                          method = METHOD, note = NOTE), class = "power.htest"))
  }
  else {
    if (sum(sapply(list(n1, n2, lambda1, lambda2, power,
                        sig.level), is.null)) != 1)
      stop("exactly one of n1, n2, lambda1, lambda2, power, and sig.level must be NULL")
    if (!is.null(sig.level) && !is.numeric(sig.level) ||
        any(0 > sig.level | sig.level > 1))
      stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 >
                                                     power | power > 1))
      stop(sQuote("power"), " must be numeric in [0, 1]")
    alternative <- match.arg(alternative)
    test.side <- switch(alternative, one.sided = 1, two.sided = 2)
    p.body <- quote({pnorm((lambda2-lambda1) / sqrt(lambda1/(n1*t1) + lambda2/(n2*t2)) - qnorm(sig.level/test.side, lower.tail = FALSE))})

    if (is.null(power))
      power <- eval(p.body)
    else if (is.null(n1))
      n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
    else if (is.null(n1))
      n2 <- uniroot(function(n2) eval(p.body) - power, c(2, 1e+07))$root
    else if (is.null(lambda1))
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(0, 1e+07))$root
    else if (is.null(lambda2))
      lambda2 <- uniroot(function(lambda2) eval(p.body) - power, c(0, 1e+07))$root

    if (lambda1*n1 < 30 || lambda2*n2 < 30) {
      p.body <- quote({pnorm((sqrt(lambda2)-sqrt(lambda1)) / (1/2 * sqrt(1/(n1 * t1) + 1/(n2 * t2))) - qnorm(sig.level/test.side, lower.tail = FALSE))})

      if (is.null(power))
        power <- eval(p.body)
      else if (is.null(n1))
        n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
      else if (is.null(n1))
        n2 <- uniroot(function(n2) eval(p.body) - power, c(2, 1e+07))$root
      else if (is.null(lambda1))
        lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(0, 1e+07))$root
      else if (is.null(lambda2))
        lambda2 <- uniroot(function(lambda2) eval(p.body) - power, c(0, 1e+07))$root
    }

    METHOD <- "Two-sample Poisson Ratio Tests (Different Sizes)"
    return(structure(list(n1 = n1, n2 = n2, lambda1 = lambda1,
                          lambda2 = lambda2, sig.level = sig.level, power = power,
                          alternative = alternative, method = METHOD), class = "power.htest"))
  }
}

pwr.var.test <- function (rho = NULL, n = NULL, sig.level = 0.05, power = NULL,
                             alternative = c("two.sided", "less", "greater"))
{
  if (sum(sapply(list(rho, n, power, sig.level), is.null)) !=
      1)
    stop("exactly one of rho, n, power, and sig.level must be NULL")

  if (!is.null(n) && n < 2)
    stop("number of observations in the first group must be at least 2")
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)))
    stop("sig.level must be between 0 and 1")
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1)))
    stop("power must be between 0 and 1")
  if (!is.null(rho) && 0 > rho)
    stop("rho must be positive")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)

  if (tside == 3) {
    p.body <- quote({
      1 - pchisq(qchisq(sig.level, df = n - 1, lower.tail = FALSE) / rho, df = n-1)
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pchisq(qchisq(sig.level, df = n - 1, lower.tail = TRUE) / rho, df = n-1)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      1 - pchisq(qchisq(sig.level/2, df = n - 1, lower.tail = FALSE) / rho, df = n-1) +
        pchisq(qchisq(sig.level/2, df = n - 1, lower.tail = TRUE) / rho, df = n-1)
    })
  }
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(rho)) {

    if (tside == 2) {
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root
      rho <- c(rho, uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 10))$root)
    }


    if (tside == 1)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root

    if (tside == 3)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 10))$root
  }
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) -
                           power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  NOTE <- "one sample"
  METHOD <- "variance ratio power calculation"
  structure(list(rho = rho, n = n, sig.level = sig.level,
                 power = power, alternative = alternative, method = METHOD,
                 note = NOTE), class = "power.htest")
}

pwr.2var2n.test <- function (rho = NULL, n = NULL, n.ratio = 1, sig.level = 0.05, power = NULL,
                                alternative = c("two.sided", "less", "greater"))
{
  if (sum(sapply(list(rho, n, n.ratio, power, sig.level), is.null)) !=
      1)
    stop("exactly one of rho, n, n.ratio, power, and sig.level must be NULL")

  if (!is.null(n) && n < 2)
    stop("number of observations in the first group must be at least 2")
  if (!is.null(n.ratio) && n.ratio <= 0)
    stop("ratio between group sizes must be positive")
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)))
    stop("sig.level must be between 0 and 1")
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1)))
    stop("power must be between 0 and 1")
  if (!is.null(rho) && 0 > rho)
    stop("rho must be positive")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)

  if (tside == 3) {
    p.body <- quote({
      1 - pnorm(qt(sig.level, df = n + (n * n.ratio) - 2, lower.tail = FALSE) / ((rho^2 + (n / (n * n.ratio))) / (n * rho^2 / n * n.ratio + 1))^0.5 - ((rho - 1) / (sqrt(pi/2 - 1) * sqrt((rho^2/n + 1/ n * n.ratio)))))
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pnorm(qt(sig.level, df = n + (n * n.ratio) - 2, lower.tail = TRUE) / ((rho^2 + (n / (n * n.ratio))) / (n * rho^2 / n * n.ratio + 1))^0.5 - ((rho - 1) / (sqrt(pi/2 - 1) * sqrt((rho^2/n + 1/ n * n.ratio)))))
    })
  }
  if (tside == 2) {
    p.body <- quote({
      1 - pnorm(qt(sig.level/2, df = n + (n * n.ratio) - 2, lower.tail = FALSE) / ((rho^2 + (n / (n * n.ratio))) / (n * rho^2 / n * n.ratio + 1))^0.5 - ((rho - 1) / (sqrt(pi/2 - 1) * sqrt((rho^2/n + 1/ n * n.ratio))))) +
        pnorm(qt(sig.level/2, df = n + (n * n.ratio) - 2, lower.tail = TRUE) / ((rho^2 + (n / (n * n.ratio))) / (n * rho^2 / n * n.ratio + 1))^0.5 - ((rho - 1) / (sqrt(pi/2 - 1) * sqrt((rho^2/n + 1/ n * n.ratio)))))
    })
  }
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(rho)) {

    if (tside == 2) {
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root
      rho <- c(rho, uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 10))$root)
    }


    if (tside == 1)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root

    if (tside == 3)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 10))$root
  }
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
  else if (is.null(n.ratio))
    n.ratio <- uniroot(function(n.ratio) eval(p.body) - power, c(1e-10, 1e+09))$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) -
                           power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  NOTE <- "different sample sizes"
  METHOD <- "variance ratio power calculation"
  structure(list(rho = rho, n = n, n.ratio = n.ratio, sig.level = sig.level,
                 power = power, alternative = alternative, method = METHOD,
                 note = NOTE), class = "power.htest")
}
# Convenience function to draw segment lines
.segment <- function (...) {
  return(ggplot2::annotate(
    geom = "segment",
    size = 1,
    linetype = "dashed",
    ...
  ))
}

ttestPlotSettings <- list(
  lens = 20,
  x.axis.n = 8,
  pow.n.levels = 10,
  curve.n = 128,
  maxn = 100,
  max.scale = 1.5,
  mind = 0,
  maxd = 2,
  background.alpha = .7,
  # stripe.cols = pal(pow.n.levels)[c(1,pow.n.levels)]
  stripe.cols = c("black", "black")
)
ttestPlotSettings$pal <- function(...) {
  viridis::viridis(..., alpha = ttestPlotSettings$background.alpha)
}
