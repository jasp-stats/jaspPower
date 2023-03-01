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

pwr.p.test <- function (p0 = NULL, p = NULL, n = NULL, sig.level = 0.05, power = NULL,
                        alternative = c("two.sided", "less", "greater"))
{
  if(is.null(p0))
    stop("p0 is a required argument")
  if (sum(sapply(list(p, n, power, sig.level), is.null)) != 1)
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
      1 - pnorm(2 * (asin(sqrt(p)) - asin(sqrt(p0))) * sqrt(n) + qnorm(sig.level/2, lower.tail = FALSE)) +
        pnorm(2 * (asin(sqrt(p)) - asin(sqrt(p0))) * sqrt(n) - qnorm(sig.level/2, lower.tail = FALSE))
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
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(p)) {
    if(tside == 3)
      p <- uniroot(function(p) eval(p.body) - power, c(p0, 1 - 1e-10))$root
    if (tside == 1)
      p <- uniroot(function(p) eval(p.body) - power, c(1e-10, p0))$root
    if (tside == 2) {
      p_1 <- try(uniroot(function(p) eval(p.body) - power, c(p0, 1 - 1e-10))$root)
      p_2 <- try(uniroot(function(p) eval(p.body) - power, c(1e-10, p0))$root)
      if(inherits(p_1, "try-error") && inherits(p_2, "try-error")) {
        stop("no solution found")
      } else {
        if (inherits(p_1, "try-error")) {
          p <- c(NA, p_2)
        } else if (inherits(p_2, "try-error")) {
          p <- c(p_1, NA)
        } else {
          p <- c(p_1, p_2)
        }
      }
    }
  }
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(1e-10, 1e+09))$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  METHOD <- "proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(p = p, n = n, sig.level = sig.level, power = power,
                 alternative = alternative, method = METHOD), class = "power.htest")
}

pwr.2p2n.test <- function (p0 = NULL, p1 = NULL, n = NULL, n.ratio = 1, sig.level = 0.05, power = NULL,
          alternative = c("two.sided", "less", "greater"))
{
  if (sum(sapply(list(p1, n, n.ratio, power, sig.level), is.null)) !=
      1)
    stop("exactly one of p1, n, n.ratio, power, and sig.level must be NULL")

  if (!is.null(n) && n < 2)
    stop("number of observations in the first group must be at least 2")
  if (!is.null(n.ratio) && n.ratio <= 0)
    stop("ratio between group sizes must be positive")
  if (!is.null(sig.level) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)))
    stop("sig.level must be between 0 and 1")
  if (!is.null(power) && (!is.numeric(power) || any(0 > power | power > 1)))
    stop("power must be between 0 and 1")
  if ((!is.null(p0) || !is.null(p1)) && (any(0 > p0 | p0 > 1) || any(0 > p1 | p1 > 1)))
    stop("proportions must be between 0 and 1")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)

  if (tside == 3) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = FALSE) - 2 * (asin(sqrt(p1))-asin(sqrt(p0))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = FALSE)
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = TRUE) - 2 * (asin(sqrt(p1))-asin(sqrt(p0))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = TRUE)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      pnorm(qnorm(sig.level/2, lower = FALSE) - 2 * abs(asin(sqrt(p1))-asin(sqrt(p0))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = FALSE) +
        pnorm(qnorm(sig.level/2, lower = TRUE) - 2 * abs(asin(sqrt(p1))-asin(sqrt(p0))) * sqrt((n * (n * n.ratio))/(n + (n * n.ratio))), lower = TRUE)
    })
  }
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(p1)) {
    if (tside == 2) {
      p_1 <- try(uniroot(function(p1) eval(p.body) - power, c(p0, 1 - 1e-10))$root)
      p_2 <- try(uniroot(function(p1) eval(p.body) - power, c(1e-10, p0))$root)
      if(inherits(p_1, "try-error") && inherits(p_2, "try-error")) {
        stop("no solution found")
      } else {
        if (inherits(p_1, "try-error")) {
          p1 <- c(NA, p_2)
        } else if (inherits(p_2, "try-error")) {
          p1 <- c(p_1, NA)
        } else {
          p1 <- c(p_1, p_2)
        }
      }
    } else {
      p1 <- uniroot(function(p1) eval(p.body) - power, c(1e-10, 1-1e-10))$root
    }
  }
  else if (is.null(n)) {
    if(n.ratio >= 1)
      n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
    if(n.ratio < 1)
      n <- uniroot(function(n) eval(p.body) - power, c((2/n.ratio), 1e+09))$root
  }
  else if (is.null(n.ratio))
    n.ratio <- uniroot(function(n.ratio) eval(p.body) - power, c(1e-10, 1e+09))$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) -
                           power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  NOTE <- "different sample sizes"
  METHOD <- "difference of proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(p0 = p0, p1 = p1, n = n, n.ratio = n.ratio, sig.level = sig.level,
                 power = power, alternative = alternative, method = METHOD,
                 note = NOTE), class = "power.htest")
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
      rho1 <- try(uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 1e+09))$root)
      rho2 <- try(uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root)
      if(inherits(rho1, "try-error") && inherits(rho2, "try-error")) {
        stop("no solution found")
      } else {
        if (inherits(rho1, "try-error")) {
          rho <- c(NA, rho2)
        } else if (inherits(rho2, "try-error")) {
          rho <- c(rho1, NA)
        } else {
          rho <- c(rho1, rho2)
        }
      }
    }

    if (tside == 1)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root

    if (tside == 3)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 1e+09))$root
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
        1 - pf(qf(1 - (sig.level/2), df1 = n - 1, df2 = (n * n.ratio) - 1) / rho, df1 = n - 1, df2 = (n * n.ratio) - 1) +
          pf(qf(sig.level/2, df1 = n - 1, df2 = (n * n.ratio) - 1) / rho, df1 = n - 1, df2 = (n * n.ratio) - 1)
    })
    }
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(rho)) {

    if (tside == 2) {
      rho1 <- try(uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 1e+09))$root)
      rho2 <- try(uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root)
      if(inherits(rho1, "try-error") && inherits(rho2, "try-error")) {
        stop("no solution found")
      } else {
        if (inherits(rho1, "try-error")) {
          rho <- c(NA, rho2)
        } else if (inherits(rho2, "try-error")) {
          rho <- c(rho1, NA)
        } else {
          rho <- c(rho1, rho2)
        }
      }
    }
    if (tside == 1)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1e-10, 1-1e-10))$root

    if (tside == 3)
      rho <- uniroot(function(rho) eval(p.body) - power, c(1+1e-10, 1e+09))$root
  }
  else if (is.null(n)) {
    if(n.ratio >= 1)
      n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
    if(n.ratio < 1)
      n <- uniroot(function(n) eval(p.body) - power, c((2/n.ratio), 1e+09))$root
  }
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

pwr.pois.test <- function (n = NULL, power = NULL, sig.level = 0.05,
                           lambda1 = NULL, lambda0 = NULL, t = 1, alternative = c("two.sided", "less", "greater"))
{

  if (sum(sapply(list(n, lambda1, lambda0, power, sig.level), is.null)) != 1)
    stop("exactly one of n, lambda1, lambda0, power, and sig.level must be NULL")
  if (!is.null(sig.level) && !is.numeric(sig.level) ||
      any(0 > sig.level | sig.level > 1))
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 >
                                                   power | power > 1))
    stop(sQuote("power"), " must be numeric in [0, 1]")
  alternative <- match.arg(alternative)
  test.side <- switch(alternative, less = 1, two.sided = 2, greater = 3)

  if(test.side == 3)
    p.body <- quote({
      1 - pnorm((qnorm(sig.level, lower.tail = FALSE) * sqrt(lambda0 / (n*t)) + lambda0 - lambda1) / sqrt(lambda1 / (n*t)))
    })
  if(test.side == 1)
    p.body <- quote({
      1 - pnorm((qnorm(sig.level, lower.tail = FALSE) * sqrt(lambda0 / (n*t)) + lambda1 - lambda0) / sqrt(lambda1 / (n*t)))
    })
  if(test.side == 2)
    p.body <- quote({
      (1 - pnorm((qnorm(sig.level/2, lower.tail = FALSE) * sqrt(lambda0 / (n*t)) + lambda0 - lambda1) / sqrt(lambda1 / (n*t)))) +
        (1 - pnorm((qnorm(sig.level/2, lower.tail = FALSE) * sqrt(lambda0 / (n*t)) + lambda1 - lambda0) / sqrt(lambda1 / (n*t))))
    })

  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+07))$root
  else if (is.null(lambda1)) {
    if (test.side == 2) {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(lambda0 + 1e-10, 1e+07))$root
    } else {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(1e-10, 1e+07))$root
    }
  }

  METHOD <- "One-sample Poisson Rate Test"
  return(structure(list(n = n, t = t, lambda1 = lambda1,
                        lambda0 = lambda0, sig.level = sig.level, power = power,
                        alternative = alternative, method = METHOD), class = "power.htest"))
}

pwr.2pois2n.test <- function (n1 = NULL, n.ratio = 1, power = NULL, sig.level = 0.05,
                              lambda1 = NULL, lambda2 = NULL, t1 = 1, t2 = 1, alternative = c("two.sided", "less", "greater"))
{

  if (sum(sapply(list(n1, lambda1, lambda2, power, sig.level), is.null)) != 1)
    stop("exactly one of n1, lambda1, lambda2, power, and sig.level must be NULL")
  if (!is.null(sig.level) && !is.numeric(sig.level) ||
      any(0 > sig.level | sig.level > 1))
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 >
                                                   power | power > 1))
    stop(sQuote("power"), " must be numeric in [0, 1]")
  alternative <- match.arg(alternative)
  test.side <- switch(alternative, less = 1, two.sided = 2, greater = 3)

  if(test.side == 1)
    p.body <- quote({
      pnorm((lambda2-lambda1) / sqrt(lambda1/(n1*t1) + lambda2/(n1*n.ratio*t2)) - qnorm(sig.level, lower.tail = FALSE))
    })
  if(test.side == 3)
    p.body <- quote({
      1 - pnorm((lambda2-lambda1) / sqrt(lambda1/(n1*t1) + lambda2/(n1*n.ratio*t2)) + qnorm(sig.level, lower.tail = FALSE))
    })
  if(test.side == 2)
    p.body <- quote({
      1 - pnorm((lambda2-lambda1) / sqrt(lambda1/(n1*t1) + lambda2/(n1*n.ratio*t2)) + qnorm(sig.level/2, lower.tail = FALSE)) +
        pnorm((lambda2-lambda1) / sqrt(lambda1/(n1*t1) + lambda2/(n1*n.ratio*t2)) - qnorm(sig.level/2, lower.tail = FALSE))
    })

  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(n1))
    n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
  else if (is.null(lambda1)) {
    if (test.side == 2) {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(lambda2 + 1e-10, 1e+07))$root
    } else {
      lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(1e-10, 1e+07))$root
    }
  }


  if (lambda1*n1 < 30 || lambda2*n1*n.ratio < 30) {
    if(test.side == 1)
      p.body <- quote({
        pnorm((sqrt(lambda2)-sqrt(lambda1)) / (1/2 * sqrt(1/(n1 * t1) + 1/(n1*n.ratio * t2))) - qnorm(sig.level, lower.tail = FALSE))
      })
    if(test.side == 3)
      p.body <- quote({
        1 - pnorm((sqrt(lambda2)-sqrt(lambda1)) / (1/2 * sqrt(1/(n1 * t1) + 1/(n1*n.ratio * t2))) + qnorm(sig.level, lower.tail = FALSE))
      })
    if(test.side == 2)
      p.body <- quote({
        1 - pnorm((sqrt(lambda2)-sqrt(lambda1)) / (1/2 * sqrt(1/(n1 * t1) + 1/(n1*n.ratio * t2))) + qnorm(sig.level/2, lower.tail = FALSE)) +
          pnorm((sqrt(lambda2)-sqrt(lambda1)) / (1/2 * sqrt(1/(n1 * t1) + 1/(n1*n.ratio * t2))) - qnorm(sig.level/2, lower.tail = FALSE))
      })

    if (is.null(power))
      power <- eval(p.body)
    else if (is.null(n1))
      n1 <- uniroot(function(n1) eval(p.body) - power, c(2, 1e+07))$root
    else if (is.null(lambda1)) {
      if (test.side == 2) {
        lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(lambda2 + 1e-10, 1e+07))$root
      } else {
        lambda1 <- uniroot(function(lambda1) eval(p.body) - power, c(1e-10, 1e+07))$root
      }
    }
  }

  METHOD <- "Two-sample Poisson Ratio Tests (Different Sizes)"
  return(structure(list(n1 = n1, n.ratio = n.ratio, t1 = t1, t2 = t2, lambda1 = lambda1,
                        lambda2 = lambda2, sig.level = sig.level, power = power,
                        alternative = alternative, method = METHOD), class = "power.htest"))
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
