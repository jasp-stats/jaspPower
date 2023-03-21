# ==== Default Plotting Configurations ====

# Common theme to be used for all plots
.pwrPlotTheme <- function() {
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

# Default settings to be used in plots
.pwrPlotDefaultSettings <- list(
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
.pwrPlotDefaultSettings$pal <- function(...) {
  viridis::viridis(..., alpha = .pwrPlotDefaultSettings$background.alpha)
}

# ==== Minor Plotting Helpers ====

# Convenience function to draw segment lines
.segment <- function(...) {
  return(ggplot2::annotate(
    geom = "segment",
    size = 1,
    linetype = "dashed",
    ...
  ))
}

# ==== Major Plotting Functions ====
.plotPowerContour <- function(options, state, ggtheme, ...) {
  calc <- options$calculation

  z.delta <- state$z.delta
  z.pwr <- state$z.pwr
  ps <- state$ps
  pow <- state$pow
  n <- state$n
  n1 <- state$n1
  n2 <- state$n2
  alpha <- state$alpha
  dd <- state$dd
  nn <- state$nn
  ps <- state$ps
  delta <- state$delta
  n_ratio <- state$n_ratio

  if (is.null(n_ratio)) {
    # Use ratio of 1 for one or paired samples
    n_ratio <- 1
  }

  if (is.null(n)) {
    # Add n for indipendent samples
    n <- n1
  }
  if (options$test == "independentSamplesTTest" || options$test == "pairedSamplesTTest" ||
    options$test == "oneSampleTTest" || options$test == "oneSampleZTest") {
    es <- paste0("|", "\u03B4", "|")
  }
  if (options$test == "oneSampleProportion" || options$test == "twoSamplesProportion") {
    es <- "|h|"
  }
  if (options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio") {
    es <- "\u03C1"
  }


  # If group sizes differ, provide a secondary axis
  secondary_axis <- ggplot2::waiver()
  if (n_ratio != 1) {
    secondary_axis <- ggplot2::sec_axis(
      ~ . * n_ratio,
      name = gettext("Sample size (group 2)")
    )
  }

  # Checking whether geom_contour_filled is availaible
  # (as it is e.g. not yet in the ggplot2 version of JASP 0.14.1)
  # TODO: Remove this check in the future
  if (!exists("geom_contour_filled", where = asNamespace("ggplot2"), mode = "function")) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "ERROR: The power contour plot needs a newer version of ggplot / JASP to function")
    )
  }

  # Dirty fix for a bug where ggplot is unable to properly bin values of 1...
  z.pwr[z.pwr == 1] <- 0.999999999

  p <- ggplot2::ggplot(
    .transformContourMatrix(x = nn, y = dd, z = z.pwr),
    ggplot2::aes(x = x, y = y, z = z)
  ) +
    ggplot2::geom_contour_filled(breaks = pretty(c(0, 1), n = ps$pow.n.levels), alpha = ps$background.alpha) +
    ggplot2::scale_x_log10(sec.axis = secondary_axis) +
    ggplot2::labs(
      x = gettext("Sample size (group 1)"),
      y = gettextf("Hypothetical effect size (%s)", es),
      fill = gettext("Power")
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorsteps(barheight = ggplot2::unit(7, "cm"))) +
    # Highlight boundary of power
    # Note: This doesn't render quite perfectly right now
    # ggplot2::scale_y_continuous(limits = c(min(dd), max(dd))) +
    # ggplot2::annotate("line", x = nn, y = z.delta) +
    # Highlight N on axis
    .segment(
      x = n, y = delta, xend = n, yend = min(dd)
    ) +
    # Highlight effect size on axis
    .segment(
      x = n, y = delta, xend = min(nn), yend = delta
    ) +
    # Add point highlighting intersection
    ggplot2::annotate("point", x = n, y = delta, size = 3) +
    ggtheme

  return(p)
}

.plotPowerCurveES <- function(options, state, ggtheme, ...) {
  y <- state$y
  cols <- state$cols
  yrect <- state$yrect
  pow <- state$pow
  n1 <- state$n1
  n2 <- state$n2
  n <- state$n
  alpha <- state$alpha
  dd <- state$dd
  delta <- state$delta

  if (options$test == "independentSamplesTTest" || options$test == "pairedSamplesTTest" ||
    options$test == "oneSampleTTest" || options$test == "oneSampleZTest") {
    es <- "|\u03B4|"
  }
  if (options$test == "oneSampleProportion" || options$test == "twoSamplesProportion") {
    es <- "|h|"
  }
  if (options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio") {
    es <- "\u03C1"
  }

  ps <- .pwrPlotDefaultSettings

  if (is.null(n2)) {
    # Paired or one sample
    plot_subtitle <- substitute(
      paste(N == n, ", ", alpha == a),
      list(a = alpha, n = n)
    )
  } else {
    # Indipendent Samples
    plot_subtitle <- substitute(
      paste(N[1] == n1, ", ", N[2] == n2, ", ", alpha == a),
      list(a = alpha, n1 = n1, n2 = n2)
    )
  }

  p <- ggplot2::ggplot(data.frame(), ggplot2::aes(x = dd, y = y))

  # Add shaded background
  for (i in 1:ps$pow.n.levels) {
    p <- p +
      ggplot2::annotate(geom = "rect", xmin = min(dd), ymin = yrect[i], xmax = max(dd), ymax = yrect[i + 1], fill = cols[i])
  }

  p <- p +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::labs(
      x = gettextf("Hypothetical effect size (%s)", es),
      y = gettext("Power"),
      subtitle = plot_subtitle
    ) +
    .segment(
      x = delta, y = pow,
      xend = delta, yend = 0
    ) +
    .segment(
      x = min(dd), y = pow,
      xend = delta, yend = pow,
    ) +
    ggtheme

  return(p)
}

.plotPowerCurveN <- function(options, state, ggtheme, ...) {
  cols <- state$cols
  yrect <- state$yrect
  lims <- state$lims
  delta <- state$delta
  alpha <- state$alpha
  n_ratio <- state$n_ratio
  nn <- state$nn
  pow <- state$pow
  n <- state$n
  y <- state$y

  if (options$test == "independentSamplesTTest" || options$test == "pairedSamplesTTest" ||
    options$test == "oneSampleTTest" || options$test == "oneSampleZTest") {
    es <- "|\u03B4|"
  }
  if (options$test == "oneSampleProportion" || options$test == "twoSamplesProportion") {
    es <- "|h|"
  }
  if (options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio") {
    es <- "\u03C1"
  }

  ps <- .pwrPlotDefaultSettings

  if (is.null(n_ratio)) {
    # Paired or one sample
    plot_subtitle <- gettextf("%s = %s, %s = %s", es, round(delta, 3), "\u03B1", alpha)
  } else {
    # Indipendent Samples
    plot_subtitle <- gettextf("%s = %s, N%s = %s %s N%s, %s = %s", es, round(delta, 3), "\u2082", n_ratio, "\u00D7", "\u2081", "\u03B1", alpha)
  }

  # Creat basic plot
  p <- ggplot2::ggplot(data.frame(), ggplot2::aes(x = nn, y = y))

  # Add shaded background
  for (i in 1:ps$pow.n.levels) {
    p <- p +
      ggplot2::annotate(geom = "rect", xmin = lims$xlim[1], ymin = yrect[i], xmax = lims$xlim[2], ymax = yrect[i + 1], fill = cols[i])
  }

  # Add main plot
  p <- p +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_x_log10(limits = lims$xlim) +
    ggplot2::scale_y_continuous(limits = lims$ylim) +
    ggplot2::labs(
      x = gettext("Sample size (group 1)"),
      y = gettext("Power"),
      subtitle = plot_subtitle
    ) +
    .segment(
      x = n, y = pow,
      xend = n, yend = 0
    ) +
    .segment(
      x = min(nn), y = pow,
      xend = n, yend = pow,
    ) +
    ggtheme

  return(p)
}

.plotPowerDist <- function(options, state, ggtheme, ...) {
  if (options$test == "independentSamplesTTest" || options$test == "pairedSamplesTTest" ||
    options$test == "oneSampleTTest" || options$test == "oneSampleZTest") {
    es <- "|\u03B4|"
  }
  if (options$test == "oneSampleProportion" || options$test == "twoSamplesProportion") {
    es <- "|h|"
  }
  if (options$test == "oneSampleVarianceRatio" || options$test == "twoSamplesVarianceRatio") {
    es <- ifelse(state$d < 1, "1/\u03C1", "\u03C1")
  }

  ps <- .pwrPlotDefaultSettings

  if (is.null(state)) {
    return(FALSE)
  }

  curves <- state$curves
  rect <- state$rect
  lims <- state$lims
  alt <- options$alternative

  themeSpec <- ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    legend.position = "none"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = curves, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = group), color = "#333333", alpha = .6) +
    ggplot2::geom_rect(data = rect, ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "white", alpha = 0.4) +
    ggplot2::geom_vline(data = rect, ggplot2::aes(xintercept = x1), linetype = "dashed") +
    ggplot2::geom_vline(data = rect, ggplot2::aes(xintercept = x2), linetype = "dashed") +
    ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE) +
    ggplot2::labs(x = gettextf("Observed effect size (%s)", es), y = gettext("Probability Density")) +
    ggtheme +
    themeSpec +
    ggplot2::scale_fill_manual(values = ps$pal(5)[c(4, 1)])

  return(p)
}
