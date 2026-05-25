.bfdCreatePlot <- function(parent, key, title, position, dependencies, width, height) {
  if (!is.null(parent[[key]]))
    return(NULL)

  plot <- createJaspPlot(title = title, width = width, height = height)
  plot$dependOn(dependencies)
  plot$position <- position
  parent[[key]] <- plot

  return(plot)
}

.bfdCachedPlotData <- function(jaspResults, stateKey, dependencies, dataKey, compute) {
  state <- jaspResults[[stateKey]]
  if (is.null(state)) {
    state <- createJaspState()
    state$dependOn(dependencies)
    jaspResults[[stateKey]] <- state
  }

  cache <- state$object
  if (is.null(cache))
    cache <- list()

  if (!is.null(cache[[dataKey]]))
    return(cache[[dataKey]])

  cache[[dataKey]] <- try(compute(), silent = TRUE)
  state$object <- cache

  return(cache[[dataKey]])
}

.bfdProbabilityYScale <- function() {
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%"))
}

.bfdTargetPowerLine <- function(settings, targets) {
  ggplot2::geom_hline(
    yintercept = unique(vapply(targets, function(target) .bfdTargetPower(settings, target), numeric(1))),
    linetype   = "dotted",
    color      = "#555555"
  )
}

.bfdApplyPlotTheme <- function(plot, settings, linetypeValues = NULL) {
  palette        <- settings[["colorPalette"]]
  legendPosition <- settings[["legendPosition"]]

  plot <- .pwrApplyPlotTheme(plot, legendPosition = .bfdLegendPosition(legendPosition)) +
    jaspGraphs::scale_JASPcolor_discrete(palette, labels = .bfdPlotmathLabels)

  if (is.null(linetypeValues)) {
    plot <- plot + ggplot2::scale_linetype_discrete(labels = .bfdPlotmathLabels)
  } else {
    plot <- plot + ggplot2::scale_linetype_manual(values = linetypeValues, labels = .bfdPlotmathLabels)
  }

  if (legendPosition == "rightInside") {
    plot <- plot +
      ggplot2::theme(
        legend.position.inside = c(0.98, 0.5),
        legend.justification   = c(1, 0.5)
      )
  }

  return(plot)
}

.bfdUnderLinetypeValues <- function() {
  setNames(c("solid", "dashed"), c(.bfdUnderLabel("h1"), .bfdUnderLabel("h0")))
}

.bfdPriorTypeLinetypeValues <- function() {
  setNames(c("solid", "dashed"), c(gettext("Analysis Prior"), gettext("Design Prior")))
}

.bfdLegendPosition <- function(legendPosition) {
  if (identical(legendPosition, "rightInside"))
    return("inside")

  return(legendPosition)
}

.bfdPlotWidth <- function(settings) {
  width <- 735
  if (settings[["legendPosition"]] %in% c("top", "bottom", "rightInside"))
    return(round(width * 0.8))

  return(width)
}

.bfdPlotmathLabels <- function(labels) {
  labels <- as.character(labels)
  parsed <- vapply(labels, .bfdPlotmathLabel, character(1), USE.NAMES = FALSE)
  parse(text = parsed)
}

.bfdPlotmathLabel <- function(label) {
  tokenMap <- setNames(
    c(
      'BF["10"]', 'BF["01"]', "H[1]", "H[0]",
      "N[1]", "N[2]", "n[1]", "n[2]",
      "p[0]", "p[1]", "p[2]",
      "theta[0]", "lambda[0]", "lambda[1]", "lambda[2]",
      "mu[0]", "sigma[0]", "s[1]", "s[2]"
    ),
    c(
      paste0("BF", "\u2081", "\u2080"), paste0("BF", "\u2080", "\u2081"),
      paste0("H", "\u2081"), paste0("H", "\u2080"),
      paste0("N", "\u2081"), paste0("N", "\u2082"),
      paste0("n", "\u2081"), paste0("n", "\u2082"),
      paste0("p", "\u2080"), paste0("p", "\u2081"), paste0("p", "\u2082"),
      paste0("\u03B8", "\u2080"),
      paste0("\u03BB", "\u2080"), paste0("\u03BB", "\u2081"), paste0("\u03BB", "\u2082"),
      paste0("\u03BC", "\u2080"), paste0("\u03C3", "\u2080"),
      paste0("s", "\u2081"), paste0("s", "\u2082")
    )
  )
  pattern <- paste(names(tokenMap), collapse = "|")
  matches <- gregexpr(pattern, label, perl = TRUE)[[1]]

  if (matches[1] == -1)
    return(.bfdPlotmathText(label))

  matchLengths <- attr(matches, "match.length")
  parts        <- character()
  cursor       <- 1

  for (i in seq_along(matches)) {
    start <- matches[i]
    end   <- start + matchLengths[i] - 1

    if (start > cursor)
      parts <- c(parts, .bfdPlotmathText(substr(label, cursor, start - 1)))

    token <- substr(label, start, end)
    parts <- c(parts, unname(tokenMap[[token]]))
    cursor <- end + 1
  }

  if (cursor <= nchar(label))
    parts <- c(parts, .bfdPlotmathText(substr(label, cursor, nchar(label))))

  if (length(parts) == 1)
    return(parts)

  paste0("paste(", paste(parts, collapse = ", "), ")")
}

.bfdPlotmathText <- function(text) {
  text <- gsub("\\", "\\\\", text, fixed = TRUE)
  text <- gsub("\"", "\\\"", text, fixed = TRUE)
  paste0("\"", text, "\"")
}

.bfdSegment <- function(...) {
  ggplot2::annotate(
    geom     = "segment",
    linetype = "dashed",
    color    = "#333333",
    ...
  )
}
