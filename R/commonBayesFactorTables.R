.bfdAddDesignOutcomeColumns <- function(table, underTitle = gettext("Under"), overtitle = gettext("Bayes Factor Evidence"),
                                       nullTitle = gettext("Null"), undecidedTitle = gettext("Inconclusive"),
                                       alternativeTitle = gettext("Alternative")) {
  table$addColumnInfo(name = "under",       title = underTitle,              type = "string")
  table$addColumnInfo(name = "null",        title = nullTitle,               type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "undecided",   title = undecidedTitle,          type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "alternative", title = alternativeTitle,        type = "number", overtitle = overtitle)
}

.bfdDesignOutcomeRowsFromOutcomes <- function(h1Outcome, h0Outcome, underLabels = c(gettext("H\u2081"), gettext("H\u2080"))) {
  data.frame(
    under       = underLabels,
    null        = c(h1Outcome[["null"]],        h0Outcome[["null"]]),
    undecided   = c(h1Outcome[["undecided"]],   h0Outcome[["undecided"]]),
    alternative = c(h1Outcome[["alternative"]], h0Outcome[["alternative"]]),
    stringsAsFactors = FALSE
  )
}

.bfdAddPriorsTableColumns <- function(table) {
  table$addColumnInfo(name = "hypothesis",    title = gettext("Hypothesis"),     type = "string")
  table$addColumnInfo(name = "designPrior",   title = gettext("Design Prior"),   type = "string")
  table$addColumnInfo(name = "analysisPrior", title = gettext("Analysis Prior"), type = "string")
}

.bfdPriorsRows <- function(settings) {
  data.frame(
    hypothesis    = c(gettext("H\u2081"), gettext("H\u2080")),
    designPrior   = c(
      .bfdPriorString(.bfdDesignPriorLabel(settings, "h1"), .bfdDesignPriorParameters(settings, "h1")),
      .bfdPriorString(.bfdDesignPriorLabel(settings, "h0"), .bfdDesignPriorParameters(settings, "h0"))
    ),
    analysisPrior = c(
      .bfdPriorString(.bfdAnalysisPriorTableLabel(settings), .bfdAnalysisPriorParameters(settings)),
      .bfdNullPriorString(settings)
    ),
    stringsAsFactors = FALSE
  )
}

.bfdEmptyTableRow <- function(columns, stringColumns = character(), integerColumns = character()) {
  values <- lapply(columns, function(column) {
    if (column %in% stringColumns)
      return("")

    if (column %in% integerColumns)
      return(NA_integer_)

    return(NA_real_)
  })
  names(values) <- columns

  data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

.bfdEmptyDesignOutcomeRow <- function() {
  return(.bfdEmptyTableRow(
    columns       = c("under", "null", "undecided", "alternative"),
    stringColumns = "under"
  ))
}

.bfdExplanatoryTextEnabled <- function(settings) {
  identical(settings[["explanatoryText"]], TRUE)
}

.bfdAddExplanationFootnotes <- function(table, settings, notes) {
  if (!.bfdExplanatoryTextEnabled(settings))
    return(invisible(FALSE))

  notes <- notes[!is.na(notes) & nzchar(notes)]
  if (length(notes) == 0)
    return(invisible(FALSE))

  for (note in notes)
    table$addFootnote(note, symbol = gettext("Explanation:"))

  return(invisible(TRUE))
}

.bfdAddExplanationHtml <- function(parent, key, settings, position, dependencies, text,
                                  title = gettext("Explanation")) {
  if (!.bfdExplanatoryTextEnabled(settings)) {
    if (!is.null(parent[[key]]))
      parent[[key]] <- NULL
    return(invisible(FALSE))
  }

  text <- text[!is.na(text) & nzchar(text)]
  if (length(text) == 0) {
    if (!is.null(parent[[key]]))
      parent[[key]] <- NULL
    return(invisible(FALSE))
  }

  if (!is.null(parent[[key]]))
    return(invisible(FALSE))

  html <- createJaspHtml(title = title)
  html$dependOn(dependencies)
  html$position <- position
  parent[[key]] <- html

  html[["text"]] <- paste0("<p>", paste(text, collapse = "</p><p>"), "</p>")
  return(invisible(TRUE))
}
