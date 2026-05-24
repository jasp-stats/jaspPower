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
    designPrior   = c(.bfdDesignPriorString(settings, "h1"), .bfdDesignPriorString(settings, "h0")),
    analysisPrior = c(.bfdAnalysisPriorString(settings), .bfdNullPriorString(settings)),
    stringsAsFactors = FALSE
  )
}

.bfdCreateTable <- function(parent, key, title, position, dependencies, showSpecifiedColumnsOnly = FALSE) {
  if (!is.null(parent[[key]]))
    return(NULL)

  table <- createJaspTable(title = title)
  table$dependOn(dependencies)
  table$position <- position
  table$showSpecifiedColumnsOnly <- showSpecifiedColumnsOnly
  parent[[key]] <- table

  return(table)
}

.bfdCreateHtml <- function(parent, key, title, position, dependencies) {
  if (!is.null(parent[[key]]))
    return(NULL)

  html <- createJaspHtml(title = title)
  html$dependOn(dependencies)
  html$position <- position
  parent[[key]] <- html

  return(html)
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

  html <- .bfdCreateHtml(
    parent       = parent,
    key          = key,
    title        = title,
    position     = position,
    dependencies = dependencies
  )
  if (is.null(html))
    return(invisible(FALSE))

  html[["text"]] <- paste0("<p>", paste(text, collapse = "</p><p>"), "</p>")
  return(invisible(TRUE))
}
