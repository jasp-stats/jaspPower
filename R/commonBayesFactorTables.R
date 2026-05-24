.bfdAddDesignOutcomeColumns <- function(table) {
  table$addColumnInfo(name = "under",       title = gettext("Under"),       type = "string")
  table$addColumnInfo(name = "null",        title = gettext("Null"),         type = "number", overtitle = gettext("Bayes Factor Evidence"))
  table$addColumnInfo(name = "undecided",   title = gettext("Inconclusive"), type = "number", overtitle = gettext("Bayes Factor Evidence"))
  table$addColumnInfo(name = "alternative", title = gettext("Alternative"),  type = "number", overtitle = gettext("Bayes Factor Evidence"))
}

.bfdDesignOutcomeRowsFromOutcomes <- function(h1Outcome, h0Outcome) {
  data.frame(
    under       = c(gettext("H\u2081"), gettext("H\u2080")),
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
