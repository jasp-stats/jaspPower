Evidence <- function(jaspResults, dataset, options) {
  html <- jaspResults[["interface"]]

  if (is.null(html)) {
    html <- createJaspHtml(title = gettext("Evidence"))
    html$position <- 1
    jaspResults[["interface"]] <- html
  }

  html[["text"]] <- gettext("The Evidence interface is available. Calculations will be implemented in a subsequent update.")

  return()
}
