.bfdCleanError <- function(error) {
  message <- as.character(error)
  message <- gsub("\n", " ", message, fixed = TRUE)
  message <- sub("^Error[^:]*: ", "", message)

  return(message)
}

.bfdCodeHtml <- function(code) {
  paste0("<pre><code>", .bfdEscapeHtml(code), "</code></pre>")
}

.bfdEscapeHtml <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)

  return(text)
}

.bfdFormatRCall <- function(functionName, args) {
  argNames <- names(args)
  values   <- vapply(args, .bfdFormatRValue, character(1))
  width    <- max(nchar(argNames))
  lines    <- paste0("  ", sprintf(paste0("%-", width, "s"), argNames), " = ", values)

  return(paste0(
    functionName,
    "(\n",
    paste(lines, collapse = ",\n"),
    "\n)"
  ))
}

.bfdFormatRValue <- function(x) {
  if (is.null(x))
    return("NULL")

  if (length(x) != 1) {
    values <- vapply(as.list(x), .bfdFormatRValue, character(1))
    return(paste0("c(", paste(values, collapse = ", "), ")"))
  }

  if (is.character(x))
    return(encodeString(x, quote = "\""))

  if (is.logical(x))
    return(if (isTRUE(x)) "TRUE" else "FALSE")

  if (is.numeric(x)) {
    if (is.na(x))
      return(if (is.integer(x)) "NA_integer_" else "NA_real_")

    return(format(signif(x, 12), scientific = FALSE, trim = TRUE))
  }

  return(encodeString(as.character(x), quote = "\""))
}

.bfdClampProbability <- function(x) {
  if (!is.finite(x))
    return(NA_real_)

  return(max(0, min(1, x)))
}

.bfdFormatNumber <- function(x) {
  format(signif(x, 4), trim = TRUE)
}

.bfdDecisionRuleLabel <- function(settings, target) {
  if (target == "h1")
    return(gettextf("BF\u2081\u2080 \u2265 %1$s", .bfdFormatNumber(settings[["bf10Threshold"]])))

  return(gettextf("BF\u2080\u2081 \u2265 %1$s", .bfdFormatNumber(settings[["bf01Threshold"]])))
}
