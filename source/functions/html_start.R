html_start <- function(fn, title, style = NULL) {
  cat(paste0('<!DOCTYPE html>
<html lang="nl">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>', title, "</title>"),
    file = fn, append = FALSE
  )
  if (!is.null(style)) {
    cat("<style>\n", style, "\n</style>\n", file = fn, append = TRUE)
  }
  cat("</head>\n<body>\n", file = fn, append = TRUE)
}

htmlend <- function(fn) {
  # Add a "back to index" link
  cat('<div class="back-to-index"><a href="index.html">Terug naar Index</a></div>\n', file = fn, append = TRUE)
  cat("</body>\n</html>\n", file = fn, append = TRUE)
}

htmlwrite <- function(text, fn) {
  cat(text, "\n", file = fn, append = TRUE)
}
