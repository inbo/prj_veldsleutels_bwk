html_write_index_page <- function(keys, folder = "", subfolder = "www") {
  if (nchar(folder)) {
    fn <- file.path(folder, "index.html")
  } else {
    fn <- "index.html"
  }

  cat('<!DOCTYPE html>\n<html>\n<head>\n  <meta charset="UTF-8">\n  <style>\n', file = fn, append = FALSE)
  cat(html_get_style(), file = fn, append = TRUE)
  cat("\n  </style>\n</head>\n<body>\n  <h1>Veldsleutels Index</h1>\n", file = fn, append = TRUE)
  for (k in keys) {
    if (nchar(subfolder)) {
      ks <- paste(subfolder, k, sep = "/")
    } else {
      ks <- k
    }
    cat(glue('  <a href="{ks}.html" class="index-btn">{toupper(k)}</a>\n'), file = fn, append = TRUE)
  }
  cat("</body>\n</html>", file = fn, append = TRUE)
}
