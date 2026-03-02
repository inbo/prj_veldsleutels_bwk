html_write_index_page <- function(keys, folder = "", subfolder = "www") {
  fn <- if (nchar(folder)) file.path(folder, "index.html") else "index.html"

  cat('<!DOCTYPE html>\n<html lang="nl">\n<head>\n  <meta charset="UTF-8">\n', file = fn, append = FALSE)
  cat("  <title>Inverde Veldsleutels</title>\n", file = fn, append = TRUE)
  cat(glue("  <style>\n{html_get_style()}\n  </style>\n"), file = fn, append = TRUE)
  cat("</head>\n<body>\n  <h1>Veldsleutels Index</h1>\n", file = fn, append = TRUE)

  # Start the grid
  cat('  <div class="index-grid">\n', file = fn, append = TRUE)

  for (k in keys) {
    target_path <- if (nchar(subfolder)) paste(subfolder, k, sep = "/") else k
    cat(glue('    <a href="{target_path}.html" class="index-btn">{toupper(k)}</a>\n'), file = fn, append = TRUE)
  }

  cat("  </div>\n", file = fn, append = TRUE) # End the grid
  cat("</body>\n</html>", file = fn, append = TRUE)
}
