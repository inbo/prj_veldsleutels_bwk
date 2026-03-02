html_write_index_page <- function(keys, folder = "", subfolder = "www") {
  fn <- if (nchar(folder)) file.path(folder, "index.html") else "index.html"

  cat('<!DOCTYPE html>\n<html lang="nl">\n<head>\n', file = fn, append = FALSE)
  cat('  <meta charset="UTF-8">\n', file = fn, append = TRUE)
  cat('  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">\n', file = fn, append = TRUE)
  cat("  <title>Inverde Veldsleutels Index</title>\n", file = fn, append = TRUE)
  cat(glue("  <style>\n{html_get_style()}\n  </style>\n"), file = fn, append = TRUE)
  cat('</head>\n<body>\n  <h1 style="text-align:center; margin-bottom:40px;">VELDSLEUTELS INDEX</h1>\n  <div class="index-grid">\n', file = fn, append = TRUE)

  for (k in keys) {
    target_path <- if (nchar(subfolder)) paste(subfolder, k, sep = "/") else k
    cat(glue('    <a href="{target_path}.html" class="index-btn">{toupper(k)}</a>\n'), file = fn, append = TRUE)
  }

  cat("  </div>\n</body>\n</html>", file = fn, append = TRUE)
}
