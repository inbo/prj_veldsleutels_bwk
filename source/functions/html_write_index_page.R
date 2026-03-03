#' Write HTML Index Page for Field Keys
#'
#' Creates an HTML index page with navigation buttons for all available field keys.
#' This serves as the main entry point for the field key web interface.
#'
#' @param keys Character vector of key names (e.g., c("bos", "grasland", "heide")).
#' @param folder Character string specifying the output directory. Default is ""
#'   (current directory).
#' @param subfolder Character string specifying the subdirectory where individual
#'   key HTML files are located. Default is "www".
#'
#' @return Invisible NULL. The function writes the index.html file to disk.
#'
#' @details
#' The function generates a responsive HTML page with:
#' \itemize{
#'   \item A centered title "VELDSLEUTELS INDEX"
#'   \item A grid layout of buttons for each field key
#'   \item Links to individual key pages in the specified subfolder
#'   \item Embedded CSS styling from \code{\link{html_get_style}}
#' }
#'
#' The output file is named "index.html" and placed in the specified folder.
#'
#' @examples
#' \dontrun{
#' # Create index in current directory linking to www/ subfolder
#' keys <- c("bos", "grasland", "heide", "moeras", "water")
#' html_write_index_page(keys, folder = "", subfolder = "www")
#'
#' # Create index in specific folder
#' html_write_index_page(keys, folder = "output", subfolder = "www")
#' }
#'
#' @seealso \code{\link{html_write_key_page}}, \code{\link{html_get_style}}
html_write_index_page <- function(keys, folder = "", subfolder = "www") {
  fn <- if (nchar(folder)) file.path(folder, "index.html") else "index.html"

  cat('<!DOCTYPE html>\n<html lang="nl">\n<head>\n', file = fn, append = FALSE)
  cat('  <meta charset="UTF-8">\n', file = fn, append = TRUE)
  cat('  <meta name="viewport" content="width=device-width, initial-scale=1.0">\n', file = fn, append = TRUE)
  cat("  <title>Veldsleutels Index</title>\n", file = fn, append = TRUE)
  cat(glue("  <style>\n{html_get_style()}\n  </style>\n"), file = fn, append = TRUE)
  cat('</head>\n<body>\n  <h1 style="text-align:center; margin-bottom:30px;">VELDSLEUTELS INDEX</h1>\n  <div class="index-grid">\n', file = fn, append = TRUE)

  for (k in keys) {
    target_path <- if (nchar(subfolder)) paste(subfolder, k, sep = "/") else k
    cat(glue('    <a href="{target_path}.html" class="index-btn">{toupper(k)}</a>\n'), file = fn, append = TRUE)
  }

  cat("  </div>\n</body>\n</html>", file = fn, append = TRUE)
}
