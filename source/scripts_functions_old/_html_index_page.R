# Create index.html
html_index_page <- function(sleutels_parsed) {
  fn <- "index.html"

  # Combine styles
  combined_style <- paste(get_default_style(),
    get_clickable_header_style(),
    sep = "\n"
  )

  # Start the HTML file
  htmlstart(fn, title = "Veldsleutels", style = combined_style)

  # Write main heading
  htmlwrite("<h1>Veldsleutels</h1>", fn = fn)

  # Add introduction text
  htmlwrite("<p>Selecteer een van de onderstaande sleutels om te beginnen met determineren:</p>", fn = fn)

  # Create the sleutel links with clickable-header class
  sleutelnamen <- paste0(names(sleutels_parsed), "sleutel")
  sleutellinks <- paste0(
    "<a href='",
    paste(sleutelnamen, "html", sep = "."),
    "' class='clickable-header'>",
    gsub("([A-Z])", " \\1", names(sleutels_parsed)), "sleutel",
    "</a>"
  )

  # Write the clickable links
  htmlwrite(paste0(sleutellinks, collapse = "\n"), fn = fn)

  # Add footer information
  htmlwrite("<div style='margin-top: 50px; font-size: 0.9em; color: #666; text-align: center;'>", fn = fn)
  htmlwrite("<p>© 2025 Veldsleutels</p>", fn = fn)
  htmlwrite("</div>", fn = fn)

  # End the HTML file
  htmlend(fn = fn)
}
