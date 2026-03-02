




# Main execution
# Create the index page
create_index_page(sleutels_parsed)

# Loop through each parsed key and create a separate HTML file for each
for (sleutel_name in names(sleutels_parsed)) {
  # Create a filename for the HTML file
  fn <- paste0(sleutel_name, "sleutel.html")
  sleutel <- sleutels_parsed[[sleutel_name]]
  
  # Combine default style with clickable header style
  combined_style <- paste(get_default_style(),
                          get_clickable_header_style(),
                          sep = "\n")
  
  # Start the HTML file with the combined style
  htmlstart(fn, title = paste0(sleutel_name, " sleutel"), style = combined_style)
  
  # Add back link and title
  htmlwrite(glue("<h1>{sleutel_name} sleutel</h1>"), fn = fn)
  
  # Write the records to the HTML file
  lapply(sleutel, FUN = write_record, fn = fn)
  
  # End the HTML file
  htmlend(fn = fn)
}