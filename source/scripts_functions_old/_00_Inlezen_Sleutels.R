## Init libraries and functions
#--------------------------------------------------------------------

library(tidyverse) # for data manipulation
library(googlesheets4) # for reading Google Sheets
library(glue) # for string interpolation


#source all functions
all_files <- list.files(path = "source/functions/", full.names = TRUE)
file_names <- basename(all_files)
files_to_source <- all_files[!startsWith(file_names, "_")]
for (file in files_to_source) {
  source(file)
}

gs4_auth(email = "pieter.verschelde@inbo.be")
sleutel_loc <- read_tsv("data/Sleutel_Gsheet.txt",
                        show_col_types = FALSE)


## Read and parse all keys
#--------------------------------------------------------------------

sleutel_data <- read_key_data(sleutel_loc)
save(sleutel_data, file = "interim/sleutel_data.RData")


sleutels_parsed <- lapply(sleutel_data, parse_key_data)
save(sleutels_parsed, file = "interim/sleutels_parsed.RData")

## Create HTML LANDING PAGE (index.html)
#---------------------------------------------------------------------

fn <- 'index.html'

# Combine default style with our new clickable header style
combined_style <- paste(get_default_style(),
                        get_clickable_header_style(),
                        sep = "\n")

htmlstart(fn, title = 'Veldsleutels', style = combined_style)

# Create the sleutel names and prepare the links with the new clickable-header class
sleutelnamen <- paste0(names(sleutels_parsed), "sleutel")
sleutellinks <- paste0(
  "<a href='", 
  paste(sleutelnamen, "html", sep = "."), 
  "' class='clickable-header'>", 
  sleutelnamen, 
  "</a>"
)

htmlwrite("<h1>Veldsleutels</h1>", fn = fn)
htmlwrite(paste0(sleutellinks, collapse = "\n"), fn = fn)
htmlend(fn = fn)

##
## Create SLEUTEL PAGES (xxxsleutel.html)
#---------------------------------------------------------------------
# Loop through each parsed key and create a separate HTML file for each
for (sleutel_name in names(sleutels_parsed)) {

  # Create a filename for the HTML file
  fn <- paste0(sleutel_name, "sleutel.html")
  sleutel <- sleutels_parsed[[sleutel_name]]
  
  # Combine default style with our new clickable header style
  combined_style <- paste(get_default_style(),
                          get_clickable_header_style(),
                          sep = "\n")
  
  # Start the HTML file with the combined style
  htmlstart(fn, title = sleutel_name, style = combined_style)
  
  # Write the records to the HTML file
  lapply(sleutel, FUN = write_record, fn = fn)
  
  # End the HTML file
  htmlend(fn = fn)
}

#HTML openen in libreoffice werkt beter dan word, zodat iedere stap een H2 is en ieder antwoord een H3, bij word werkt de H3 niet, de interne links werken in beiden. Daarna bewaren als docx en de layout correct maken (nieuwe multilevel lijst)