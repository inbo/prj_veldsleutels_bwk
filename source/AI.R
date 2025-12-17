htmlstart <- function(fn, title, style = NULL) {
  cat(paste0('<!DOCTYPE html>
<html lang="nl">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>', title, '</title>'), 
      file = fn, append = FALSE)
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

get_default_style <- function() {
  styletext <- "  
  /* Base styles */
  * {
    box-sizing: border-box;
  }
  
  body {
    background-color: #e6f2f7;
    font-family: 'Arial', sans-serif;
    font-size: 18px;
    line-height: 1.6;
    color: #333;
    max-width: 1200px;
    margin: 0 auto;
    padding: 15px;
  }
  
  h1 {
    color: #1a5276;
    padding-bottom: 10px;
    border-bottom: 2px solid #1a5276;
    margin-bottom: 25px;
  }
  
  h2, h3, h4 {
    color: #2874a6;
    margin-top: 25px;
    margin-bottom: 15px;
  }
  
  a {
    color: #2874a6;
    text-decoration: none;
    transition: color 0.3s;
  }
  
  a:hover {
    color: #1a5276;
    text-decoration: underline;
  }
  
  /* Table styles */
  table {
    width: 100%;
    margin: 20px 0;
    border-collapse: collapse;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1);
  }
  
  th, td {
    border: 1px solid #b3d9e6;
    padding: 12px;
    text-align: left;
  }
  
  th {
    background-color: #b3d9e6;
    color: #1a5276;
  }
  
  tr:nth-child(even) {
    background-color: #f2f9fc;
  }
  
  tr:hover {
    background-color: #d9ebf2;
  }
  
  /* Step styling */
  .step-container {
    background-color: white;
    border-radius: 8px;
    margin-bottom: 30px;
    padding: 20px;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1);
  }
  
  .step-header {
    background-color: #d9ebf2;
    padding: 15px;
    margin: -20px -20px 20px -20px;
    border-radius: 8px 8px 0 0;
    position: relative;
  }
  
  .step-number {
    font-weight: bold;
    color: #1a5276;
  }
  
  .step-incoming {
    font-size: 0.9em;
    color: #666;
    margin-top: 5px;
  }
  
  .step-remark {
    background-color: #fff9e0;
    padding: 10px;
    border-left: 4px solid #ffd700;
    margin: 15px 0;
    font-style: italic;
  }
  
  /* Answer styling */
  .answer {
    background-color: #f7f9fa;
    border-left: 4px solid #2874a6;
    padding: 15px;
    margin: 15px 0;
    border-radius: 0 8px 8px 0;
    transition: background-color 0.3s;
  }
  
  .answer:hover {
    background-color: #e6f2f7;
  }
  
  .answer a {
    display: block;
    font-weight: bold;
  }
  
  .answer-remark {
    font-style: italic;
    color: #666;
    margin-top: 10px;
  }
  
  .extra-info {
    background-color: #f0f7fa;
    border: 1px solid #d1e8f0;
    border-radius: 8px;
    padding: 15px;
    margin: 15px 0;
  }
  
  .result {
    background-color: #e8f5e9;
    color: #2e7d32;
    padding: 10px 15px;
    border-radius: 8px;
    margin-top: 15px;
    font-weight: bold;
  }
  
  .other-key {
    background-color: #e8eaf6;
    padding: 10px 15px;
    border-radius: 8px;
    margin-top: 15px;
  }
  
  /* Back to index button */
  .back-to-index {
    position: fixed;
    bottom: 20px;
    right: 20px;
    background-color: #2874a6;
    color: white;
    padding: 10px 15px;
    border-radius: 30px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.2);
    z-index: 100;
  }
  
  .back-to-index a {
    color: white;
    text-decoration: none;
    font-weight: bold;
  }
  
  /* Responsive design */
  @media (max-width: 768px) {
    body {
      font-size: 16px;
      padding: 10px;
    }
    
    .step-container {
      padding: 15px;
    }
    
    .step-header {
      padding: 12px;
      margin: -15px -15px 15px -15px;
    }
    
    .answer, .step-remark, .extra-info {
      padding: 12px;
    }
  }
  "
  return(styletext)
}

get_clickable_header_style <- function(){
  clickable_header_style <- "
.clickable-header {
    background-color: #ffffff;
    border-radius: 8px;
    padding: 20px;
    margin: 15px 0;
    cursor: pointer;
    display: block;
    color: #2874a6;
    text-decoration: none;
    font-size: 1.2em;
    font-weight: bold;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    transition: all 0.3s ease;
    border-left: 5px solid #2874a6;
}

.clickable-header:hover {
    background-color: #f2f9fc;
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    transform: translateY(-2px);
}

.clickable-header:active {
    background-color: #e6f2f7;
    transform: translateY(1px);
    box-shadow: 0 1px 4px rgba(0,0,0,0.1);
}

@media (max-width: 768px) {
    .clickable-header {
        padding: 15px;
        margin: 12px 0;
    }
}
"
return(clickable_header_style)
}

write_record <- function(obj, fn, obj_header = 2, tit_h_offset = 1) {
  if(is.na(obj$text)) obj$text <- ""
  
  if (obj$type == 'TITLE') {
    h <- as.integer(obj$level) + tit_h_offset
    cat(glue("<h{h}>{obj$text}</h{h}>\n"), file = fn, append = TRUE)
  } else {
    # Start step container
    cat('<div class="step-container">\n', file = fn, append = TRUE)
    
    # Step header
    cat('<div class="step-header">\n', file = fn, append = TRUE)
    cat(glue('<h{obj_header} id="{obj$html_id}"><span class="step-number">Stap {obj$step_number}:</span> {obj$text}</h{obj_header}>\n'), 
        file = fn, append = TRUE)
    
    # Process incoming steps
    incoming <- unlist(obj$incoming_step)
    if(length(incoming) > 0) {
      incoming_links <- paste0("<a href=#S", incoming, '>', incoming, '</a>')
      incoming_text <- paste(incoming_links, collapse = ", ")
      cat(glue('<div class="step-incoming">Komt van stap: {incoming_text}</div>\n'), 
          file = fn, append = TRUE)
    }
    cat('</div>\n', file = fn, append = TRUE)
    
    # Remark if any
    if(!is.na(obj$remark)) {
      cat(glue('<div class="step-remark">{obj$remark}</div>\n'), file = fn, append = TRUE)
    }
    
    # Answers if any
    if (length(obj$answer)) {
      lapply(obj$answer, write_answer, fn = fn, hn = obj_header + 1)
    }
    
    # End step container
    cat('</div>\n', file = fn, append = TRUE)
  }
}

write_answer <- function(x, fn, hn) {
  # Start answer div
  cat('<div class="answer">\n', file = fn, append = TRUE)
  
  # Answer text with link if needed
  next_step <- ifelse(!is.na(x$next_step), glue('#S{x$next_step}'), "")
  text_next_step <- ifelse(is.na(x$next_step), "", paste("(→ stap", x$next_step, ")"))
  
  if(next_step != "") {
    cat(glue('<a href="{next_step}">{x$text} {text_next_step}</a>\n'), file = fn, append = TRUE)
  } else {
    cat(glue('<div class="answer-text">{x$text}</div>\n'), file = fn, append = TRUE)
  }
  
  # Remark if any
  if(!is.na(x$remark)) {
    cat(glue('<div class="answer-remark">{x$remark}</div>\n'), file = fn, append = TRUE)
  }
  
  # Extra info if any
  if(!is.null(x$extra_info) & is.na(x$bwk)) {
    info <- paste(unlist(x$extra_info), collapse = "\n") 
    cat(glue('<div class="extra-info">{info}</div>\n'), file = fn, append = TRUE)
  }
  
  # Results (n2000 and/or bwk) if any
  if(!is.na(x$n2000) | !is.na(x$bwk)) {
    cat('<div class="result">', file = fn, append = TRUE)
    
    if (!is.na(x$n2000) & is.na(x$bwk)) {
      cat(glue('Resultaat: {x$n2000}'), file = fn, append = TRUE)
    } else if (is.na(x$n2000) & !is.na(x$bwk)) {
      cat(glue('BWK: {x$bwk}'), file = fn, append = TRUE)
    } else if (!is.na(x$n2000) & !is.na(x$bwk)) {
      cat(glue('Resultaat: {x$n2000} (BWK: {x$bwk})'), file = fn, append = TRUE)
    }
    
    cat('</div>\n', file = fn, append = TRUE)
  }
  
  # Other key if any
  if(!is.na(x$other_key)) {
    cat(glue('<div class="other-key">Gebruik ook sleutel: <strong>{x$other_key}</strong></div>\n'), 
        file = fn, append = TRUE)
  }
  
  # End answer div
  cat('</div>\n', file = fn, append = TRUE)
}

# Create index.html
create_index_page <- function(sleutels_parsed) {
  fn <- 'index.html'
  
  # Combine styles
  combined_style <- paste(get_default_style(),
                          get_clickable_header_style(),
                          sep = "\n")
  
  # Start the HTML file
  htmlstart(fn, title = 'Veldsleutels', style = combined_style)
  
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