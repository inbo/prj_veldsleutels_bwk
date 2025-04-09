
htmlstart <- function(fn, title, style = NULL) {
  cat(paste0("<!DOCTYPE html>\n<HTML>\n<HEAD>\n<TITLE>", title, "</TITLE>\n"), 
      file = fn, append = FALSE)
  if (!is.null(style)) {
    cat("<style>\n", style, "\n</style>\n", file = fn, append = TRUE)
    #cat('<link rel = "stylesheet" type="text/css" href="', stylesheet, '">\n',
    #    sep = "", file = fn, append = TRUE)
  }
  cat("</HEAD>\n<BODY size=24px>\n", file = fn, append = TRUE)  
}

htmlend <-  function(fn) {
  cat("</body>\n</html>\n",  file = fn, append = TRUE) 
}


htmlwrite <- function(text, fn) {
  cat(text, file = fn, append = TRUE)
}


get_default_style <- function() {
  styletext <- "  
  body {
    background-color: lightblue;
    font-size: 1.5em;
  }
  
  h1 {
    color: navy;
    margin-left: 20px;
  }
  
  table, th, td {
    border: 2px solid;
  }
  
  tr {
    border: 2px solid;
    color: red;
  }
  
  .bordered-text {
    border: 1px solid #000; /* 1px solid black border */
    padding: 10px; /* Optional: Add padding for better appearance */
        }
  "
  styletext
}

write_record <- function(obj, fn, obj_header = 2, tit_h_offset = 1) {
  os <-  obj_header
  oa <- obj_header + 1
  if(is.na(obj$text)) obj$text <- ""
  incoming <- unlist(obj$incoming_step)
  incoming <- paste0("<a href=#S", incoming, '>', incoming, '</a>')
  incoming <- paste(incoming, collapse = ", ")
  if (obj$type == 'TITLE') {
    h <- as.integer(obj$level) + tit_h_offset
    cat(glue("<h{h}>{obj$text}</h{h}>"), file = fn, append = TRUE)
  } else {
    cat(glue('<h{os} id="{obj$html_id}">stap: {obj$step_number} {obj$text} ({incoming}) </h{os}> '), "\n", 
        file = fn, append = TRUE)
    if(!is.na(obj$remark)) {
      cat(glue('<p>note: <i>{obj$remark}</i></p>'), file = fn, append = TRUE)
    }
    if (length(obj$answer)) {
      lapply(obj$answer, write_answer, fn = fn, hn = oa)
    }
    cat("<p style=font-size:1080px></p>", "\n", file = fn, append = TRUE)
  }
}

write_answer <- function(x, fn, hn) {
  next_step <- glue('#S{x$next_step}')
  text_next_step <- ifelse(is.na(x$next_step), "", paste("(-->", x$next_step, ")"))
  cat(glue('<h{hn}> <a href={next_step}>{x$text} {text_next_step}</a> </h{hn}>'), "\n", file = fn, append = TRUE)
  if(!is.na(x$remark)) {
    cat(glue('<p>note: <i>{x$remark}</i></p>'), append = TRUE, file = fn)
  }
  if(!is.null(x$extra_info) & is.na(x$bwk)) {
    info <- paste(unlist(x$extra_info),collapse = "\n") 
    cat('<div class="bordered-text"><p>', info, ',</p></div>', file = fn, append = TRUE)
  }
  if(!is.na(x$n2000) & is.na(x$bwk)) {
    cat(glue('<p><b> --> {x$n2000}</b></p>'), "\n", file = fn, append = TRUE)
  } else if (!is.na(x$bwk) & is.na(x$n2000)) {
    cat(glue('<p><b> --> ( bwk: {x$bwk})</b></p>'), "\n", file = fn, append = TRUE)
  } else if (!is.na(x$n2000) & !is.na(x$bwk)) {
    cat(glue('<p><b> --> {x$n2000} ( bwk: {x$bwk})</b></p>'), "\n", file = fn, append = TRUE)
  } else {
    #print nothing
  }
  if(!is.na(x$other_key)) cat(glue('<p><b>ANDERE SLEUTEL: {x$other_key}</b></p>'), "\n", file = fn, append = TRUE)
}

get_clickable_header_style <- function(){
  # Create custom CSS style for clickable headers
  clickable_header_style <- "
.clickable-header {
    background-color: #f5f5f5;
    border-radius: 8px;
    padding: 15px;
    margin: 15px 0;
    cursor: pointer;
    display: block;
    color: #333;
    text-decoration: none;
    font-size: 1.5em;
    font-weight: bold;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    transition: all 0.3s ease;
}

.clickable-header:hover {
    background-color: #e0e0e0;
    box-shadow: 0 4px 8px rgba(0,0,0,0.15);
    transform: translateY(-2px);
}

.clickable-header:active {
    background-color: #d0d0d0;
    transform: translateY(1px);
    box-shadow: 0 1px 2px rgba(0,0,0,0.1);
}

@media (max-width: 768px) {
    .clickable-header {
        padding: 20px;
        margin: 20px 0;
    }
}
"
  return(clickable_header_style)
}

