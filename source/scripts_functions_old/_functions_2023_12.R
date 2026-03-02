parse_key_data <- function(sheet_data) {
  questions <- sheet_data %>% filter(.data$TYPE == "Q")
  
  # Create a list of records
  records_list <- list()
  current_record <- list()
  for (i in 1:nrow(sheet_data)) {
    type_record <- substring(sheet_data[i, "TYPE"],1,1)
    if (type_record %in% c("Q", "T")) {
      # save previous record
      if (length(current_record)) {
        records_list[[length(records_list) + 1]] <- current_record
      }
      # Start a new record
      current_record <- list()
      if (type_record == "T") {
        current_record$type = "TITLE"
        current_record$text <- unlist(sheet_data[i, "NAME"])
        current_record$level <- substring(unlist(sheet_data[i, "TYPE"]),2,2)
      } else {
        current_record$type = "STEP"
        current_record$step_number <- unlist(sheet_data[i, "STEP"])
        current_record$html_id <- paste0("S", current_record$step_number)
        current_record$incoming_step <-  
          as.list(na.omit(unique(sheet_data$STEP[sheet_data$NEXT_STEP == current_record$step_number])))
        current_record$text <- unlist(sheet_data[i, "NAME"])
        current_record$remark <- unlist(sheet_data[i, "REMARK"])
        current_record$answers <- list()
      }
    } else if (sheet_data[i, "TYPE"] == "A") {
      # Add answer to current record
      answer <- list()
      answer$text <- sheet_data[i, "NAME", drop = TRUE]
      answer$next_step <- sheet_data[i, "NEXT_STEP", drop = TRUE]
      answer$n2000 <- sheet_data[i, "CLASSIFICATION", drop = TRUE]
      answer$bwk <- sheet_data[i, "BWK_CODE", drop = TRUE]
      answer$other_key <- sheet_data[i, "SUBKEY", drop = TRUE]
      answer$remark <- sheet_data[i, "REMARK", drop = TRUE]
      if ((sheet_data[i+1, "TYPE"] == "I") && (i < nrow(sheet_data))) {
        # Add extra information to answer
        answer$extra_info <- unlist(sheet_data[i+1, "NAME"])
      }
      current_record$answers[[length(current_record$answers) + 1]] <- answer
    } else {
      #do nothing
    }
    
    #save on EOF (because other saves are at the beginning of the next step)
    if (i == nrow(sheet_data)) {
      records_list[[length(records_list) + 1]] <- current_record
    }
  }
  records_list
}


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


