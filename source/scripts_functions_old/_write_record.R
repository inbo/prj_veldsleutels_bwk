write_record <- function(obj, fn, obj_header = 2, tit_h_offset = 1) {
  if (is.na(obj$text)) obj$text <- ""

  if (obj$type == "TITLE") {
    h <- as.integer(obj$level) + tit_h_offset
    cat(glue("<h{h}>{obj$text}</h{h}>\n"), file = fn, append = TRUE)
  } else {
    # Start step container
    cat('<div class="step-container">\n', file = fn, append = TRUE)

    # Step header
    cat('<div class="step-header">\n', file = fn, append = TRUE)
    cat(glue('<h{obj_header} id="{obj$html_id}"><span class="step-number">Stap {obj$step_number}:</span> {obj$text}</h{obj_header}>\n'),
      file = fn, append = TRUE
    )

    # Process incoming steps
    incoming <- unlist(obj$incoming_step)
    if (length(incoming) > 0) {
      incoming_links <- paste0("<a href=#S", incoming, ">", incoming, "</a>")
      incoming_text <- paste(incoming_links, collapse = ", ")
      cat(glue('<div class="step-incoming">Komt van stap: {incoming_text}</div>\n'),
        file = fn, append = TRUE
      )
    }
    cat("</div>\n", file = fn, append = TRUE)

    # Remark if any
    if (!is.na(obj$remark)) {
      cat(glue('<div class="step-remark">{obj$remark}</div>\n'), file = fn, append = TRUE)
    }

    # Answers if any
    if (length(obj$answer)) {
      lapply(obj$answer, write_answer, fn = fn, hn = obj_header + 1)
    }

    # End step container
    cat("</div>\n", file = fn, append = TRUE)
  }
}
