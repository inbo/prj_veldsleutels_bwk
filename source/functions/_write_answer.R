write_answer <- function(x, fn, hn) {
  # Start answer div
  cat('<div class="answer">\n', file = fn, append = TRUE)

  # Answer text with link if needed
  next_step <- ifelse(!is.na(x$next_step), glue("#S{x$next_step}"), "")
  text_next_step <- ifelse(is.na(x$next_step), "", paste("(→ stap", x$next_step, ")"))

  if (next_step != "") {
    cat(glue('<a href="{next_step}">{x$text} {text_next_step}</a>\n'), file = fn, append = TRUE)
  } else {
    cat(glue('<div class="answer-text">{x$text}</div>\n'), file = fn, append = TRUE)
  }

  # Remark if any
  if (!is.na(x$remark)) {
    cat(glue('<div class="answer-remark">{x$remark}</div>\n'), file = fn, append = TRUE)
  }

  # Extra info if any
  if (!is.null(x$extra_info) & is.na(x$bwk)) {
    info <- paste(unlist(x$extra_info), collapse = "\n")
    cat(glue('<div class="extra-info">{info}</div>\n'), file = fn, append = TRUE)
  }

  # Results (n2000 and/or bwk) if any
  if (!is.na(x$n2000) | !is.na(x$bwk)) {
    cat('<div class="result">', file = fn, append = TRUE)

    if (!is.na(x$n2000) & is.na(x$bwk)) {
      cat(glue("Resultaat: {x$n2000}"), file = fn, append = TRUE)
    } else if (is.na(x$n2000) & !is.na(x$bwk)) {
      cat(glue("BWK: {x$bwk}"), file = fn, append = TRUE)
    } else if (!is.na(x$n2000) & !is.na(x$bwk)) {
      cat(glue("Resultaat: {x$n2000} (BWK: {x$bwk})"), file = fn, append = TRUE)
    }

    cat("</div>\n", file = fn, append = TRUE)
  }

  # Other key if any
  if (!is.na(x$other_key)) {
    cat(glue('<div class="other-key">Gebruik ook sleutel: <strong>{x$other_key}</strong></div>\n'),
      file = fn, append = TRUE
    )
  }

  # End answer div
  cat("</div>\n", file = fn, append = TRUE)
}
