html_write_key_page <- function(key_name, key_data, folder = "") {
  fn <- file.path(folder, paste0(key_name, ".html"))
  cat("Generating:", fn, "\n")

  cat('<!DOCTYPE html>\n<html lang="nl">\n<head>\n  <meta charset="UTF-8">\n', file = fn, append = FALSE)
  cat(glue("  <title>{toupper(key_name)} Sleutel</title>\n"), file = fn, append = TRUE)
  cat(glue("  <style>\n{html_get_style()}\n  </style>\n</head>\n<body>\n"), file = fn, append = TRUE)
  cat(glue("<h1>Sleutel: {toupper(key_name)}</h1>\n"), file = fn, append = TRUE)
  cat('<a href="../index.html" class="back-to-index">← Terug naar Index</a><hr>\n', file = fn, append = TRUE)

  for (id in names(key_data)) {
    step <- key_data[[id]]
    step_nr <- step$step_number

    cat(glue('<div class="step-container" id="step{step_nr}">\n'), file = fn, append = TRUE)

    # Header section
    cat('  <div class="step-header">\n', file = fn, append = TRUE)
    cat(glue('    <span class="step-number">Stap {step_nr}</span>\n'), file = fn, append = TRUE)

    incoming <- unlist(step$incoming_step)
    incoming <- incoming[!is.na(incoming) & incoming != "" & !is.null(incoming)]
    if (length(incoming) > 0) {
      cat('    <div class="incoming-steps">Komt van: ', file = fn, append = TRUE)
      links <- sapply(incoming, function(inc_id) {
        if (!is.null(key_data[[inc_id]])) {
          inc_nr <- key_data[[inc_id]]$step_number
          return(glue('<a href="#step{inc_nr}" class="incoming-link">{inc_nr}</a>'))
        }
        return(NULL)
      })
      cat(paste(Filter(Negate(is.null), links), collapse = ", "), file = fn, append = TRUE)
      cat("</div>\n", file = fn, append = TRUE)
    }
    cat("  </div>\n", file = fn, append = TRUE)

    # h2, h3, background, question
    if (!is.null(step$h2$name) && nzchar(step$h2$name)) cat(glue("  <h2>{step$h2$name}</h2>\n"), file = fn, append = TRUE)
    if (!is.null(step$h3$name) && nzchar(step$h3$name)) cat(glue("  <h3>{step$h3$name}</h3>\n"), file = fn, append = TRUE)
    if (!is.null(step$background$name) && nzchar(step$background$name)) {
      cat(glue('  <div class="background-box"><strong>Achtergrond:</strong><br>{step$background$name}</div>\n'), file = fn, append = TRUE)
    }
    if (!is.null(step$question$name) && nzchar(step$question$name)) {
      cat(glue('  <div class="question">{step$question$name}</div>\n'), file = fn, append = TRUE)
    }

    # Answers Loop
    if (!is.null(step$answer)) {
      for (ans in step$answer) {
        cat('  <div class="answer-block">\n', file = fn, append = TRUE)

        ns_val <- ans$nextstep
        is_terminal <- is.null(ns_val) || is.na(ns_val) || ns_val %in% c("", ".na.character")

        target <- NULL
        label <- ans$name

        if (is_terminal) {
          target <- NULL
        } else if (ns_val %in% c("SLEUTEL", "HOOFDSLEUTEL")) {
          target <- "../index.html"
          label <- paste(label, "(→ Index)")
        } else {
          target <- paste0("#step", ns_val)
          label <- paste(label, glue("(→ Stap {ns_val})"))
        }

        # Check for Info/Remarks/OtherKey (these group with the answer)
        has_extra_info <- nzchar(ans$info) || nzchar(ans$remark) || nzchar(ans$otherkey)

        # Determine classes
        link_class <- if (has_extra_info) "answer-link has-details" else "answer-link"
        term_class <- if (has_extra_info) "answer-link-terminal has-details" else "answer-link-terminal"

        if (!is.null(target)) {
          cat(glue('    <a href="{target}" class="{link_class}">{label}</a>\n'), file = fn, append = TRUE)
        } else {
          cat(glue('    <div class="{term_class}">{label}</div>\n'), file = fn, append = TRUE)
        }

        # Group Info / Remark / OtherKey
        if (has_extra_info) {
          cat('    <div class="answer-details-group">\n', file = fn, append = TRUE)
          if (nzchar(ans$info)) cat(glue('      <div class="extra-info">{ans$info}</div>\n'), file = fn, append = TRUE)
          if (nzchar(ans$remark)) cat(glue('      <div class="answer-remark">{ans$remark}</div>\n'), file = fn, append = TRUE)
          if (nzchar(ans$otherkey)) cat(glue('      <div class="other-key">Gebruik sleutel: <strong>{ans$otherkey}</strong></div>\n'), file = fn, append = TRUE)
          cat("    </div>\n", file = fn, append = TRUE)
        }

        # ONLY show Result Box if terminal AND has data
        if (is_terminal) {
          has_res_data <- nzchar(ans$n2000) || nzchar(ans$bwk)
          if (has_res_data) {
            cat('    <div class="result-box">\n', file = fn, append = TRUE)
            if (nzchar(ans$n2000)) cat(glue("      <strong>Natura 2000:</strong> {ans$n2000}<br>\n"), file = fn, append = TRUE)
            if (nzchar(ans$bwk)) cat(glue("      <strong>BWK:</strong> {ans$bwk}\n"), file = fn, append = TRUE)
            cat("    </div>\n", file = fn, append = TRUE)
          }
        }

        cat("  </div>\n", file = fn, append = TRUE)
      }
    }
    cat("</div>\n\n", file = fn, append = TRUE)
  }

  cat('<div class="back-to-index"><a href="../index.html">Terug naar Index</a></div>\n', file = fn, append = TRUE)
  cat("</body>\n</html>", file = fn, append = TRUE)
}
