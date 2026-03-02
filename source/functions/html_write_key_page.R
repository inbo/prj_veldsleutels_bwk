html_write_key_page <- function(key_name, key_data, folder = "") {
  fn <- file.path(folder, paste0(key_name, ".html"))
  cat("Generating:", fn, "\n")

  cat('<!DOCTYPE html>\n<html lang="nl">\n<head>\n', file = fn, append = FALSE)
  cat('  <meta charset="UTF-8">\n', file = fn, append = TRUE)
  cat('  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">\n', file = fn, append = TRUE)
  cat(glue("  <title>{toupper(key_name)} Sleutel</title>\n"), file = fn, append = TRUE)
  cat(glue("  <style>\n{html_get_style()}\n  </style>\n</head>\n<body>\n"), file = fn, append = TRUE)
  cat(glue("<h1>Sleutel: {toupper(key_name)}</h1>\n"), file = fn, append = TRUE)
  cat('<div style="margin-bottom:30px;"><a href="../index.html" class="back-to-index">← NAAR INDEX</a></div>\n', file = fn, append = TRUE)

  for (id in names(key_data)) {
    step <- key_data[[id]]
    step_nr <- step$step_number

    cat(glue('<div class="step-container" id="step{step_nr}">\n'), file = fn, append = TRUE)

    # Header section
    cat('  <div class="step-header">\n', file = fn, append = TRUE)
    cat(glue('    <span class="step-number">STAP {step_nr}</span>\n'), file = fn, append = TRUE)

    incoming <- unlist(step$incoming_step)
    incoming <- incoming[!is.na(incoming) & incoming != "" & !is.null(incoming)]
    if (length(incoming) > 0) {
      cat('    <div class="incoming-steps">Van: ', file = fn, append = TRUE)
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
      cat(glue('  <div class="background-box"><strong>INFO:</strong><br>{step$background$name}</div>\n'), file = fn, append = TRUE)
    }
    if (!is.null(step$question$name) && nzchar(step$question$name)) {
      cat(glue('  <div class="question">{step$question$name}</div>\n'), file = fn, append = TRUE)
    }

    # Answers loop
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
          label <- paste(label, "(→ Hoofdsleutel)")
        } else {
          target <- paste0("#step", ns_val)
          label <- paste(label, glue("(→ {ns_val})"))
        }

        has_extra_info <- nzchar(ans$info) || nzchar(ans$remark) || nzchar(ans$otherkey)
        l_class <- if (has_extra_info) "answer-link has-details" else "answer-link"
        t_class <- if (has_extra_info) "answer-link-terminal has-details" else "answer-link-terminal"

        if (!is.null(target)) {
          cat(glue('    <a href="{target}" class="{l_class}">{label}</a>\n'), file = fn, append = TRUE)
        } else {
          cat(glue('    <div class="{t_class}">{label}</div>\n'), file = fn, append = TRUE)
        }

        if (has_extra_info) {
          cat('    <div class="answer-details-group">\n', file = fn, append = TRUE)
          if (nzchar(ans$info)) cat(glue('      <div class="extra-info">{ans$info}</div>\n'), file = fn, append = TRUE)
          if (nzchar(ans$remark)) cat(glue('      <div class="answer-remark">{ans$remark}</div>\n'), file = fn, append = TRUE)
          if (nzchar(ans$otherkey)) cat(glue('      <div class="other-key">Sleutel: <strong>{ans$otherkey}</strong></div>\n'), file = fn, append = TRUE)
          cat("    </div>\n", file = fn, append = TRUE)
        }

        if (is_terminal && (nzchar(ans$n2000) || nzchar(ans$bwk))) {
          cat('    <div class="result-box">\n', file = fn, append = TRUE)
          if (nzchar(ans$n2000)) cat(glue("      <strong>NATURA 2000:</strong> {ans$n2000}<br>\n"), file = fn, append = TRUE)
          if (nzchar(ans$bwk)) cat(glue("      <strong>BWK CODE:</strong> {ans$bwk}\n"), file = fn, append = TRUE)
          cat("    </div>\n", file = fn, append = TRUE)
        }
        cat("  </div>\n", file = fn, append = TRUE)
      }
    }
    cat("</div>\n\n", file = fn, append = TRUE)
  }
  cat('<div style="text-align:center; margin-top: 50px;"><a href="../index.html" class="back-to-index">TERUG NAAR INDEX</a></div>\n</body>\n</html>', file = fn, append = TRUE)
}
