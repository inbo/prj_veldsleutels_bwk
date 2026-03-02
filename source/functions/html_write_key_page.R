# 3. Key Page Generator
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

    # Header Section
    cat('  <div class="step-header">\n', file = fn, append = TRUE)
    cat(glue('    <span class="step-number">Stap {step_nr}</span>\n'), file = fn, append = TRUE)

    incoming <- unlist(step$incoming_step)
    incoming <- incoming[!is.na(incoming) & incoming != "" & !is.null(incoming)]

    if (length(incoming) > 0) {
      cat('    <div class="incoming-steps">Komt van stap: ', file = fn, append = TRUE)
      links <- sapply(incoming, function(inc_id) {
        if (!is.null(key_data[[inc_id]])) {
          inc_nr <- key_data[[inc_id]]$step_number
          return(glue('<a href="#step{inc_nr}" class="incoming-link">{inc_nr}</a>'))
        }
        return(NULL)
      })
      links <- Filter(Negate(is.null), links)
      cat(paste(links, collapse = ", "), file = fn, append = TRUE)
      cat("</div>\n", file = fn, append = TRUE)
    }
    cat("  </div>\n", file = fn, append = TRUE)

    # Titles and content
    if (!is.null(step$h2$name)) cat(glue("  <h2>{step$h2$name}</h2>\n"), file = fn, append = TRUE)
    if (!is.null(step$h3$name)) cat(glue("  <h3>{step$h3$name}</h3>\n"), file = fn, append = TRUE)
    if (!is.null(step$background$name)) {
      cat(glue('  <div class="background-box"><strong>Achtergrond:</strong><br>{step$background$name}</div>\n'), file = fn, append = TRUE)
    }
    if (!is.null(step$question$name) && step$question$name != "") {
      cat(glue('  <div class="question">{step$question$name}</div>\n'), file = fn, append = TRUE)
    }

    # Answers Loop
    if (!is.null(step$answer)) {
      for (ans in step$answer) {
        cat('  <div class="answer-block">\n', file = fn, append = TRUE)

        ns_val <- ans$nextstep
        target <- NULL
        label <- ans$name

        if (is.null(ns_val) || is.na(ns_val) || ns_val == ".na.character") {
          target <- NULL
        } else if (ns_val == "SLEUTEL") {
          target <- "index.html"
          label <- paste(ans$name, "(→ Hoofdsleutel)")
        } else {
          target <- paste0("#step", ns_val)
          label <- paste(ans$name, glue("(→ Stap {ns_val})"))
        }

        if (!is.null(target)) {
          cat(glue('    <a href="{target}" class="answer-link">{label}</a>\n'), file = fn, append = TRUE)
        } else {
          cat(glue('    <div class="answer-link" style="color:#333; background:#fafafa; cursor:default; border-bottom: 2px solid #27ae60;">{label}</div>\n'), file = fn, append = TRUE)
        }

        # Result Details
        n2000 <- if (!is.null(ans$n2000)) ans$n2000 else ""
        bwk <- if (!is.null(ans$bwk)) ans$bwk else ""
        if (n2000 != "" || bwk != "") {
          cat('    <div class="result-box">\n', file = fn, append = TRUE)
          if (n2000 != "") cat(glue("      <strong>Natura 2000:</strong> {n2000}<br>\n"), file = fn, append = TRUE)
          if (bwk != "") cat(glue("      <strong>BWK:</strong> {bwk}\n"), file = fn, append = TRUE)
          cat("    </div>\n", file = fn, append = TRUE)
        }

        # Extra Info (from I type rows)
        info <- if (!is.null(ans$info)) ans$info else ""
        if (info != "" && !is.na(info)) {
          cat(glue('    <div class="extra-info">{info}</div>\n'), file = fn, append = TRUE)
        }

        # Answer Remark
        remark <- if (!is.null(ans$remark)) ans$remark else ""
        if (remark != "" && !is.na(remark)) {
          cat(glue('    <div class="answer-remark">{remark}</div>\n'), file = fn, append = TRUE)
        }

        # Other Key reference
        otherkey <- if (!is.null(ans$otherkey)) ans$otherkey else ""
        if (otherkey != "" && !is.na(otherkey)) {
          cat(glue('    <div class="other-key">Gebruik ook sleutel: <strong>{otherkey}</strong></div>\n'), file = fn, append = TRUE)
        }

        cat("  </div>\n", file = fn, append = TRUE) # End answer-block
      }
    }
    cat("</div>\n\n", file = fn, append = TRUE)
  }

  cat('<div class="back-to-index"><a href="../index.html">Terug naar Index</a></div>\n', file = fn, append = TRUE)
  cat("</body>\n</html>", file = fn, append = TRUE)
}
