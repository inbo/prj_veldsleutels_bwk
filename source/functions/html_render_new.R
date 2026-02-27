library(yaml)
library(glue)



# 2. CSS Styling (Added .answer-block and refined result-box)
get_style <- function() {
  "
  body { font-family: sans-serif; line-height: 1.6; max-width: 800px; margin: 40px auto; padding: 20px; color: #333; background: #f4f7f6; }
  h1 { color: #2c3e50; border-bottom: 2px solid #2c3e50; }
  .step-container { background: white; padding: 20px; margin-bottom: 40px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-left: 5px solid #27ae60; }
  .step-header { margin-bottom: 10px; display: flex; justify-content: space-between; align-items: baseline; }
  .step-number { font-weight: bold; color: #27ae60; font-size: 1.2em; }
  .incoming-steps { font-size: 0.85em; color: #7f8c8d; }
  .incoming-link { color: #2980b9; text-decoration: none; margin-left: 5px; }

  /* Answer Grouping */
  .answer-block { margin-bottom: 25px; padding-bottom: 10px; }
  .answer-link { display: block; background: #fff; border: 1px solid #ddd; padding: 12px 15px; text-decoration: none; color: #2980b9; border-radius: 4px; transition: 0.2s; font-weight: 500; }
  .answer-link:hover { background: #3498db; color: white; border-color: #3498db; }

  /* Result specific styling */
  .result-box { background: #d4edda; color: #155724; padding: 10px 15px; border-radius: 0 0 4px 4px; font-size: 0.95em; border: 1px solid #c3e6cb; border-top: none; margin-left: 10px; margin-right: 10px; }

  .background-box { background: #eef9f1; padding: 15px; border-radius: 5px; font-size: 0.9em; margin-bottom: 15px; border: 1px solid #c3e6cb; white-space: pre-wrap; }
  .question { font-size: 1.2em; font-weight: bold; margin: 15px 0 20px 0; }
  .back-to-index { display: inline-block; margin: 20px 0; padding: 10px; background: #eee; text-decoration: none; border-radius: 4px; color: #333; }
  .index-btn { display: block; padding: 20px; margin: 10px 0; background: white; border: 1px solid #ddd; border-radius: 8px; font-size: 1.2em; text-decoration: none; color: #2c3e50; font-weight: bold; text-align: center; }
  "
}

# 3. Key Page Generator
write_key_page <- function(key_name, key_data) {
  fn <- paste0(key_name, ".html")
  cat("Generating:", fn, "\n")

  cat('<!DOCTYPE html>\n<html lang="nl">\n<head>\n  <meta charset="UTF-8">\n', file = fn, append = FALSE)
  cat(glue("  <title>{toupper(key_name)} Sleutel</title>\n"), file = fn, append = TRUE)
  cat(glue("  <style>\n{get_style()}\n  </style>\n</head>\n<body>\n"), file = fn, append = TRUE)
  cat(glue("<h1>Sleutel: {toupper(key_name)}</h1>\n"), file = fn, append = TRUE)
  cat('<a href="index.html" class="back-to-index">← Terug naar Index</a><hr>\n', file = fn, append = TRUE)

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

        cat("  </div>\n", file = fn, append = TRUE) # End answer-block
      }
    }
    cat("</div>\n\n", file = fn, append = TRUE)
  }

  cat('<div class="back-to-index"><a href="index.html">Terug naar Index</a></div>\n', file = fn, append = TRUE)
  cat("</body>\n</html>", file = fn, append = TRUE)
}

# 4. Write Index
write_index <- function(keys) {
  fn <- "index.html"
  cat('<!DOCTYPE html>\n<html>\n<head>\n  <meta charset="UTF-8">\n  <style>\n', file = fn, append = FALSE)
  cat(get_style(), file = fn, append = TRUE)
  cat("\n  </style>\n</head>\n<body>\n  <h1>Veldsleutels Index</h1>\n", file = fn, append = TRUE)
  for (k in keys) {
    cat(glue('  <a href="{k}.html" class="index-btn">{toupper(k)}</a>\n'), file = fn, append = TRUE)
  }
  cat("</body>\n</html>", file = fn, append = TRUE)
}

# Alias for compatibility with main.R
write_index_page <- write_index

# # 5. Run
# main_keys <- names(data)
# for (k in main_keys) write_key_page(k, data[[k]])
# write_index(main_keys)
# # 5. Execution
#
# library(yaml)
# library(glue)
#
# # 1. Load your data
# # data <- yaml::yaml.read_file("test.yaml")
#
# data <- veldsleutels
# main_keys <- names(data) # bos, mrs, grl, wtr, hei
# for (k in main_keys) {
#   write_key_page(k, data[[k]])
# }
# write_index_page(main_keys)
