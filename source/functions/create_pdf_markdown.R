library(yaml)
library(glue)


create_pdf_markdown <- function(data, rmdfile) {
  # 1. Load data
  data <- data

  # 2. Output Rmd file name
  rmd_fn <- rmdfile

  # 3. Write Rmd Header (YAML Metadata for PDF)
  cat('---
title: "Veldsleutels BWK & Natura 2000"
author: "Generatie Document"
date: "`r Sys.Date()`"
output:
  pdf_document:
    latex_engine: xelatex
    toc: true
    toc_depth: 2
    number_sections: false
---

\\newpage

', file = rmd_fn, append = FALSE)

  # 4. Loop through main keys (bos, mrs, grl, etc.)
  main_keys <- names(data)

  for (k in main_keys) {
    # Write Section Title for the Key
    cat(glue("\n# Sleutel: {toupper(k)} \n\n"), file = rmd_fn, append = TRUE)

    key_data <- data[[k]]

    for (id in names(key_data)) {
      step <- key_data[[id]]
      step_nr <- step$step_number

      # Step Anchor and Header
      cat(glue("## Stap {step_nr} {{#step-{k}-{step_nr}}} \n\n"), file = rmd_fn, append = TRUE)

      # Titles from H2/H3
      if (!is.null(step$h2$name)) cat(glue("**{step$h2$name}**\n\n"), file = rmd_fn, append = TRUE)
      if (!is.null(step$h3$name)) cat(glue("*{step$h3$name}*\n\n"), file = rmd_fn, append = TRUE)

      # Background Box (Using a Quote block for visual distinction in PDF)
      if (!is.null(step$background$name)) {
        cat(glue("> **Achtergrond:** {step$background$name}\n\n"), file = rmd_fn, append = TRUE)
      }

      # Question
      if (!is.null(step$question$name) && step$question$name != "") {
        cat(glue("### {step$question$name}\n\n"), file = rmd_fn, append = TRUE)
      }

      # Answers Loop
      if (!is.null(step$answer)) {
        for (ans in step$answer) {
          ns_val <- ans$nextstep

          # Format the Answer line
          if (is.null(ns_val) || is.na(ns_val) || ns_val == ".na.character") {
            cat(glue("* **{ans$name}**\n"), file = rmd_fn, append = TRUE)
          } else if (ns_val == "SLEUTEL") {
            cat(glue("* **{ans$name}** (Ga naar Hoofdsleutel)\n"), file = rmd_fn, append = TRUE)
          } else {
            # PDF internal link format: [Text](#anchor)
            cat(glue("* **{ans$name}** (Ga naar [Stap {ns_val}](#step-{k}-{ns_val}))\n"), file = rmd_fn, append = TRUE)
          }

          # Results (Indented)
          n2000 <- if (!is.null(ans$n2000)) ans$n2000 else ""
          bwk <- if (!is.null(ans$bwk)) ans$bwk else ""
          if (n2000 != "" || bwk != "") {
            cat(glue("    - *Resultaat:* {n2000} | *BWK:* {bwk}\n"), file = rmd_fn, append = TRUE)
          }
        }
        cat("\n", file = rmd_fn, append = TRUE)
      }
      cat("---\n\n", file = rmd_fn, append = TRUE) # Horizontal rule between steps
    }
    cat("\\newpage\n", file = rmd_fn, append = TRUE) # Start new key on new page
  }
}

# 5. Finalize and Render
# rmarkdown::render(rmd_fn)
