#' Create Individual Quarto PDF Documents with Flandersqmd Style
#'
#' Generates separate Quarto (.qmd) documents for each field key using the
#' INBO flandersqmd style. Each document can be rendered to a professionally
#' styled PDF following Flemish government corporate identity guidelines.
#'
#' @param data A named list of key groups (e.g., bos, grasland, heide, moeras,
#'   water), where each contains parsed step data with questions, answers, and
#'   results.
#' @param output_dir Character string specifying the output directory for the
#'   .qmd files. Default is "pdf_output".
#' @param author_name Character string with the author name. Default is
#'   "Research Institute for Nature and Forest (INBO)".
#' @param author_email Character string with the author email. Default is
#'   "info@inbo.be".
#' @param use_flandersqmd Logical indicating whether to use flandersqmd styling.
#'   If TRUE (default), generates Quarto documents configured for flandersqmd-book.
#'   If FALSE, generates simple Quarto PDF documents.
#'
#' @return Invisible NULL. The function writes Quarto .qmd files to disk.
#'
#' @details
#' For each key in the data, the function creates a separate .qmd file with:
#' \itemize{
#'   \item YAML header configured for flandersqmd-book-pdf format (if use_flandersqmd = TRUE)
#'   \item Professional title page with key name
#'   \item Table of contents
#'   \item Structured sections for each step
#'   \item Questions and answers with visual formatting
#'   \item Result codes (Natura 2000 and BWK classifications)
#'   \item Cross-references between steps
#'   \item Callout boxes for background information
#' }
#'
#' When \code{use_flandersqmd = TRUE}, the YAML header is configured for the
#' flandersqmd-book extension with INBO corporate identity. This requires the
#' flandersqmd-book extension to be installed via:
#' \code{quarto install extension inbo/flandersqmd-book}
#'
#' When \code{use_flandersqmd = FALSE}, standard Quarto PDF output is used.
#'
#' @examples
#' \dontrun{
#' # Create Quarto documents with flandersqmd style
#' create_quarto_pdf(veldsleutels, output_dir = "pdf_output")
#'
#' # Then render to PDF using Quarto
#' system("quarto render pdf_output/bos.qmd")
#'
#' # Or render all keys
#' for (key in names(veldsleutels)) {
#'   system(sprintf("quarto render pdf_output/%s.qmd", key))
#' }
#'
#' # Create simple Quarto documents without flandersqmd
#' create_quarto_pdf(veldsleutels, use_flandersqmd = FALSE)
#' }
#'
#' @seealso \code{\link{create_pdf_markdown}}, \code{\link{parse_data}}
create_quarto_pdf <- function(data, 
                               output_dir = "pdf_output",
                               author_name = "Research Institute for Nature and Forest (INBO)",
                               author_email = "info@inbo.be",
                               use_flandersqmd = TRUE) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get all keys
  main_keys <- names(data)
  
  # Map key abbreviations to full Dutch names
  key_names <- list(
    bos = "Bossleutel",
    grl = "Graslandsleutel",
    hei = "Heidesleutel",
    mrs = "Moerassleutel",
    wtr = "Watersleutel"
  )
  
  # Generate a document for each key
  for (k in main_keys) {
    qmd_fn <- file.path(output_dir, paste0(k, ".qmd"))
    cat("Generating:", qmd_fn, "\n")
    
    key_data <- data[[k]]
    key_title <- if (!is.null(key_names[[k]])) key_names[[k]] else toupper(k)
    
    # Write YAML Header
    if (use_flandersqmd) {
      # Flandersqmd-style YAML header
      cat(glue('---
title: "{key_title}"
subtitle: "Veldsleutel voor habitatclassificatie BWK & Natura 2000"
format:
  flandersqmd-book-pdf:
    toc: true
    toc-depth: 2
    number-sections: false
flandersqmd:
  entity: INBO
  level: 2
  author:
    - name:
        given: Research Institute for Nature and Forest
        family: (INBO)
      email: {author_email}
      corresponding: true
      affiliation:
        - Research Institute for Nature and Forest (INBO)
  year: {format(Sys.Date(), "%Y")}
  public_report: false
  colophon: false
---

'), file = qmd_fn, append = FALSE)
    } else {
      # Standard Quarto PDF YAML header
      cat(glue('---
title: "{key_title}"
subtitle: "Veldsleutel voor habitatclassificatie BWK & Natura 2000"
author: "{author_name}"
date: "`r Sys.Date()`"
format:
  pdf:
    toc: true
    toc-depth: 2
    number-sections: false
    papersize: a4
    documentclass: article
---

'), file = qmd_fn, append = FALSE)
    }
    
    # Introduction section
    cat(glue('
# Inleiding {{-}}

Deze veldsleutel helpt bij het identificeren en classificeren van {tolower(key_title)} volgens de Biologische Waarderingskaart (BWK) en Natura 2000 habitattypen.

Gebruik de sleutel door stap voor stap de vragen te beantwoorden en de aanwijzingen te volgen.

'), file = qmd_fn, append = TRUE)
    
    # Write each step
    for (id in names(key_data)) {
      step <- key_data[[id]]
      step_nr <- step$step_number
      
      # Step Header with anchor
      cat(glue("\n# Stap {step_nr} {{#step-{step_nr}}}\n\n"), file = qmd_fn, append = TRUE)
      
      # Titles from H2/H3
      if (!is.null(step$h2$name) && nzchar(step$h2$name)) {
        cat(glue("**{step$h2$name}**\n\n"), file = qmd_fn, append = TRUE)
      }
      if (!is.null(step$h3$name) && nzchar(step$h3$name)) {
        cat(glue("*{step$h3$name}*\n\n"), file = qmd_fn, append = TRUE)
      }
      
      # Background Box using Quarto callout
      if (!is.null(step$background$name) && nzchar(step$background$name)) {
        cat(glue("
::: {{.callout-note}}
## Achtergrond

{step$background$name}
:::

"), file = qmd_fn, append = TRUE)
      }
      
      # Question
      if (!is.null(step$question$name) && nzchar(step$question$name)) {
        cat(glue("## {step$question$name}\n\n"), file = qmd_fn, append = TRUE)
      }
      
      # Answers
      if (!is.null(step$answer)) {
        for (i in seq_along(step$answer)) {
          ans <- step$answer[[i]]
          ns_val <- ans$nextstep
          
          # Determine if this is a terminal answer
          is_terminal <- is.null(ns_val) || is.na(ns_val) || ns_val == ".na.character"
          
          # Format the answer with navigation
          if (is_terminal) {
            cat(glue("**{i}. {ans$name}**\n\n"), file = qmd_fn, append = TRUE)
          } else if (ns_val == "SLEUTEL" || ns_val == "HOOFDSLEUTEL") {
            cat(glue("**{i}. {ans$name}** → Ga terug naar hoofdsleutel\n\n"), file = qmd_fn, append = TRUE)
          } else {
            cat(glue("**{i}. {ans$name}** → Ga naar [Stap {ns_val}](#step-{ns_val})\n\n"), file = qmd_fn, append = TRUE)
          }
          
          # Additional info
          if (!is.null(ans$info) && nzchar(ans$info)) {
            cat(glue("   *Info:* {ans$info}\n\n"), file = qmd_fn, append = TRUE)
          }
          
          # Remarks
          if (!is.null(ans$remark) && nzchar(ans$remark)) {
            cat(glue("   *Opmerking:* {ans$remark}\n\n"), file = qmd_fn, append = TRUE)
          }
          
          # Other key reference
          if (!is.null(ans$otherkey) && nzchar(ans$otherkey)) {
            cat(glue("   *Andere sleutel:* {ans$otherkey}\n\n"), file = qmd_fn, append = TRUE)
          }
          
          # Results in a callout box
          n2000 <- if (!is.null(ans$n2000) && nzchar(ans$n2000)) ans$n2000 else ""
          bwk <- if (!is.null(ans$bwk) && nzchar(ans$bwk)) ans$bwk else ""
          
          if (nzchar(n2000) || nzchar(bwk)) {
            cat("   ::: {.callout-important}\n", file = qmd_fn, append = TRUE)
            cat("   ## Classificatie\n\n", file = qmd_fn, append = TRUE)
            if (nzchar(n2000)) {
              cat(glue("   **Natura 2000:** {n2000}\n\n"), file = qmd_fn, append = TRUE)
            }
            if (nzchar(bwk)) {
              cat(glue("   **BWK Code:** {bwk}\n\n"), file = qmd_fn, append = TRUE)
            }
            cat("   :::\n\n", file = qmd_fn, append = TRUE)
          }
        }
      }
      
      cat("---\n\n", file = qmd_fn, append = TRUE)
    }
    
    cat(glue("\nVeldsleutel **{key_title}** gegenereerd op {Sys.Date()}.\n"), 
        file = qmd_fn, append = TRUE)
  }
  
  # Create a README with render instructions
  readme_fn <- file.path(output_dir, "README.md")
  cat(glue('# Veldsleutels PDF Output

Dit directory bevat Quarto documenten voor elke veldsleutel.

## Quarto installeren

Als je Quarto nog niet hebt geïnstalleerd:

```bash
# Download van https://quarto.org/docs/get-started/
```

## Flandersqmd extensie installeren (optioneel)

Voor INBO corporate identity styling:

```bash
quarto install extension inbo/flandersqmd-book
```

## PDFs genereren

### Individuele sleutel:

```bash
quarto render {main_keys[1]}.qmd
```

### Alle sleutels:

```bash
quarto render
```

## Gegenereerde bestanden

'), file = readme_fn, append = FALSE)
  
  for (k in main_keys) {
    key_title <- if (!is.null(key_names[[k]])) key_names[[k]] else toupper(k)
    cat(glue("- `{k}.qmd` - {key_title}\n"), file = readme_fn, append = TRUE)
  }
  
  cat(glue("\n\nDe PDF bestanden worden aangemaakt in de `{output_dir}` directory.\n"), 
      file = readme_fn, append = TRUE)
  
  invisible(NULL)
}
