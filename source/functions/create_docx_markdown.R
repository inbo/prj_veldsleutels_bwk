library(glue)

# Creates one DOCX file per indicator (bos, grl, hei, mrs, wtr).
# Each DOCX is rendered from an intermediate Rmd via rmarkdown/pandoc,
# using pandoc custom-style fenced divs to apply the required Word styles.
# A reference DOCX with the custom paragraph styles is generated once and
# stored in data/reference_veldsleutels.docx.
#
# Styling applied:
#   T1  (h2)        – font 16, green pastel background (#B8DFB8)
#   T2  (h3)        – font 14, bold, underlined
#   Q   (question)  – plain text followed by a blank line
#   A   (answer)    – plain text + arrow to next step (→ step_nr)
#   I   (info)      – bullet points (one per original newline)
#   BI  (background)– box with a light-grey background (#F0F0F0)
#   Result          – light-blue bullet point (after I fields)

# Word stores font sizes in half-points (1 pt = 2 half-points)
HALF_POINTS_PER_PT <- 2L

create_docx_markdown <- function(data, output_dir = ".") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ref_docx_path <- file.path("data", "reference_veldsleutels.docx")
  if (!file.exists(ref_docx_path)) {
    create_docx_reference_template(ref_docx_path)
  }

  main_keys <- names(data)

  for (k in main_keys) {
    rmd_fn  <- file.path(tempdir(), paste0("veldsleutel_", k, ".Rmd"))
    docx_fn <- normalizePath(
      file.path(output_dir, paste0("veldsleutel_", k, ".docx")),
      mustWork = FALSE
    )

    write_indicator_rmd(k, data[[k]], rmd_fn, ref_docx_path)

    rmarkdown::render(
      input       = rmd_fn,
      output_file = docx_fn,
      quiet       = TRUE
    )

    message(glue("Veldsleutel DOCX aangemaakt: {docx_fn}"))
  }
}

# ---------------------------------------------------------------------------
# write_indicator_rmd
# Writes a pandoc-flavoured Rmd file for one indicator.
# ---------------------------------------------------------------------------
write_indicator_rmd <- function(key_name, key_data, rmd_fn, ref_docx_path) {
  # Normalise the reference path and use forward slashes for YAML
  ref_path_yaml <- gsub("\\\\", "/",
    normalizePath(ref_docx_path, mustWork = FALSE))

  # YAML header
  yaml_header <- glue(
    '---\n',
    'title: "Veldsleutel {toupper(key_name)}"\n',
    'output:\n',
    '  word_document:\n',
    '    reference_docx: "{ref_path_yaml}"\n',
    '---\n\n'
  )
  cat(yaml_header, file = rmd_fn, append = FALSE)

  for (id in names(key_data)) {
    step <- key_data[[id]]

    # T1 (h2): font 16, green pastel background
    if (!is.null(step$h2$name) && nzchar(step$h2$name)) {
      cat(
        '::: {custom-style="T1Title"}\n',
        step$h2$name, "\n",
        ":::\n\n",
        sep = "", file = rmd_fn, append = TRUE
      )
    }

    # T2 (h3): font 14, bold, underlined
    if (!is.null(step$h3$name) && nzchar(step$h3$name)) {
      cat(
        '::: {custom-style="T2Title"}\n',
        step$h3$name, "\n",
        ":::\n\n",
        sep = "", file = rmd_fn, append = TRUE
      )
    }

    # BI: box with a slight colour background
    if (!is.null(step$background$name) && nzchar(step$background$name)) {
      cat(
        '::: {custom-style="BackgroundBox"}\n',
        step$background$name, "\n",
        ":::\n\n",
        sep = "", file = rmd_fn, append = TRUE
      )
    }

    # Q: plain text followed by a blank line
    if (!is.null(step$question$name) && nzchar(step$question$name)) {
      cat(step$question$name, "\n\n", sep = "", file = rmd_fn, append = TRUE)
    }

    # Answers
    if (!is.null(step$answer)) {
      for (ans in step$answer) {
        ns_val      <- ans$nextstep
        # ".na.character" is the string that yaml::as.yaml() writes when the
        # original R value was NA; we treat it the same as a missing next step.
        is_terminal <- is.null(ns_val) || is.na(ns_val) ||
          ns_val %in% c("", ".na.character")

        # A: answer text with arrow notation (→ next step)
        ans_display <- if (is_terminal) {
          ans$name
        } else if (ns_val %in% c("SLEUTEL", "HOOFDSLEUTEL")) {
          paste0(ans$name, " \u2192 Hoofdsleutel")
        } else {
          paste0(ans$name, " \u2192 ", ns_val)
        }

        cat(ans_display, "\n\n", sep = "", file = rmd_fn, append = TRUE)

        # I: info as bullet points (one bullet per original line)
        if (!is.null(ans$info) && nzchar(ans$info)) {
          info_lines <- strsplit(ans$info, "\n")[[1]]
          for (line in info_lines[nzchar(info_lines)]) {
            cat("- ", line, "\n", sep = "", file = rmd_fn, append = TRUE)
          }
          cat("\n", file = rmd_fn, append = TRUE)
        }

        # Result: light-blue bullet point (after I fields, if a classification
        # or BWK code is present)
        n2000 <- if (!is.null(ans$n2000)) ans$n2000 else ""
        bwk   <- if (!is.null(ans$bwk))   ans$bwk   else ""
        if (nzchar(n2000) || nzchar(bwk)) {
          result_parts <- Filter(nzchar, c(
            if (nzchar(n2000)) paste0("Natura 2000: ", n2000),
            if (nzchar(bwk))   paste0("BWK: ", bwk)
          ))
          result_str <- paste(result_parts, collapse = " | ")
          cat(
            '::: {custom-style="AnswerResult"}\n',
            "- ", result_str, "\n",
            ":::\n\n",
            sep = "", file = rmd_fn, append = TRUE
          )
        }
      }
    }

    # Horizontal rule between steps
    cat("---\n\n", file = rmd_fn, append = TRUE)
  }
}

# ---------------------------------------------------------------------------
# create_docx_reference_template
# Creates a Word reference document that defines the four custom paragraph
# styles used by the indicator Rmd files.
# Requires: officer, xml2, zip
# ---------------------------------------------------------------------------
create_docx_reference_template <- function(path) {
  for (pkg in c("officer", "xml2", "zip")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(glue(
        "Package '{pkg}' is required to create the reference DOCX template. ",
        "Install it with: install.packages('{pkg}')"
      ))
    }
  }

  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)

  # ---- 1. Generate a minimal base DOCX with officer ----
  base_doc <- officer::read_docx()
  tmp_docx <- tempfile(fileext = ".docx")
  print(base_doc, target = tmp_docx)

  # ---- 2. Unzip and modify styles.xml ----
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  unzip(tmp_docx, exdir = tmp_dir)

  styles_xml_path <- file.path(tmp_dir, "word", "styles.xml")
  styles_doc <- xml2::read_xml(styles_xml_path)

  # Helper: build the XML string for one custom paragraph style
  make_style_xml <- function(style_id, style_name,
                             font_size_pt = 12,
                             bold       = FALSE,
                             underline  = FALSE,
                             text_color = "000000",
                             bg_color   = NULL,
                             has_border = FALSE) {
    sz       <- font_size_pt * HALF_POINTS_PER_PT
    ns_uri   <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    bold_xml <- if (bold)      "<w:b/><w:bCs/>"            else ""
    ul_xml   <- if (underline) '<w:u w:val="single"/>'     else ""
    color_xml <- sprintf('<w:color w:val="%s"/>', text_color)
    sz_xml    <- sprintf(
      '<w:sz w:val="%d"/><w:szCs w:val="%d"/>', sz, sz)
    shd_xml   <- if (!is.null(bg_color)) sprintf(
      '<w:shd w:val="clear" w:color="auto" w:fill="%s"/>', bg_color
    ) else ""
    border_xml <- if (has_border) paste0(
      "<w:pBdr>",
      '<w:top    w:val="single" w:sz="4" w:space="1" w:color="CCCCCC"/>',
      '<w:left   w:val="single" w:sz="4" w:space="4" w:color="CCCCCC"/>',
      '<w:bottom w:val="single" w:sz="4" w:space="1" w:color="CCCCCC"/>',
      '<w:right  w:val="single" w:sz="4" w:space="4" w:color="CCCCCC"/>',
      "</w:pBdr>"
    ) else ""

    sprintf(
      '<w:style xmlns:w="%s" w:type="paragraph" w:styleId="%s">
        <w:name w:val="%s"/>
        <w:pPr>%s%s</w:pPr>
        <w:rPr>%s%s%s%s</w:rPr>
      </w:style>',
      ns_uri, style_id, style_name,
      border_xml, shd_xml,
      bold_xml, ul_xml, color_xml, sz_xml
    )
  }

  # Define the four custom styles
  custom_styles <- list(
    # T1: font 16, green pastel background
    make_style_xml("T1Title",      "T1Title",
                   font_size_pt = 16, bg_color = "B8DFB8"),
    # T2: font 14, bold, underlined
    make_style_xml("T2Title",      "T2Title",
                   font_size_pt = 14, bold = TRUE, underline = TRUE),
    # BackgroundBox: font 12, light grey background, border
    make_style_xml("BackgroundBox", "BackgroundBox",
                   font_size_pt = 12, bg_color = "F0F0F0",
                   has_border = TRUE),
    # AnswerResult: font 12, light blue text
    make_style_xml("AnswerResult",  "AnswerResult",
                   font_size_pt = 12, text_color = "ADD8E6")
  )

  for (style_xml_str in custom_styles) {
    style_node <- xml2::read_xml(style_xml_str)
    xml2::xml_add_child(styles_doc, style_node)
  }

  xml2::write_xml(styles_doc, styles_xml_path)

  # ---- 3. Repackage the modified files into a new DOCX ----
  # Use zip's root parameter to set the base directory, avoiding setwd()
  abs_path  <- normalizePath(path, mustWork = FALSE)
  all_files <- list.files(tmp_dir, recursive = TRUE)
  zip::zip(zipfile = abs_path, files = all_files, root = tmp_dir,
           mode = "cherry-pick")

  message("Reference DOCX template aangemaakt: ", path)
}
