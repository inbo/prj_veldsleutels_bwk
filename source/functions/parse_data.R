parse_data <- function(data) {
  records_list <- list()
  rec <- list()
  record_counter <- 0
  answer_counter <- 0

  for (i in 1:nrow(data)) {
    row <- data[i, ]

    # Identify the current step and the previous one
    step <- row$STEP
    prevstep <- row$.prev
    logictype <- row$.type
    step_intern <- row$.step # e.g., "bos0001"

    # Detect step change
    if (step != prevstep) {
      # Save the completed record using its stored ID
      if (prevstep != "-1") {
        records_list[[rec$html_id]] <- rec
      }

      # Reset for the NEW record
      rec <- list()
      answer_counter <- 0

      # Store identification inside the record
      rec$step_number <- step
      rec$html_id <- step_intern
      rec$incoming_step <- row$.incoming[[1]] # row$.incoming is a list column
    }

    # Fill content for Headers (h2, h3), Questions (q), or Background (b)
    if (substring(logictype, 1, 1) %in% c("h", "q", "b")) {
      rec[[logictype]]$name <- row$NAME
      rec[[logictype]]$remark <- row$REMARK
    }

    # Handle Answer rows
    if (logictype == "answer") {
      answer_counter <- answer_counter + 1
      rec$answer[[answer_counter]] <- list(
        name = row$NAME,
        nextstep = row$NEXT_STEP,
        n2000 = row$CLASSIFICATION,
        bwk = row$BWK_CODE,
        otherkey = row$SUBKEY,
        remark = row$REMARK,
        info = "" # Initialize empty info string
      )
    }

    # Handle Info (Type I) rows - Grouping them with the previous Answer
    if (logictype == "info") {
      if (answer_counter > 0) {
        existing_info <- rec$answer[[answer_counter]]$info
        new_info <- row$NAME

        if (is.null(existing_info) || existing_info == "") {
          rec$answer[[answer_counter]]$info <- new_info
        } else {
          # Append multiple info rows with a newline for clarity
          rec$answer[[answer_counter]]$info <- paste(existing_info, new_info, sep = "\n")
        }
      }
    }
  }

  # Final Save: Capture the last record after the loop finishes
  if (length(rec) > 0) {
    records_list[[rec$html_id]] <- rec
  }

  return(records_list)
}
