check_logic <- function(data) {
  for (key_group in names(data)) {
    cat("\n--- Analysing Key Group:", toupper(key_group), "---\n")

    steps <- data[[key_group]]
    step_ids <- names(steps)
    step_numbers <- sapply(steps, function(x) as.character(x$step_number))

    # Track which steps are referenced as a 'nextstep'
    referenced_steps <- c()

    for (id in step_ids) {
      curr_step <- steps[[id]]
      curr_nr <- curr_step$step_number

      # 1. Check for Stale Answers
      if (!is.null(curr_step$answer)) {
        for (i in seq_along(curr_step$answer)) {
          ans <- curr_step$answer[[i]]
          ns <- ans$nextstep
          res_n2k <- ans$n2000
          res_bwk <- ans$bwk

          # Logic: An answer is stale if it has no next step AND no result codes
          is_terminal_val <- is.null(ns) || is.na(ns) || ns == ".na.character"
          has_result <- (!is.null(res_n2k) && res_n2k != "") ||
            (!is.null(res_bwk) && res_bwk != "") ||
            (is.character(ans$otherkey) && ans$otherkey != "")

          if (is_terminal_val && !has_result) {
            cat(sprintf(
              "[STALE ANSWER] Step %s, Answer %d ('%s') is a dead end.\n",
              curr_nr, i, ans$name
            ))
          }

          # Collect references for Reachability check
          if (!is_terminal_val && ns != "SLEUTEL") {
            referenced_steps <- c(referenced_steps, as.character(ns))
          }
        }
      }
    }

    # 2. Check for Unreachable Steps
    # Step 1 (or the first step) is usually the entry point, so we ignore it
    entry_step_nr <- step_numbers[1]
    unreachable <- step_numbers[!(step_numbers %in% referenced_steps) &
      (step_numbers != entry_step_nr)]

    if (length(unreachable) > 0) {
      cat("[UNREACHABLE STEPS]: The following step numbers are never called by an answer:\n")
      cat(paste(unreachable, collapse = ", "), "\n")
    } else {
      cat("[OK] All steps are reachable.\n")
    }
  }
}

# check_logic(data)
