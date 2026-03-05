#' Parse Prepared Field Key Data into Structured Records
#'
#' Converts prepared field key data from a tabular format into a hierarchical
#' list structure suitable for HTML rendering and further processing.
#'
#' @param data A data frame with prepared field key data, typically output from
#'   \code{\link{prepare_data}}. Must include columns: STEP, .prev, .type, .step,
#'   NAME, NEXT_STEP, CLASSIFICATION, BWK_CODE, SUBKEY, REMARK, and .incoming.
#'
#' @return A named list where each element represents a step, keyed by the
#'   internal step ID (e.g., "bos0001"). Each step contains:
#'   \itemize{
#'     \item \code{step_number}: The display step number
#'     \item \code{html_id}: The internal step identifier
#'     \item \code{incoming_step}: List of steps that link to this step
#'     \item \code{h2}, \code{h3}: Header information (name, remark)
#'     \item \code{question}: Question text and remark
#'     \item \code{background}: Background information
#'     \item \code{answer}: List of answer objects, each containing:
#'       \itemize{
#'         \item \code{name}: Answer text
#'         \item \code{nextstep}: Next step identifier or terminal value
#'         \item \code{n2000}: Natura 2000 classification code
#'         \item \code{bwk}: BWK classification code
#'         \item \code{otherkey}: Reference to another key
#'         \item \code{remark}: Additional remarks
#'         \item \code{info}: Information text (from info rows)
#'       }
#'   }
#'
#' @details
#' The function processes rows sequentially, detecting step changes and
#' aggregating content by row type:
#' \itemize{
#'   \item Headers (h2, h3) are stored at the step level
#'   \item Questions and background are stored at the step level
#'   \item Answers are accumulated in a list
#'   \item Info rows are appended to the previous answer
#' }
#'
#' @examples
#' \dontrun{
#' # After preparing data
#' prepared <- prepare_data(raw_data)
#' parsed <- parse_data(prepared)
#'
#' # Access a specific step
#' step1 <- parsed[["bos0001"]]
#' }
#'
#' @seealso \code{\link{prepare_data}}, \code{\link{html_write_key_page}}
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
