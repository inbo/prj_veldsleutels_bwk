#' Prepare Raw Field Key Data for Parsing
#'
#' Validates, filters, and transforms raw field key data from Google Sheets
#' into a standardized format ready for parsing. This includes type mapping,
#' ID generation, and incoming step calculation.
#'
#' @param data A data frame containing raw field key data from Google Sheets.
#'   Must include columns: STEP, TYPE, NAME, NEXT_STEP, CLASSIFICATION,
#'   BWK_CODE, SUBKEY, REMARK, and KEY.
#'
#' @return A data frame with additional computed columns:
#'   \itemize{
#'     \item \code{.prev}: Previous step number (for change detection)
#'     \item \code{.type}: Mapped type (h2, h3, h4, info, background, question, answer)
#'     \item \code{.step}: Internal step ID (e.g., "bos0001")
#'     \item \code{.nextstep}: Internal next step ID
#'     \item \code{.incoming}: List of steps that reference this step
#'   }
#'   All NA values in text columns are replaced with empty strings.
#'
#' @details
#' The function performs several data preparation tasks:
#' \enumerate{
#'   \item Validates that all required columns are present
#'   \item Filters out rows with missing or excluded STEP values
#'   \item Maps TYPE codes to semantic names:
#'     \itemize{
#'       \item T1 → h2 (level 2 header)
#'       \item T2 → h3 (level 3 header)
#'       \item T3 → h4 (level 4 header)
#'       \item I → info (additional information)
#'       \item BI → background (background information)
#'       \item Q → question
#'       \item A → answer
#'     }
#'   \item Creates zero-padded internal step IDs (e.g., "bos0001")
#'   \item Calculates incoming steps (which steps link to each step)
#'   \item Replaces NA values with empty strings
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare data after reading from Google Sheets
#' raw <- read_gsheet_data(sleutel_df)
#' prepared <- lapply(raw, prepare_data)
#' }
#'
#' @seealso \code{\link{read_gsheet_data}}, \code{\link{parse_data}}
prepare_data <- function(data) {
  colnames <- c(
    "STEP", "TYPE", "NAME", "NEXT_STEP",
    "CLASSIFICATION", "BWK_CODE", "SUBKEY", "REMARK", "KEY"
  )
  if (!all(names(data) %in% colnames)) {
    stop(paste(
      "all columns must be present",
      paste(colnames, collapse = ", ")
    ))
  }
  data <- data |>
    select(all_of(colnames)) |>
    filter(
      !is.na(STEP),
      !(STEP %in% c("x", "X"))
    )

  if (any(is.na(data$TYPE))) stop("every row needs a type")

  data <- data |>
    mutate(
      .prev = replace_na(lag(.data$STEP), "-1"),
      .type =
        case_when(
          TYPE == "T1" ~ "h2",
          TYPE == "T2" ~ "h3",
          TYPE == "T3" ~ "h4",
          TYPE == "I" ~ "info",
          TYPE == "BI" ~ "background",
          TYPE == "Q" ~ "question",
          TYPE == "A" ~ "answer",
          .default = "undefined"
        ),
      .step = paste0(
        .data$KEY,
        stringr::str_pad(
          .data$STEP,
          width = 4,
          side = "left",
          pad = "0"
        )
      ),
      .nextstep = ifelse(!is.na(.data$NEXT_STEP),
        paste0(
          .data$KEY,
          stringr::str_pad(
            .data$NEXT_STEP,
            width = 4,
            side = "left",
            pad = "0"
          )
        ),
        ""
      ),
      NAME = replace_na(.data$NAME, ""),
      CLASSIFICATION = replace_na(.data$CLASSIFICATION, ""),
      BWK_CODE = replace_na(.data$BWK_CODE, ""),
      SUBKEY = replace_na(.data$SUBKEY, ""),
      REMARK = replace_na(.data$REMARK, "")
    )

  if (any(data$.type == "undefined")) stop("undefined TYPE found")

  # incoming steps
  incoming_steps <- data |>
    filter(.nextstep != "") |>
    distinct(.step, .nextstep) |>
    group_by(.nextstep) |>
    summarize(.incoming = list(.step))

  # view(incoming_steps)  # Debugging only - removed for production

  data <- data |> left_join(incoming_steps, join_by(.step == .nextstep))

  return(data)
}
