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

  view(incoming_steps)

  data <- data |> left_join(incoming_steps, join_by(.step == .nextstep))

  return(data)
}
