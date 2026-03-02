###############################################################

parse_data <- function(data) {
  records_list <- list()
  rec <- list() # current record
  record_counter <- 0
  answer_counter <- 0

  # // iterate row per row through data
  for (i in 1:nrow(data)) {
    # print(paste(i, "of", nrow(sheet_data), ":", sheet_data[i, "NAME"]))

    # define short names
    row <- data[i, ]
    step <- row$STEP
    type <- row$TYPE
    name <- row$NAME
    nextstep <- row$NEXT_STEP
    classif <- row$CLASSIFICATION
    bwkcode <- row$BWK_CODE
    otherkey <- row$SUBKEY
    remark <- row$REMARK
    key <- row$KEY
    prevstep <- row$.prev
    logictype <- row$.type
    incoming <- row$.incoming
    step_intern <- row$.step

    # start new current record if .prev is not equal to step
    # save the current record in the records list
    if (step != prevstep) {
      # when first record do not save previous record but set new record
      if (prevstep == -1) {
        rec <- list()
        record_counter <- record_counter + 1
        answer_counter <- 0
        # save previous record and reset new record
      } else {
        records_list[[record_counter]] <- rec
        names(records_list)[record_counter] <- step_intern
        rec <- list()
        record_counter <- record_counter + 1
        answer_counter <- 0
      }
    }

    # create contents for record
    if (substring(logictype, 1, 1) %in% c("h", "q", "b")) {
      rec[[logictype]]$name <- name
      rec[[logictype]]$remark <- remark
    }
    rec$step_number <- step
    rec$html_id <- step_intern
    rec$incoming_step <- incoming

    if (logictype == "answer") {
      answer_counter <- answer_counter + 1
      rec$answer[[answer_counter]] <- list()
      rec$answer[[answer_counter]]$name <- name
      rec$answer[[answer_counter]]$nextstep <- nextstep
      rec$answer[[answer_counter]]$n2000 <- classif
      rec$answer[[answer_counter]]$bwk <- bwkcode
      rec$answer[[answer_counter]]$otherkey <- otherkey
      rec$answer[[answer_counter]]$remark <- remark
    }
    if (logictype == "info") {
      rec$answer[[answer_counter]]$info <- name
    }
  }
  records_list
}

# parse_data(veldsleutels_prep$bos)

# nog omgaan met lege rijen (bossleutel, 501 is erafgekapt
# nog invullen van antwoorden


#########################################################################
