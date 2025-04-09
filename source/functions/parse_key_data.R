parse_key_data <- function(sheet_data) {
  print(attributes(sheet_data))
  questions <- sheet_data %>% filter(.data$TYPE == "Q")
  
  # Create a list of records
  records_list <- list()
  current_record <- list()
  for (i in 1:nrow(sheet_data)) {
    print(paste(i, "of", nrow(sheet_data), ":", sheet_data[i, "NAME"] ))  # Debugging line
    type_record <- substring(sheet_data[i, "TYPE"],1,1)
    type_record_sub <- substring(sheet_data[i, "TYPE"],2,2)
    next_type_record <- substring(sheet_data[i+1, "TYPE"],1,1)
    
    #set NA type records to "NA" to avoid missing value where TRUE/FALSE needed
    if (is.na(type_record)) {
      type_record <- "NA"
    }
    if (is.na(type_record_sub)) {
      type_record_sub <- "NA"
    }
    if (is.na(next_type_record)) {
      next_type_record <- "NA"
    }
 
    if (type_record %in% c("Q", "T")) {
      # save previous record
      if (length(current_record)) {
        records_list[[length(records_list) + 1]] <- current_record
      }
      # Start a new record
      current_record <- list()
      if (type_record == "T") {
        current_record$type = "TITLE"
        current_record$text <- unlist(sheet_data[i, "NAME"])
        current_record$level <- type_record_sub
      } else {
        current_record$type = "STEP"
        current_record$step_number <- unlist(sheet_data[i, "STEP"])
        current_record$html_id <- paste0("S", current_record$step_number)
        current_record$incoming_step <-  
          as.list(na.omit(unique(sheet_data$STEP[sheet_data$NEXT_STEP == current_record$step_number])))
        current_record$text <- unlist(sheet_data[i, "NAME"])
        current_record$remark <- unlist(sheet_data[i, "REMARK"])
        current_record$answers <- list()
      }
    } else if (type_record == "A") {
      # Add answer to current record
      answer <- list()
      answer$text <- sheet_data[i, "NAME", drop = TRUE]
      answer$next_step <- sheet_data[i, "NEXT_STEP", drop = TRUE]
      answer$n2000 <- sheet_data[i, "CLASSIFICATION", drop = TRUE]
      answer$bwk <- sheet_data[i, "BWK_CODE", drop = TRUE]
      answer$other_key <- sheet_data[i, "SUBKEY", drop = TRUE]
      answer$remark <- sheet_data[i, "REMARK", drop = TRUE]
 
      if (next_type_record == "I" && (i < nrow(sheet_data))) {
        # Add extra information to answer
        answer$extra_info <- unlist(sheet_data[i+1, "NAME"])
      }
      current_record$answers[[length(current_record$answers) + 1]] <- answer
    } else {
      #do nothing
    }
    
    #save on EOF (because other saves are at the beginning of the next step)
    if (i == nrow(sheet_data)) {
      records_list[[length(records_list) + 1]] <- current_record
    }
  }
  records_list
}
