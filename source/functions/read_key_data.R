read_key_data <- function(df) {
  sleutels <- list()
  for (i in 1:nrow(df)) {
    sleutel <- df[i,] |> pull("Sleutel") |> as.character()
    print(sleutel)
    key <- df[i,] |> pull("Link") |> as.character()
    sheet <- 1
    df_meta <- read_sheet(ss = key,
                          sheet = sheet,
                          range = "A1:B4",
                          col_types = "cc")
    sheet_data <- read_sheet(ss = key,
                             sheet = sheet,
                             range = "A5:H1000", 
                             col_types = "cccccccc") %>% 
      filter(!is.na(STEP))
    attr(sheet_data, "meta") <- df_meta
    sleutels[[sleutel]] <- sheet_data
  }
  sleutels
}

# sleutels[[sleutel]] <- parse_key_data(sheet_data)
# attr(sleutels[[sleutel]], "meta") <- df_meta