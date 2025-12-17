################
# main execution
################

# //set environment
#-----------------

library(tidyverse) # for data manipulation
library(googlesheets4) # for reading Google Sheets
library(glue) # for string interpolation

# source all functions that do not start with _
fun_path <- file.path("source", "functions")
all_function_files <-
  list.files(
    path = fun_path,
    pattern = "^[^_].*\\.[rR]$",
    full.names = TRUE
  )

status <- lapply(all_function_files,
  FUN = source
)

# google authorisatie (aanpassen naargelang gebruiker)
gs4_auth(email = "pieter.verschelde@inbo.be")

# google sheet ids voor de sleutels
sleutel_df <- tribble(
  ~habitat, ~sheet_id,
  "bos", "11_sh5YYlc5RFY8v7ECecAlm-4Euo_4ph2CfIPWb6CBw",
  "grasland", "1dgSVnJBSMm4G4nPNriDzfB1XhyV0xfo-1yqcgjWwr20",
  "heide", "1d38qnUQXfEvkee0HtlI_L8aC76myZdSSAiWg_fLIEh0",
  "moeras", "1eKtmWcgURTCZvshOQPS_IDOga8fC3ADmJ8-p4_rTkHo",
  "water", "17o7UHZipArEBwQiHKxIqHmnaL2YB84Y4x1dHkgSMDBk"
)


# // Read googlesheets
#---------------------

veldsleutels_onbewerkt <- read_gsheet_data(sleutel_df)
saveRDS(veldsleutels_onbewerkt, file.path("interim", "veldsleuteldata.RDS"))
