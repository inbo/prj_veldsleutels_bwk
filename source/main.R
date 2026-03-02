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
# folder: https://drive.google.com/drive/folders/1zAjO1kQaZEYxD7J7DgtYejPjftQ-7du2)
sleutel_df <- tribble(
  ~habitat, ~sheet_id, ~afkorting,
  "bos", "11_sh5YYlc5RFY8v7ECecAlm-4Euo_4ph2CfIPWb6CBw", "bos",
  "grasland", "1dgSVnJBSMm4G4nPNriDzfB1XhyV0xfo-1yqcgjWwr20", "grl",
  "heide", "1d38qnUQXfEvkee0HtlI_L8aC76myZdSSAiWg_fLIEh0", "hei",
  "moeras", "1eKtmWcgURTCZvshOQPS_IDOga8fC3ADmJ8-p4_rTkHo", "mrs",
  "water", "17o7UHZipArEBwQiHKxIqHmnaL2YB84Y4x1dHkgSMDBk", "wtr"
)


# // Read googlesheets
#---------------------

veldsleutels_onbewerkt <- read_gsheet_data(sleutel_df)
saveRDS(veldsleutels_onbewerkt, file.path("interim", "veldsleuteldata.RDS"))

veldsleutels_raw <- readRDS(file.path("interim", "veldsleuteldata.RDS"))
veldsleutels_prep <- lapply(veldsleutels_raw, prepare_data)
sapply(veldsleutels_prep, dim) # check the amount of records


# //parse data
#--------------

veldsleutels <- lapply(veldsleutels_prep, parse_data)
veldsleutels_yaml <- yaml::as.yaml(veldsleutels)
cat(veldsleutels_yaml, file = "test.yaml")


# // check logic
#--------------

check_logic(veldsleutels)

# //html render
#---------------
data <- veldsleutels
main_keys <- names(data) # bos, mrs, grl, wtr, hei
for (k in main_keys) {
  html_write_key_page(k, data[[k]], "www")
}
html_write_index_page(main_keys, "")


# //pdf render
#-------------

create_pdf_markdown(data, "veldsleutels.Rmd")
rmarkdown::render("veldsleutels.Rmd", )
