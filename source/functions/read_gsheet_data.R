#' Read Field Key Data from Google Sheets
#'
#' Reads multiple field key datasets from Google Sheets using the googlesheets4
#' package. Each sheet contains structured field key data with steps, questions,
#' and answers.
#'
#' @param df A data frame with columns:
#'   \itemize{
#'     \item \code{habitat}: Name of the habitat/key (e.g., "bos", "grasland")
#'     \item \code{sheet_id}: Google Sheets ID for the key
#'     \item \code{afkorting}: Abbreviated key code (e.g., "bos", "grl")
#'   }
#'
#' @return A named list of data frames, one for each habitat. Each data frame
#'   contains the field key data with columns: STEP, TYPE, NAME, NEXT_STEP,
#'   CLASSIFICATION, BWK_CODE, SUBKEY, REMARK, and KEY. The metadata from
#'   cells A1:B4 is attached as an attribute.
#'
#' @details
#' For each row in the input data frame, the function:
#' \enumerate{
#'   \item Reads metadata from cells A1:B4 of the Google Sheet
#'   \item Reads the main data from cells A5:H1000
#'   \item Filters out rows with missing STEP values
#'   \item Adds the abbreviated key code to each row
#'   \item Attaches metadata as an attribute
#'   \item Stores the result in a named list using the habitat name
#' }
#'
#' The function uses \code{googlesheets4::read_sheet()} and requires prior
#' authentication via \code{gs4_auth()}.
#'
#' @examples
#' \dontrun{
#' # Authenticate with Google
#' gs4_auth(email = "your.email@example.com")
#'
#' # Define field keys
#' sleutel_df <- tribble(
#'   ~habitat, ~sheet_id, ~afkorting,
#'   "bos", "11_sh5YYlc5RFY8v7ECecAlm-4Euo_4ph2CfIPWb6CBw", "bos",
#'   "grasland", "1dgSVnJBSMm4G4nPNriDzfB1XhyV0xfo-1yqcgjWwr20", "grl"
#' )
#'
#' # Read all sheets
#' raw_data <- read_gsheet_data(sleutel_df)
#'
#' # Access individual keys
#' bos_data <- raw_data[["bos"]]
#' }
#'
#' @seealso \code{\link{prepare_data}}, \code{\link{parse_data}}
read_gsheet_data <- function(df) {
  sleutels <- list()
  for (i in 1:nrow(df)) {
    sleutel <- df[i, ] |>
      pull("habitat") |>
      as.character()
    print(sleutel)
    key <- df[i, ] |>
      pull("sheet_id") |>
      as.character()
    sleutel_kort <- df[i, ] |>
      pull("afkorting")
    sheet <- 1
    df_meta <- read_sheet(
      ss = key,
      sheet = sheet,
      range = "A1:B4",
      col_types = "cc"
    )
    sheet_data <- read_sheet(
      ss = key,
      sheet = sheet,
      range = "A5:H1000",
      col_types = "cccccccc"
    ) %>%
      filter(!is.na(STEP)) |>
      mutate(KEY = sleutel_kort)
    attr(sheet_data, "meta") <- df_meta
    sleutels[[sleutel]] <- sheet_data
  }
  sleutels
}
