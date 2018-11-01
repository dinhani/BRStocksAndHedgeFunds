#' Download Stock Fundamentals
#'
#' Download balance sheet and income statement from stocks listed in the Brazilian stock market.
#'
#' @export
#'
#' @import httr
#' @import lubridate
#' @import reticulate
#'
#' @param ticker Character. The ticker of a brazilian stock to download its fundamentals.
#' @param folder Character. A path to a folder where the downloaded data (ZIP file and Excel sheet) will be stored.
#'
#' @return A list containing:
#' \itemize{
#' \item ticker: Character. Ticker passed as parameter to this function.
#' \item filename: Character. Path to the downloaded Excel file containing the balance sheet and income statement.
#' \item balance: DataFrame. Balance sheet parsed from the downloaded Excel file.
#' \item income: DataFrame. Income statement parsed from the downloaded Excel file.
#' }
#'
#' @examples
#' \dontrun{
#' download_stock_fundamentals("CIEL3")}
#'
#' @author Renato Dinhani
download_stock_fundamentals <- function(ticker, folder = "temp/fundamentals/") {
  # validate
  stopifnot(is.character(ticker))

  # create base folder
  ticker_folder <- paste0(folder, "/", ticker)
  dir.create(ticker_folder, recursive = TRUE, showWarnings = FALSE)

  # get SID to download fundamentals
  ticker_sid_url <- paste0("http://fundamentus.com.br/balancos.php?papel=", ticker, "&tipo=1")
  ticker_sid_response <- httr::GET(ticker_sid_url)
  ticker_sid <- ticker_sid_response$cookies$value

  # download fundamentals
  ticker_fundamentals_url <- paste0("http://fundamentus.com.br/planilhas.php?SID=", ticker_sid)
  ticker_fundamentals_zip <- paste0(ticker_folder, "/", ticker, ".zip")
  httr::GET(ticker_fundamentals_url, httr::write_disk(ticker_fundamentals_zip, overwrite = TRUE))

  # unzip fundamentals
  utils::unzip(ticker_fundamentals_zip, files = "balanco.xls", exdir = ticker_folder, overwrite = TRUE)

  # rename fundamentals
  ticker_fundamentals_zip_xls <- paste0(ticker_folder, "/balanco.xls")
  ticker_fundamentals_xls <- paste0(ticker_folder, "/", ticker, ".xls")
  file.rename(ticker_fundamentals_zip_xls, ticker_fundamentals_xls)

  # parse fundamentals from excel file
  ticker_balance <- read_excel(ticker_fundamentals_xls, 0L)
  ticker_income <- read_excel(ticker_fundamentals_xls, 1L)

  # generate result list
  list(ticker = ticker, filename = ticker_fundamentals_xls, balance = ticker_balance, income = ticker_income)
}

read_excel <- function(excel_file, excel_sheet) {
  # import pandas
  pandas <- reticulate::import("pandas", convert = FALSE)

  # read excel file using pandas
  pandas_df <- pandas$read_excel(excel_file, sheet_name = excel_sheet, index_col = 0L, skiprows = 1L)
  pandas_df <- pandas_df$transpose()

  # convert pandas to R data.frame using CSV as intermediary
  pandas_csv <- reticulate::py_to_r(pandas_df$to_csv())
  r_df <- utils::read.csv(textConnection(pandas_csv))

  # format columns
  colnames(r_df)[1] <- "Date"
  r_df$Date <- lubridate::dmy(r_df$Date)

  # sort dataframe
  r_df <- r_df[order(r_df$Date), ]

  # return r dataframe
  r_df
}
