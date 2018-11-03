#' Download Stocks Fundamentals
#'
#' Download balance sheet and income statement from stocks listed in the Brazilian stock market.
#'
#' @export
#'
#' @importFrom httr GET write_disk
#' @importFrom lubridate dmy
#' @importFrom purrr map_dfr
#' @importFrom reticulate import py_to_r
#'
#' @param tickers Character. Tickers of one or more brazilian stocks to download its fundamentals.
#' @param folder Character. Path to a folder where the downloaded data (ZIP file and Excel sheet) will be stored.
#'
#' @return A data.frame containing data from the balance sheet and income statement.
#'
#' @examples
#' \dontrun{
#' download_stocks_fundamentals("CIEL3")}
#'
#' @author Renato Dinhani
download_stocks_fundamentals <- function(tickers, folder = "temp/stocks/fundamentals/") {
  purrr::map_dfr(tickers, download_stock_fundamentals, folder = folder)
}

download_stock_fundamentals <- function(ticker, folder = "temp/stocks/fundamentals/") {
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
  ticker_balance <- read_excel(ticker, ticker_fundamentals_xls, 0L)
  ticker_income <- read_excel(ticker, ticker_fundamentals_xls, 1L)

  # generate result list
  cbind(ticker_balance, ticker_income)
}

read_excel <- function(ticker, excel_file, excel_sheet) {
  # import pandas
  pandas <- reticulate::import("pandas", convert = FALSE)

  # read excel file using pandas
  pandas_df <- pandas$read_excel(excel_file, sheet_name = excel_sheet, index_col = 0L, skiprows = 1L)
  pandas_df <- pandas_df$transpose()
  pandas_df$insert(0L, "Ticker", ticker)

  # convert pandas to R data.frame using CSV as intermediary
  pandas_csv <- reticulate::py_to_r(pandas_df$to_csv())
  r_df <- utils::read.csv(textConnection(pandas_csv), stringsAsFactors = FALSE)

  # enhance columns
  colnames(r_df)[1] <- "Date"
  r_df$Date <- lubridate::dmy(r_df$Date)
  r_df$StartDate <- min(r_df$Date, na.rm = TRUE)

  # order rows
  r_df <- r_df[order(r_df$Date), ]

  # return r dataframe
  r_df
}
