#' Download Stocks Prices
#'
#' Download daily prices of stocks listed in the Brazilian stock market.
#'
#' @export
#'
#' @importFrom dplyr lag
#' @importFrom lubridate year quarter month day wday
#' @importFrom quantmod getSymbols
#'
#' @param tickers Character. Tickers of one or more brazilian stocks to download its daily prices.
#'
#' @return A data.frame containing:
#' \itemize{
#' \item Ticker
#' \item Date
#' \item StartDate
#' \item Open
#' \item High
#' \item Low
#' \item Close
#' \item Adjusted (Adjusted Close)
#' \item PctChange (based on Adjusted)
#' \item Volume
#' }
#'
#' @examples
#' \dontrun{
#' download_stocks_prices("CIEL3")}
#'
#' @author Renato Dinhani
download_stocks_prices <- function(tickers, ...) {
  purrr::map_dfr(tickers, download_stocks_price, ...)
}

download_stocks_price <- function(ticker, ...) {
  # validate
  stopifnot(is.character(ticker))

  # generate Yahoo ticker for Brazilian stocks
  # do not modify ticker for indices (eg.: ^BVSP)
  if(startsWith(ticker, "^")) {
    ticker_yahoo <- ticker
  } else {
    ticker_yahoo <- paste0(ticker, ".SA")
  }  

  # download
  ticker_data_xts <- quantmod::getSymbols(ticker_yahoo, auto.assign = FALSE, warnings = FALSE, ...)
  ticker_data_df <- data.frame(Ticker = ticker, Date = zoo::index(ticker_data_xts), zoo::coredata(ticker_data_xts), stringsAsFactors = FALSE)

  # remove rows with NA values
  ticker_data_df <- stats::na.omit(ticker_data_df)

  # rename columns
  colnames(ticker_data_df) <- c("Ticker", "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

  # reorder rows
  ticker_data_df <- ticker_data_df[order(ticker_data_df$Date), ]

  # remove duplicates
  ticker_data_df <- unique(ticker_data_df)

  # enhance
  ticker_data_df$StartDate <- min(ticker_data_df$Date, na.rm = TRUE)
  ticker_data_df$PctChange <- ticker_data_df$Adjusted / dplyr::lag(ticker_data_df$Adjusted, 1) - 1

  # reorder columns
  ticker_data_df <- ticker_data_df[c("Ticker", "Date", "StartDate", "Open", "High", "Low", "Close", "Adjusted", "PctChange", "Volume")]

  # return data
  return(ticker_data_df)
}
