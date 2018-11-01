#' Download Stock Prices
#'
#' Download daily prices of stocks listed in the Brazilian stock market.
#'
#' @export
#'
#' @import quantmod
#'
#' @param ticker Character. The ticker of a brazilian stock to download its daily prices.
#'
#' @return A data.frame containing:
#' \itemize{
#' \item Ticker
#' \item Date
#' \item Open
#' \item High
#' \item Low
#' \item Close
#' \item Adjusted (Adjusted Close)
#' \item Volume
#' }
#'
#' @examples
#' \dontrun{
#' download_stock_prices("CIEL3")}
#'
#' @author Renato Dinhani
download_stock_prices <- function(ticker) {
  # validate input
  stopifnot(is.character(ticker))

  # generate Yahoo ticker for Brazilian stocks
  ticker_yahoo <- paste0(ticker, ".SA")

  # download data
  ticker_data_xts <- quantmod::getSymbols(ticker_yahoo, src = "yahoo", auto.assign = FALSE, warnings = FALSE)
  ticker_data_df <- data.frame(Ticker = ticker, Date = zoo::index(ticker_data_xts), zoo::coredata(ticker_data_xts), stringsAsFactors = FALSE)

  # rename data
  colnames(ticker_data_df) <- c("Ticker", "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

  # reorder data
  ticker_data_df <- ticker_data_df[c("Ticker", "Date", "Open", "High", "Low", "Close", "Adjusted", "Volume")]
  ticker_data_df <- ticker_data_df[order(ticker_data_df$Date), ]

  # return data
  return(ticker_data_df)
}
