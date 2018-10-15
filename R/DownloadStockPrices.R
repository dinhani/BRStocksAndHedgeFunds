#' Download Stock Prices
#'
#' Download daily prices of stocks listed in the Brazilian stock market.
#'
#' @param ticker Character. The ticker of a brazilian stock to download its daily prices.
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
#' @import quantmod
#' @export
#'
#' @examples
#' \dontrun{
#' DownloadStockPrices("CIEL3")}
#'
#' @author Renato Dinhani
DownloadStockPrices <- function(ticker) {
  stopifnot(is.character(ticker))

  # generate Yahoo ticker for Brazilian stocks
  ticker.yahoo <- paste0(ticker, ".SA")

  # download stock data
  ticker.data <- quantmod::getSymbols(ticker.yahoo, src = "yahoo", auto.assign = FALSE, warnings = FALSE)
  ticker.data.df <- data.frame(Ticker = ticker, Date = zoo::index(ticker.data), zoo::coredata(ticker.data), stringsAsFactors = FALSE)

  # rename downloaded data
  colnames(ticker.data.df) <- c("Ticker", "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

  # reorder stock data columns
  ticker.data.df <- ticker.data.df[c("Ticker", "Date", "Open", "High", "Low", "Close", "Adjusted", "Volume")]

  # return downloaded data
  return(ticker.data.df)
}
