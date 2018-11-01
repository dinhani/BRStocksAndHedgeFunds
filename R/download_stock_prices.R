#' Download Stock Prices
#'
#' Download daily prices of stocks listed in the Brazilian stock market.
#'
#' @export
#'
#' @importFrom dplyr lag
#' @importFrom lubridate year quarter month day wday
#' @import quantmod
#'
#' @param ticker Character. The ticker of a brazilian stock to download its daily prices.
#'
#' @return A data.frame containing:
#' \itemize{
#' \item Ticker
#' \item Date
#' \item StartDate
#' \item Year
#' \item Quarter
#' \item Month
#' \item Day
#' \item Weekday
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
#' download_stock_prices("CIEL3")}
#'
#' @author Renato Dinhani
download_stock_prices <- function(ticker) {
  # validate
  stopifnot(is.character(ticker))

  # generate Yahoo ticker for Brazilian stocks
  ticker_yahoo <- paste0(ticker, ".SA")

  # download
  ticker_data_xts <- quantmod::getSymbols(ticker_yahoo, src = "yahoo", auto.assign = FALSE, warnings = FALSE)
  ticker_data_df <- data.frame(Ticker = ticker, Date = zoo::index(ticker_data_xts), zoo::coredata(ticker_data_xts), stringsAsFactors = FALSE)

  # remove rows with NA values
  ticker_data_df <- stats::na.omit(ticker_data_df)

  # rename columns
  colnames(ticker_data_df) <- c("Ticker", "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

  # reorder rows
  ticker_data_df <- ticker_data_df[order(ticker_data_df$Date), ]

  # enhance
  ticker_data_df$StartDate <- min(ticker_data_df$Date)
  ticker_data_df$Year <- lubridate::year(ticker_data_df$Date)
  ticker_data_df$Quarter <- lubridate::quarter(ticker_data_df$Date)
  ticker_data_df$Month <- lubridate::month(ticker_data_df$Date)
  ticker_data_df$Day <- lubridate::day(ticker_data_df$Date)
  ticker_data_df$Weekday <- lubridate::wday(ticker_data_df$Date)
  ticker_data_df$PctChange <- ticker_data_df$Adjusted / dplyr::lag(ticker_data_df$Adjusted, 1) - 1

  # reorder columns
  ticker_data_df <- ticker_data_df[c("Ticker", "Date", "StartDate", "Year", "Quarter", "Month", "Day", "Weekday", "Open", "High", "Low", "Close", "Adjusted", "PctChange", "Volume")]

  # return data
  return(ticker_data_df)
}
