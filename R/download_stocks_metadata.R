#' Download Stocks Metadata
#'
#' Download information like ticker, number of shares and market segment from the companies listed in the Brazilian stock market.
#'
#' @export
#'
#' @importFrom httr GET write_disk
#' @importFrom reticulate import py_to_r
#'
#' @param folder Character. Path to a folder where the downloaded data (Excel sheet) will be stored.
#'
#' @return A data.frame containing:
#' \itemize{
#' \item Name
#' \item Ticker
#' \item FullName
#' \item MarketSegment
#' \item OrdinaryShares
#' \item PreferenceShares
#' \item TotalShares
#' }
#'
#' @examples
#' \dontrun{
#' download_stocks_metadata()}
#'
#' @author Renato Dinhani
download_stocks_metadata <- function(folder = "temp/stocks/metadata/") {
  # create path to store metadata file
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  # download stocks metadata
  metadata_file <- paste0(folder, "stocks.xlsx")
  httr::GET("http://www.b3.com.br/data/files/FE/47/4C/35/AF33B5105CA4E1B5790D8AA8/Capital%20Social.xlsx", httr::write_disk(metadata_file, overwrite = TRUE))

  # parse stocks metatadata
  pandas <- reticulate::import("pandas", convert = FALSE)
  metadata_pandas <- pandas$read_excel(metadata_file)
  metadata_pandas <- metadata_pandas[metadata_pandas$columns[c(0, 1, 2, 3, 7, 8, 9)]]

  # convert pandas to R data.frame using CSV as intermediary
  metadata_pandas_csv <- reticulate::py_to_r(metadata_pandas$to_csv(index = FALSE))
  metadata_r <- utils::read.csv(textConnection(metadata_pandas_csv), stringsAsFactors = FALSE)

  # rename columns
  colnames(metadata_r) <- c("Name", "Ticker", "FullName", "MarketSegment", "OrdinaryShares", "PreferenceShares", "TotalShares")

  # remove empty rows
  metadata_r <- metadata_r[metadata_r$Ticker != "", ]

  # order rows
  metadata_r <- metadata_r[order(metadata_r$Ticker), ]

  # return r dataframe
  metadata_r
}
