#' Download Indicator - CDI
#'
#' Download daily and monthly values of the CDI indicator.
#'
#' @return A data.frame containing:
#' \itemize{
#' \item Indicator (indicator name)
#' \item Date
#' \item Value (percentage)
#' }
#'
#' @import Quandl
#' @export
#'
#' @examples
#' \dontrun{
#' DownloadIndicatorCDIDaily()
#' DownloadIndicatorCDIMonhtly()}
#'
#' @author Renato Dinhani
DownloadIndicatorCDIDaily <- function() {
  # download data
  cdi.data.df <- Quandl::Quandl("BCB/12")

  # rename data
  colnames(cdi.data.df) <- c("Date", "Value")

  # enrich data
  cdi.data.df$Indicator <- "CDI-Daily"

  # reorder data
  cdi.data.df <- cdi.data.df[c("Indicator", "Date", "Value")]
  cdi.data.df <- cdi.data.df[order(cdi.data.df$Date), ]

  # return downloaded data
  return(cdi.data.df)
}

#' @rdname DownloadIndicatorCDIDaily
#' @export
DownloadIndicatorCDIMonthly <- function() {
  # download data
  cdi.data.df <- Quandl::Quandl("BCB/4391")

  # rename data
  colnames(cdi.data.df) <- c("Date", "Value")

  # enrich data
  cdi.data.df$Indicator <- "CDI-Monthly"

  # reorder data
  cdi.data.df <- cdi.data.df[c("Indicator", "Date", "Value")]
  cdi.data.df <- cdi.data.df[order(cdi.data.df$Date), ]

  # return downloaded data
  return(cdi.data.df)
}
