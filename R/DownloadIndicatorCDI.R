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
  cdi.data <- Quandl::Quandl("BCB/12")

  # rename data
  colnames(cdi.data) <- c("Date", "Value")

  # enrich data
  cdi.data$Indicator <- "CDI-Daily"

  # reorder data
  cdi.data <- cdi.data[c("Indicator", "Date", "Value")]
  cdi.data <- cdi.data[order(cdi.data$Date), ]

  # return downloaded data
  return(cdi.data)
}

#' @rdname DownloadIndicatorCDIDaily
#' @export
DownloadIndicatorCDIMonthly <- function() {
  # download data
  cdi.data <- Quandl::Quandl("BCB/4391")

  # rename data
  colnames(cdi.data) <- c("Date", "Value")

  # enrich data
  cdi.data$Indicator <- "CDI-Monthly"

  # reorder data
  cdi.data <- cdi.data[c("Indicator", "Date", "Value")]
  cdi.data <- cdi.data[order(cdi.data$Date), ]

  # return downloaded data
  return(cdi.data)
}
