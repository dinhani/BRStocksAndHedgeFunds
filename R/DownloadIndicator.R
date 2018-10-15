#' Download Indicators
#'
#' Download daily and monthly values of several Brazilian economic indicators.
#'
#' @param indicator.name Character. The indicator in a human readable format. Serves only to identify the indicator in the returned data.frame.
#' @param indicator.id Chracter. The Quandl unique ID of the desired indicator.
#'
#' @return A data.frame containing:
#' \itemize{
#' \item Indicator (indicator name)
#' \item Date
#' \item Value (percentage)
#' }
#'
#' @import Quandl
#'
#' @examples
#' \dontrun{
#' DownloadIndicatorCDIDaily()
#' DownloadIndicatorCDIMonhtly()
#' DownloadIndicatorIbovespaDaily()}
#'
#' @author Renato Dinhani
DownloadIndicatorQuandl <- function(indicator.id, indicator.name = "") {
  # validate
  stopifnot(is.character(indicator.id))
  stopifnot(is.character(indicator.name))

  # download data
  cdi.data.df <- Quandl::Quandl(indicator.id)

  # rename data
  colnames(cdi.data.df) <- c("Date", "Value")

  # enrich data
  cdi.data.df$Indicator <- indicator.name

  # reorder data
  cdi.data.df <- cdi.data.df[c("Indicator", "Date", "Value")]
  cdi.data.df <- cdi.data.df[order(cdi.data.df$Date), ]

  # return downloaded data
  return(cdi.data.df)
}

# ==============================================================================
# CDI
# ==============================================================================
#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorCDIDaily <- function() {
  DownloadIndicatorQuandl("BCB/12", "CDI-Daily")
}

#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorCDIMonthly <- function() {
  DownloadIndicatorQuandl("BCB/4391", "CDI-Monthly")
}

# ==============================================================================
# IBOVESPA
# ==============================================================================
#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorIbovespaDaily <- function() {
  DownloadIndicatorQuandl("BCB/7", "IBOVESPA-Daily")
}

# ==============================================================================
# IPCA
# ==============================================================================
#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorIpcaAccumulated <- function() {
  DownloadIndicatorQuandl("BCB/13522", "IPCA-Accumulated")
}

#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorIpcaMonthly <- function() {
  DownloadIndicatorQuandl("BCB/433", "IPCA-Monthly")
}

# ==============================================================================
# SELIC
# ==============================================================================
#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorSelicDaily <- function() {
  DownloadIndicatorQuandl("BCB/11", "SELIC-Daily")
}

#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorSelicMonthly <- function() {
  DownloadIndicatorQuandl("BCB/4390", "SELIC-Monthly")
}

# ==============================================================================
# TJLP
# ==============================================================================
#' @rdname DownloadIndicatorQuandl
#' @export
DownloadIndicatorTjlpMonthly <- function() {
  DownloadIndicatorQuandl("BCB/7815", "TJLP-Monthly")
}
