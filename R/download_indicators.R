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
#' download_indicator_cdi_daily()
#' download_indicator_cdi_monhtly()
#' download_indicator_ibovespa_daily()}
#'
#' @author Renato Dinhani
download_indicator_quandl <- function(indicator.id, indicator.name = "") {
  # validate
  stopifnot(is.character(indicator.id))
  stopifnot(is.character(indicator.name))

  # download data
  cdi_data_df <- Quandl::Quandl(indicator.id)

  # rename data
  colnames(cdi_data_df) <- c("Date", "Value")

  # enrich data
  cdi_data_df$Indicator <- indicator.name

  # reorder data
  cdi_data_df <- cdi_data_df[c("Indicator", "Date", "Value")]
  cdi_data_df <- cdi_data_df[order(cdi_data_df$Date), ]

  # return downloaded data
  return(cdi_data_df)
}

# ==============================================================================
# CDI
# ==============================================================================
#' @rdname download_indicator_quandl
#' @export
download_indicator_cdi_daily <- function() {
  download_indicator_quandl("BCB/12", "CDI-Daily")
}

#' @rdname download_indicator_quandl
#' @export
download_indicator_cdi_monthly <- function() {
  download_indicator_quandl("BCB/4391", "CDI-Monthly")
}

# ==============================================================================
# IBOVESPA
# ==============================================================================
#' @rdname download_indicator_quandl
#' @export
download_indicator_ibovespa_daily <- function() {
  download_indicator_quandl("BCB/7", "IBOVESPA-Daily")
}

# ==============================================================================
# IPCA
# ==============================================================================
#' @rdname download_indicator_quandl
#' @export
download_indicator_ipca_accumulated <- function() {
  download_indicator_quandl("BCB/13522", "IPCA-Accumulated")
}

#' @rdname download_indicator_quandl
#' @export
download_indicator_ipca_monthly <- function() {
  download_indicator_quandl("BCB/433", "IPCA-Monthly")
}

# ==============================================================================
# SELIC
# ==============================================================================
#' @rdname download_indicator_quandl
#' @export
download_indicator_selic_daily <- function() {
  download_indicator_quandl("BCB/11", "SELIC-Daily")
}

#' @rdname download_indicator_quandl
#' @export
download_indicator_selic_monthly <- function() {
  download_indicator_quandl("BCB/4390", "SELIC-Monthly")
}

# ==============================================================================
# TJLP
# ==============================================================================
#' @rdname download_indicator_quandl
#' @export
download_indicator_tjlp_monthly <- function() {
  download_indicator_quandl("BCB/7815", "TJLP-Monthly")
}
