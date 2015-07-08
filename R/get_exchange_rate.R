#' Function get current exchange rates. 
#'
#' get_exchange_rate is a simple wrapper for \code{quantmod::getFX} which returns
#' a single exchange as a numeric value. 
#' 
#' @param currencies Currency-pairs to get. 
#' 
#' @examples
#' 
#' \dontrun{
#' # Get exhange rate of the New Zealand Dollar to the British Pound
#' nzd.to.gbp <- get_exchange_rate("NZD/GBP")
#' 
#' # On 2015-07-08
#' nzd.to.gbp
#' 0.4289
#' }
#' 
#' @export
#' 
get_exchange_rate <- function (currencies) {
  
  # Get current exchange rate
  suppressWarnings(
    exchange.rate <- quantmod::getFX(currencies, from = Sys.Date(), 
                                     auto.assign = FALSE)
  )
  
  # Make a vector
  exchange.rate <- as.vector(exchange.rate)
  
  # Return
  exchange.rate
  
}
