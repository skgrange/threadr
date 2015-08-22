#' Function get exchange rates. 
#'
#' \code{get_exchange_rates} is a simple wrapper for \code{quantmod::getFX}. In
#' default usage, \code{get_exchange_rates} returns a single numeric value for
#' the current exchange rate from Oanda (\url{http://www.oanda.com/}). If 
#' a \code{from} date is specified, a data frame is returned with daily exchange
#' rates to the current day. Oanda only supports the retrieval of data up to 
#' five years from the current day. 
#' 
#' @param currencies Currency-pairs to get. 
#' @param from Date from when exchange rates should be returned from. The date
#' format should be \code{yyyy-mm-dd}. 
#' @param rename If \code{from} is used, should the exchange rate variable be
#' renamed as a generic "exchange.rate" rather than the currency pair string?
#' Default is TRUE.  
#' 
#' @examples
#' 
#' \dontrun{
#' # Get exhange rate of the New Zealand Dollar to the British Pound
#' nzd.to.gbp <- get_exchange_rates("NZD/GBP")
#' 
#' # On 2015-07-08
#' nzd.to.gbp
#' 0.4289
#' 
#' 
#' # Get exchange rates since 2015-07-08
#' data.exchange.rates <- get_exchange_rates("NZD/GBP", from = "2015-07-08")
#' 
#' head(data.exchange.rates, 3)
#'       date exchange.rate
#' 2015-07-08        0.4289
#' 2015-07-09        0.4339
#' 2015-07-10        0.4375
#' }
#' 
#' @export
#' 
get_exchange_rates <- function (currencies, from = NA, rename = TRUE) {
  
  # If no date is specified, use system time
  if (is.na(from)) {
    from <- Sys.Date()
  }
  
  # Check date range, then give message
  if (lubridate::ymd(from) + lubridate::years(5) <= lubridate::ymd(Sys.Date())) {
    message("This function returns a maximum of five years worth of data.")
  }
  
  # Get current exchange rate, a xts object
  suppressWarnings(
    exchange.rate.auto.assigned <- quantmod::getFX(currencies, from = from)
  )
  
  # Return the values as a object which name is known
  exchange.rate <- get(exchange.rate.auto.assigned)
  
  if (length(exchange.rate) == 1) {
    
    # For a single day
    # Make a vector
    exchange.rate <- as.vector(exchange.rate)
    
  } else {
    
    # For multiple days
    # Make data frame
    exchange.rate <- data.frame(exchange.rate)
    # Get dates
    exchange.rate$date <- lubridate::ymd(row.names(exchange.rate))
    # Drop row names
    row.names(exchange.rate) <- NULL
    
    # Reorder variables
    exchange.rate <- exchange.rate[c(2, 1)]
    
    # Rename to generic variable
    if (rename) {
      names(exchange.rate)[2] <- "exchange.rate"
    }
      
  }
  
  # Return
  exchange.rate
  
}

#' Deprecate singular function
#' @export
get_exchange_rate <- function(x) {
  .Deprecated("get_exchange_rates", package = "threadr")
  get_exchange_rates(x)
}
