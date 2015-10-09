#' Function get exchange rates from Oanda. 
#'
#' \code{get_exchange_rates} is a simple wrapper for \code{quantmod::getFX}. In
#' default usage, \code{get_exchange_rates} returns a single numeric value for
#' the current exchange rate from Oanda (\url{http://www.oanda.com/}). If 
#' a \code{from} date is specified, a data frame is returned with daily exchange
#' rates to the current day. Oanda only supports the retrieval of data up to 
#' five years from the current day. 
#' 
#' @param currencies Currency-pairs to get. See examples for format. 
#' @param from Date from when exchange rates should be returned from. The date
#' format should be \code{yyyy-mm-dd}. 
#' @param rename If \code{from} is used, should the exchange rate variable be
#' renamed as a generic "exchange_rate" rather than the currency pair string?
#' Default is TRUE.  
#' 
#' @examples
#' 
#' \dontrun{
#' # Get exhange rate of the New Zealand Dollar to the British Pound
#' nzd_to_gbp <- get_exchange_rates("NZD/GBP")
#' 
#' # On 2015-07-08
#' nzd_to_gbp
#' 0.4289
#' 
#' 
#' # Get exchange rates since 2015-07-08
#' data_exchange_rates <- get_exchange_rates("NZD/GBP", from = "2015-07-08")
#' 
#' head(data_exchange_rates, 3)
#'       date exchange_rate
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
    exchange_rate_auto_assigned <- quantmod::getFX(currencies, from = from)
  )
  
  # Return the values as a object which name is known
  exchange_rate <- get(exchange_rate_auto_assigned)
  
  if (length(exchange_rate) == 1) {
    
    # For a single day
    # Make a vector
    exchange_rate <- as.vector(exchange_rate)
    
  } else {
    
    # For multiple days
    # Make data frame
    exchange_rate <- data.frame(exchange_rate)
    # Get dates
    exchange_rate$date <- lubridate::ymd(row.names(exchange_rate))
    # Drop row names
    row.names(exchange_rate) <- NULL
    
    # Reorder variables
    exchange_rate <- exchange_rate[c(2, 1)]
    
    # Rename to generic variable
    if (rename) {
      names(exchange_rate)[2] <- "exchange_rate"
    }
      
  }
  
  # Return
  exchange_rate
  
}

# Deprecate singular function
#' @export
get_exchange_rate <- function(x) {
  .Deprecated("get_exchange_rates", package = "threadr")
  get_exchange_rates(x)
}
