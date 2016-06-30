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
#' 
#' @param from Date from when exchange rates should be returned from. The date
#' format should be \code{yyyy-mm-dd}. 
#' 
#' @param rename If \code{from} is used, should the exchange rate variable be
#' renamed as a generic "exchange_rate" rather than the currency pair string?
#' Default is \code{TRUE}.  
#' 
#' @return Numeric vector with a length of one or a data frame. 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Get exchange rate of the New Zealand Dollar to the British Pound
#' nzd_to_gbp <- get_exchange_rates("NZD/GBP")
#' 
#' # On 2015-07-08
#' nzd_to_gbp
#' 0.4289
#' 
#' # Get exchange rates since 2015-07-08
#' data_exchange_rates <- get_exchange_rates("NZD/GBP", from = "2015-07-08")
#' }
#' 
#' @export
get_exchange_rates <- function (currencies, from = NA, rename = TRUE) {
  
  # Parse
  currencies <- stringr::str_to_upper(currencies)
  currencies <- stringr::str_replace(currencies, "_|\\.", "/")
  
  if (is.na(from)) {
    
    exchange_rate <- tryCatch({
      
      suppressWarnings(
        quantmod::getFX(currencies, from = Sys.Date(), auto.assign = FALSE)
      )
      
    }, error = function(e) {
      
      # getFX can fail on the command line due to an issue with libcurl-
      # Get original
      option_download <- options()$download.file.method
      
      # Set
      options(download.file.method = "wget")
      
      # Try the function again
      # Return
      suppressWarnings(
        quantmod::getFX(currencies, from = Sys.Date(), auto.assign = FALSE)
      )
      
    })
    
    # If today is not available, get yesterday's
    if (length(exchange_rate) == 0) {
      
      suppressWarnings(
        exchange_rate <- quantmod::getFX(
          currencies, from = Sys.Date() - lubridate::days(1), auto.assign = FALSE)
      )
      
    }
    
  } else {
    
    # Check date range, then give message if needed
    if (lubridate::ymd(from, tz = "UTC") + lubridate::years(5) <= 
        lubridate::ymd(Sys.Date(), tz = "UTC"))
      warning("This function returns a maximum of five years worth of data.", 
              call. = FALSE)
    
    # Use argument when it is used
    suppressWarnings(
      exchange_rate <- quantmod::getFX(
        currencies, from = from, auto.assign = FALSE)
    )
    
  }
  
  # The returning
  if (length(exchange_rate) == 1) {
    
    # For a single day
    exchange_rate <- as.vector(exchange_rate)
    
  } else {
    
    # For multiple days
    # Make data frame
    exchange_rate <- data.frame(exchange_rate)
    # Get dates
    exchange_rate$date <- lubridate::ymd(row.names(exchange_rate), tz = "UTC")
    # Drop row names
    row.names(exchange_rate) <- NULL
    
    # Reorder variables
    exchange_rate <- exchange_rate[c(2, 1)]
    
    # Rename to generic variable
    if (rename) names(exchange_rate)[2] <- "exchange_rate"
    
  }
  
  # Set download method again
  if (exists("option_download")) options(download.file.method = option_download)
  
  # Return
  exchange_rate
  
}
