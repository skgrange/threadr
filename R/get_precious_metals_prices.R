#' Function to get the cost of precious metals from 
#' \code{\link{http://services.packetizer.com/spotprices/}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get precious metal prices
#' data_metals <- get_precious_metals_prices()
#' 
#' }
#' 
#' @import dplyr
#' 
#' @export
get_precious_metals_prices <- function() {
  
  # Read url
  url <- "http://services.packetizer.com/spotprices/?f=json"
  text <- read_lines(url)
  
  # Parse
  json <- read_json(text)
  
  # Parse date
  json$date <- lubridate::ymd(json$date, tz = "UTC")
  
  # To data frame and reshape
  df <- as.data.frame(json, stringsAsFactors = FALSE) %>% 
    tidyr::gather(metal, value, -date) %>% 
    mutate(currency = "usd")
  
  # Return
  df
  
}
