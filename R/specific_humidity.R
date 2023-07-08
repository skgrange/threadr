#' Function to calculate specific humidity. 
#' 
#' Specific humidity is the mass of water vapour in grams per kilogram of moist 
#' air.
#' 
#' @param temp Temperature degrees Celsius. 
#' 
#' @param rh Relative humidity in percentage.
#' 
#' @param pressure Pressure in hectopascals (hPa). 
#' 
#' @return Numeric vector, specific humidity in \code{g.kg-1}. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{absolute_humidity}}, \code{\link{calculate_dew_point}},
#' \code{\link{calculate_vapour_pressure}}, \code{\link{calculate_mixing_ratio}}
#' 
#' @examples 
#' 
#' # Calculate specific humidities at different relative humidities
#' specific_humidity(20, 95:100)
#' 
#' @export
specific_humidity <- function(temp, rh, pressure = 1013.25) {
  
  # Calculate dew point temperature
  dew_point <- calculate_dew_point(temp, rh)
  
  # Calculate vapour pressure
  vapour_pressure <- calculate_vapour_pressure(dew_point, type = 2L)
  
  # In kg.kg-1
  specific_humidity <- (0.622 * vapour_pressure) / 
    (pressure - (0.378 * vapour_pressure)) * 1000
  
  return(specific_humidity)
  
}
