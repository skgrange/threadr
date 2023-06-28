#' Function to calculate absolute humidity from temperature and relative 
#' humidity. 
#' 
#' Absolute humidity is the amount of amount of water vapour in air, usually
#' expressed in \code{g.m-3}. 
#' 
#' @param air_temp Air temperature in degrees Celsius. 
#' 
#' @param rh Relative humidity in percentage. 
#' 
#' @return Numeric vector, absolute humidity in \code{g.m-3}.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Calculate absolute humidity with temperature in degrees C and relative
#' # humidity in percent
#' absolute_humidity(10.7, 73.3)
#' 
#' @export
absolute_humidity <- function(air_temp, rh) {
  # https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/
  (6.112 * exp((17.67 * air_temp) / (air_temp + 243.5)) * rh * 2.1674) / 
    (273.15 + air_temp)
}


#' Function to calculate relative humidity from air temperature and dew point 
#' temperature. 
#' 
#' @param air_temp Air temperature in degrees Celsius. 
#' 
#' @param dewpoint Dew point temperature in degrees Celsius. 
#' 
#' @param m,t_n Constants for the approximation. The default values are accurate 
#' in temperatures which are experienced in the ambient atmosphere. 
#' 
#' @return Numeric vector, relative humidity in percentage (\%).
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Calculate relative humidity in percent with degrees C
#' calculate_relative_humidity(10.7, 6.11)
#' 
#' @export
calculate_relative_humidity <- function(air_temp, dewpoint, m = 7.591386, 
                                        t_n = 240.7263) {
  # https://earthscience.stackexchange.com/questions/16570/how-to-calculate-relative-humidity-from-temperature-dew-point-and-pressure
  100 * 10 ^ (m * ((dewpoint / (dewpoint + t_n)) - (air_temp / (air_temp + t_n))))
}
