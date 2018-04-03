#' Function to calculate absolute humidity from temperature and relative 
#' humidity.
#' 
#' @param temp Air temperature in degrees Celcius. 
#' 
#' @param rh Relative humidity in percentage. 
#' 
#' @return Numeric vector, absolute humidity in g.m-3
#' 
#' @author Stuart K. Grange
#' 
#' @export
absolute_humidity <- function(temp, rh) {
  
  # Formula from
  # https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/
  
  (6.112 * exp((17.67 * temp) / (temp + 243.5)) * rh * 2.1674) / (273.15 + temp)
  
}
