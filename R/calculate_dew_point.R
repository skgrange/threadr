#' Function to calculate the dew point from temperature and relative 
#' humidity.
#' 
#' @param temp Air temperature in degrees Celsius. 
#' 
#' @param rh Relative humidity in percentage. 
#' 
#' @return Numeric vector, dew point temperature in degrees Celsius.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Calculate dew point with temperature in degrees C and relative humidity
#' # in percent
#' calculate_dew_point(20, 65)
#' 
#' @export
calculate_dew_point <- function(temp, rh) {
  
  # Pulled from https://github.com/geanders/weathermetrics
  
  # A relative humidity test, made this a requirement
  if (any(rh > 100, na.rm = TRUE)) {
    stop(
      "Values with relative humidity values greater than 100 are detected.", 
      call. = FALSE
    )
  }
  
  (rh / 100) ^ (1 / 8) * (112 + (0.9 * temp)) - 112 + (0.1 * temp)
  
}


#' Function to calculate atmospheric vapour pressure. 
#' 
#' @param temp Air temperature or dew point temperature in degrees Celsius. 
#' 
#' @return Numeric vector, vapour pressure in hectopascals (hPa). 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Calculate saturated vapour pressure with air temperature
#' calculate_vapour_pressure(20)
#' 
#' # Calculate actual vapour pressure with dew point temperature
#' calculate_vapour_pressure(calculate_dew_point(temp = 20, rh = 65))
#' 
#' @export
calculate_vapour_pressure <- function(temp) {
  6.11 * 10 ^ (7.5 * temp / (237.3 + temp))
}
