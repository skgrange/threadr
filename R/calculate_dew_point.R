#' Function to calculate the dew point from temperature and relative 
#' humidity.
#' 
#' Dew point temperature is the temperature which air must be cooled to achieve
#' saturation.   
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
  
  # A relative humidity test, make this a requirement
  if (any(rh > 100, na.rm = TRUE)) {
    stop(
      "Values with relative humidity values greater than 100 are detected.", 
      call. = FALSE
    )
  }
  
  (rh / 100) ^ (1 / 8) * (112 + (0.9 * temp)) - 112 + (0.1 * temp)
  
}
