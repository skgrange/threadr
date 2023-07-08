#' Function to calculate atmospheric vapour pressure. 
#' 
#' Water vapour pressure is the atmospheric pressure which is exerted by water 
#' vapour (gaseous water). 
#' 
#' @param temp Air temperature or dew point temperature in degrees Celsius. 
#' 
#' @param type What type of vapour pressure should be calculated? Type \code{1} 
#' is the package's original version, type \code{2} is Bolton 1980's version
#' often used with dew point temperatures, and type \code{3} is Dosseger et 
#' al. 1992's version that is used for the calculation of apparent temperature
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
#' # Use Bolton 1980's formula
#' calculate_vapour_pressure(20, type = 2)
#' 
#' # Calculate actual vapour pressure with dew point temperature
#' calculate_vapour_pressure(calculate_dew_point(temp = 20, rh = 65))
#' calculate_vapour_pressure(calculate_dew_point(temp = 20, rh = 65), type = 2)
#' 
#' @export
calculate_vapour_pressure <- function(temp, type = 1L) {
  
  if (type == 1L) {
    # The original version
    x <- 6.11 * 10 ^ (7.5 * temp / (237.3 + temp))
  } else if (type == 2L) {
    # Bolton 1980's version, usually used with dew point temperature
    x <- 6.112 * exp((17.67 * temp) / (temp + 243.5))
  } else {
    stop("`type` not recognised.", call. = FALSE)
  }
  
  return(x)
  
}
