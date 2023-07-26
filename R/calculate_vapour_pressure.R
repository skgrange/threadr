#' Function to calculate atmospheric vapour pressure. 
#' 
#' Water vapour pressure is the atmospheric pressure which is exerted by water 
#' vapour (gaseous water). 
#' 
#' @author Stuart K. Grange
#' 
#' @param temp Air temperature or dew point temperature in degrees Celsius. 
#' 
#' @param type What type of vapour pressure should be calculated? Type \code{1} 
#' is the package's original version and type \code{2} is Bolton 1980's version
#' often used with dew point temperatures.
#' 
#' @return Numeric vector, vapour pressure in hectopascals (hPa). 
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
  
  # Check input
  stopifnot(is.numeric(temp))
  
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


#' Function to calculate the saturation vapour pressure of water vapour. 
#' 
#' The saturation vapour pressure is the pressure at which water vapour is in 
#' thermodynamic equilibrium with its condensed state. 
#' 
#' @author Stuart K. Grange
#' 
#' @param temp Air temperature in degrees Celsius. 
#' 
#' @param type What type of saturation vapour pressure should be calculated? 
#' Type \code{1} uses the Tetens equation with different logic for temperatures
#' below and above zero degrees and type \code{2} uses the  August-Roche-Magnus 
#' formula. 
#' 
#' @return Numeric vector, vapour pressure in hectopascals (hPa). 
#' 
#' @export
saturation_vapour_pressure <- function(temp, type = 1L) {
  
  # Check input
  stopifnot(is.numeric(temp))
  
  if (type == 1L) {
    # Tetens equation
    # Monteith, J.L., and Unsworth, M.H. 2008.
    x <- if_else(
      temp > 0, 
      6.1078 * exp(17.27 * temp / (temp + 237.3)),
      6.1078 * exp(21.875 * temp) / (temp + 265.5)
    )
  } else if (type == 2L) {
    # August-Roche-Magnus formula
    x <- 6.1094 * exp((17.625 * temp) / (temp + 243.04))  
  } else {
    stop("`type` not recognised.", call. = FALSE)
  }
  
  return(x)
  
}
