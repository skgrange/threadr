#' Function to calculate the mixing ratio of water.
#' 
#' \code{calculate_water_volume_mixing_ratio} uses the August-Roche-Magnus 
#' equation to calculate the partial pressure of air and this is divided by
#' air pressure to calculate the mixing ratio of water in dry air.  
#' 
#' @param temp Temperature in degrees Celsius -- usually air temperature.
#' 
#' @param rh Relative humidity in percent. 
#' 
#' @param pressure Pressure (usually air pressure) in the units of 
#' hectopascal/millibar.
#' 
#' @param as_percent Should the units used for the return be percentage rather 
#' than a ratio? 
#' 
#' @return Numeric vector with the units of a ratio or percentage if 
#' \code{as_percent} is \code{TRUE}.
#' 
#' @seealso \code{\link{calculate_water_partial_pressure}}
#' 
#' @examples
#' 
#' # Calculate the mixing ratio of water from some observations in Zürich, at an
#' # elevation of ~ 430 m, therefore air pressure is relatively low
#' calculate_water_volume_mixing_ratio(13.3, 92.5, 976.5)
#' 
#' # Or as a percent
#'  calculate_water_volume_mixing_ratio(13.3, 92.5, 976.5, as_percent = TRUE)
#' 
#' 
#' @export
calculate_water_volume_mixing_ratio <- function(temp, rh, pressure, 
                                                as_percent = FALSE) {
  
  # Calculate water partial pressure and divide by pressure to get mole 
  # fraction/mixing ratio
  x <- calculate_water_partial_pressure(temp, rh) / pressure
  
  # Transform mixing ratio to percent
  if (as_percent) x <- x * 100
  
  return(x)
  
}
