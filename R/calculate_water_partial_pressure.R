#' Function to calculate the water partial pressure/vapour pressure of water 
#' using the August-Roche-Magnus (or Magnus-Tetens or Magnus) equation. 
#' 
#' @param temp Temperature in degrees Celsius -- usually air temperature.
#' 
#' @param rh Relative humidity in percent.
#' 
#' @return Numeric vector with the units of hectopascal/millibar.
#' 
#' @seealso \code{\link{calculate_water_volume_mixing_ratio}}
#' 
#' @export
calculate_water_partial_pressure <- function(temp, rh) {
  
  # Calculate water partial pressure in hectopascal/millibar using the 
  # August-Roche-Magnus (or Magnus-Tetens or Magnus) equation
  if_else(
    temp > 0,
    (rh / 100) * (6.10780 * exp((17.08085 * temp) / (234.175 + temp))),
    (rh / 100) * (6.10780 * exp((17.84362 * temp) / (245.425 + temp)))
  )
  
}
