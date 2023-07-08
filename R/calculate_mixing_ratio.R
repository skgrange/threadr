#' Function to calculate the mixing ratio of water vapour.  
#' 
#' The mixing ratio is the mass of water vapour in grams mixed into a kilogram 
#' of dry air. 
#' 
#' @param temp Temperature degrees Celsius. 
#' 
#' @param pressure Pressure in hectopascals (hPa). 
#' 
#' @return Numeric vector, mixing ratio in \code{g.kg-1 in dry air}. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{absolute_humidity}}, \code{\link{calculate_dew_point}},
#' \code{\link{calculate_vapour_pressure}}, \code{\link{specific_humidity}}
#' 
#' @examples 
#' 
#' # Calculate the mixing ratio
#' calculate_mixing_ratio(temp = 20, pressure = 1013.25)
#' 
#' @export
calculate_mixing_ratio <- function(temp, pressure = 1013.25) {
  
  # Calculate vapour pressure
  vapour_pressure <- calculate_vapour_pressure(temp, type = 2L)
  
  # Calculate mixing ratio
  621.97 * vapour_pressure / (pressure - vapour_pressure)
  
}
