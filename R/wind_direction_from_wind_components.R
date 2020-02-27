#' Functions to calculate wind direction and wind speed from horizontal wind 
#' components. 
#' 
#' @param u \code{u} wind component. The wind component which represents flow 
#' from west to east. 
#' 
#' @param v \code{v} wind component. The wind component which represents flow 
#' from south to north.
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector.
#' 
#' @examples 
#' 
#' # Calculate wind direction in decimal degrees
#' wind_direction_from_wind_components(3.12, 0.29)
#' 
#' # Calculate wind speed in m s-1
#' wind_speed_from_wind_components(3.12, 0.29)
#' 
#' @export
wind_direction_from_wind_components <- function(u, v) {
  atan2(u, v) * 360 / 2 / pi + 180
}


#' @rdname wind_direction_from_wind_components
#' @export
wind_speed_from_wind_components <- function(u, v) sqrt(u ^ 2 + v ^ 2)
