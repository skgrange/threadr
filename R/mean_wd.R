#' Function to calculate mean wind direction. 
#' 
#' @param wd Numeric vector representing wind direction. 
#' 
#' @param ws Numeric vector representing wind speed. If \code{ws} is supplied, 
#' average wind directions will be weighted by wind speed and therefore will be
#' the "resultant vector average wind speed". This is usually considered the 
#' most appropriate aggregation for wind direction. 
#' 
#' @param na.rm Should \code{NA}s be omitted from the calculation? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector with length of 1. 
#' 
#' @export 
mean_wd <- function(wd, ws, na.rm = FALSE) {
  
  # Check input vector
  if (any(na.omit(wd) < 0)) 
    stop("Negative wind directions detected...", call. = FALSE)
  
  if (any(na.omit(wd) > 360)) 
    stop("Wind directions greater than 360 detected...", call. = FALSE)
  
  # Calculate wind components, watch the negation
  if (missing(ws)) {
    
    # No weighting by wind speed
    wind_u <- - sin(2 * pi * wd / 360)
    wind_v <- - cos(2 * pi * wd / 360)
    
  } else {
    
    # Weight by wind speed, resultant vector average wind speed
    wind_u <- - ws * sin(2 * pi * wd / 360)
    wind_v <- - ws * cos(2 * pi * wd / 360)
    
  }

  # Mean wind components
  x_u <- mean(wind_u, na.rm = na.rm)
  x_v <- mean(wind_v, na.rm = na.rm)
  
  # Average wind speed
  x <- atan2(x_u, x_v) * 360 / 2 / pi + 180
  
  return(x)
  
}
