#' Function to calculate mean wind direction. 
#' 
#' @param wd Numeric vector representing wind direction in degrees. 
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
#' @return Numeric vector with length of \code{1}. 
#' 
#' @export 
mean_wd <- function(wd, ws, na.rm = FALSE) {
  
  # Check input vector
  if (any(wd < 0, na.rm = TRUE)) {
    stop("Negative wind directions detected.", call. = FALSE)
  }
  
  if (any(wd > 360, na.rm = TRUE)) {
    stop("Wind directions greater than 360 detected.", call. = FALSE)
  }
  
  # Convert wd into radians before using trigonometry
  wd_radians <- wd * pi / 180
  
  # Calculate wind components, watch the negation
  if (missing(ws)) {
    
    # No weighting by wind speed
    wind_u <- -sin(wd_radians)
    wind_v <- -cos(wd_radians)
    
  } else {
    
    # Weight by wind speed, resultant vector average wind speed
    wind_u <- -ws * sin(wd_radians)
    wind_v <- -ws * cos(wd_radians)
    
  }

  # Mean wind components
  x_u <- mean(wind_u, na.rm = na.rm)
  x_v <- mean(wind_v, na.rm = na.rm)
  
  # Average wind speed
  x <- wind_direction_from_wind_components(x_u, x_v)
  
  return(x)
  
}


#' Function to calculate the standard deviation of wind direction based on 
#' Yamartino's 1984 method. 
#' 
#' @param wd Numeric vector representing wind direction in degrees. 
#' 
#' @param na.rm Should \code{NA}s be omitted from the calculation? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector with length of \code{1}.
#' 
#' @export 
sd_wind <- function(wd, na.rm = FALSE) {
  
  # Check input vector
  if (any(wd < 0, na.rm = TRUE)) {
    stop("Negative wind directions detected.", call. = FALSE)
  }
  
  if (any(wd > 360, na.rm = TRUE)) {
    stop("Wind directions greater than 360 detected.", call. = FALSE)
  }
  
  # Convert degrees to radians
  wd_radians <- wd * pi / 180
  
  # Calculate the two wind components, using negation but doe not matter for the
  # error calculation
  x_u <- mean(-sin(wd_radians), na.rm = na.rm)
  x_v <- mean(-cos(wd_radians), na.rm = na.rm)
  
  # Mean wind direction in degrees
  # atan2(x_u, x_v) * 360/2/pi + 180
  
  # Calculate sd that was selected from an optimisation process in Yamartino1984
  # The error
  error <- sqrt(1 - (x_u ^ 2 + x_v ^ 2))
  
  # Expand to Yamartino1984's standard deviation
  sd <- asin(error) * (1 + (2 / sqrt(3) - 1) * error ^ 3)
  
  # Back to degrees
  sd <- sd * 180 / pi
  
  return(sd)
  
}
