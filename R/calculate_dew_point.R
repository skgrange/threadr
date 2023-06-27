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
#' @param type What type of vapour pressure should be calculated? Type \code{1} 
#' is the package's original version and type \code{2} is Bolton 1980's version
#' often used with dew point temperatures. 
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
#' 
#' @export
calculate_vapour_pressure <- function(temp, type = 1L) {
  
  if (type == 1L) {
    # The original version
    x <- 6.11 * 10 ^ (7.5 * temp / (237.3 + temp))
  } else if (type == 2L) {
    # Bolton 1980's version, usually used with dew point temperature
    x <- 6.112 * exp((17.67 * temp)/(temp + 243.5))
  } else {
    stop("`type` not recognised.", call. = FALSE)
  }
  
  return(x)
  
}


#' Function to calculate specific humidity. 
#' 
#' Specific humidity is the mass of water vapour in grams per kilogram of moist 
#' air.
#' 
#' @param temp Temperature degrees Celsius. 
#' 
#' @param rh Relative humidity in percentage.
#' 
#' @param pressure Pressure in hectopascals (hPa). 
#' 
#' @return Numeric vector, specific humidity in \code{g.kg-1}. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{absolute_humidity}}, \code{\link{calculate_dew_point}},
#' \code{\link{calculate_vapour_pressure}}, \code{\link{calculate_mixing_ratio}}
#' 
#' @examples 
#' 
#' # Calculate specific humidities at different relative humidities
#' specific_humidity(20, 95:100)
#' 
#' @export
specific_humidity <- function(temp, rh, pressure = 1013.25) {
  
  # Calculate dew point temperature
  dew_point <- calculate_dew_point(temp, rh)
  
  # Calculate vapour pressure
  vapour_pressure <- calculate_vapour_pressure(dew_point, type = 2L)
  
  # In kg.kg-1
  specific_humidity <- (0.622 * vapour_pressure) / 
    (pressure - (0.378 * vapour_pressure)) * 1000
  
  return(specific_humidity)
  
}


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
