#' Function to calculate apparent temperature. 
#' 
#' Apparent temperature was defined by Steadman (1994). There are two variants,
#' one using radiation and one without -- this function uses the non-radiation
#' variant. 
#' 
#' @author Stuart K. Grange
#' 
#' @param temp Temperature (usually air temperature) in degrees Celsius. 
#' 
#' @param rh Relative humidity in percentage. 
#' 
#' @param ws Wind speed in \code{m.s-1}. 
#' 
#' @seealso \code{\link{heat_index}}. 
#' 
#' @examples
#' 
#' # Calculate apparent temperature, some real observations from the Bernese 
#' # Jura in summer 2021 when it felt warm
#' apparent_temperature(25.7, 64.5, 0.9)
#' 
#' @export
apparent_temperature <- function(temp, rh, ws) {
  
  # Calculate a variant of actual vapour pressure
  vapour_pressure_actual <- calculate_vapour_pressure_dosseger(temp, rh)
  
  # Calculate apparent temperature, can be found in Buzan2015
  temp_apparent <- temp + 0.33 * vapour_pressure_actual - 0.7 * ws - 4
  
  return(temp_apparent)
  
}


# Pulled from the HeatStress package and edited a bit
# https://github.com/anacv/HeatStress/blob/master/R/tasrh2vap.pres.R
# Not included in the calculate_vapour_pressure function because it takes both
# temp and converts temp to dew point temp
calculate_vapour_pressure_dosseger <- function(temp, rh) {
  
  # Constants (see Dosseger et al. 1992)
  c1 <- 0.06107
  a1 <- 17.368
  b1 <- 2388.3
  c2 <- 0.06108
  a2 <- 17.856
  b2 <- 2455.2
  T0 <- 0
  
  # units
  temp <- 10 * temp
  rh[rh > 100] <- 100 # some SMN stations with rh >100
  
  iceMask <- which(temp < T0)
  waterMask <- which(temp >= T0)
  vapour_pressure <- rep(NA, length(temp))
  
  vapour_pressure[waterMask] <- rh[waterMask] * c1 * exp((a1 * temp[waterMask]) / (b1 + temp[waterMask]))
  vapour_pressure[iceMask] <- rh[iceMask] * c2 * exp((a2 * temp[iceMask]) / (b2 + temp[iceMask]))
  
  return(vapour_pressure)
  
}
