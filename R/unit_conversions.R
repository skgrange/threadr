#' Functions to convert unit systems. 
#' 
#' @author Stuart K. Grange
#' 
#' 
#' @rdname miles_to_kilometres
#' @export
miles_to_kilometres <- function (x) x * 1.609344

#' @rdname miles_to_kilometres
#' @export
kilometres_to_miles <- function (x) miles_to_kilometres(x) / 1


# Volume
#' @rdname miles_to_kilometres
#' @export
imperial_gallon_to_litres <- function (x) x * 4.54609

#' @rdname miles_to_kilometres
#' @export
litres_to_imperial_gallon <- function (x) imperial_gallon_to_litres(x) / 1

# US
#' @rdname miles_to_kilometres
#' @export
us_gallon_to_litres <- function (x) x * 3.785411784

#' @rdname miles_to_kilometres
#' @export
litres_to_us_gallon <- function (x) us_gallon_to_litres(x) / 1


# Fuel consumption
#' @rdname miles_to_kilometres
#' @export
mpg_to_l_100_km <- function (mpg, type = "imperial") {
  
  if (type == "imperial") {
    metric <- 100 / (mpg * miles_to_kilometres(1) / imperial_gallon_to_litres(1))
  }
  
  if (type == "us") {
    metric <- 100 / (mpg * miles_to_kilometres(1) / us_gallon_to_litres(1))
  }
  
  # Return
  metric
  
} 

#' @export
#' @rdname miles_to_kilometres
mpg_to_km_l <- function (mpg, type = "imperial") {
  
  if (type == "imperial") {
    metric <-  mpg * miles_to_kilometres(1) / imperial_gallon_to_litres(1)
  }
  
  if (type == "us") {
    metric <-  mpg * miles_to_kilometres(1) / us_gallon_to_litres(1)
  }
  
  # Return
  metric

}  


#' @export
#' @rdname miles_to_kilometres
cubic_inches_to_cubic_centimetres <- function (x) x * 16.387064

#' @export
#' @rdname miles_to_kilometres
cubic_centimetres_to_cubic_inches <- function (x) x / cubic_inches_to_cubic_centimetres(1)


# Pressure
#' @export
#' @rdname miles_to_kilometres
bar_to_psi <- function (x) x * 14.5037738007

#' @export
#' @rdname miles_to_kilometres
psi_to_bar <- function (x) x / bar_to_psi(1)


# Temperatures
#' @export
#' @rdname miles_to_kilometres
fahrenheit_to_celsius <- function (x) (x - 32) / (9 / 5)

#' @export
#' @rdname miles_to_kilometres
celsius_to_fahrenheit <- function (x) x * 9 / 5 + 32


# https://github.com/cran/weathermetrics
#' @export
#' @rdname miles_to_kilometres
heat_index <- function (temp, rh, unit = "c") {
  
  if (unit == "c") {
    temp <- celsius_to_fahrenheit(temp)
  }
  
  heat.index <- mapply(heat_index_calculation, temp, rh)
  
  if (unit == "c") {
    heat.index <- fahrenheit_to_celsius(heat.index)
  }
  
  # Return
  heat.index
  
}

# No export
heat_index_calculation <- function (t = NA, rh = NA) {
  
  if (is.na(rh) | is.na(t)) {
    
    hi <- NA
    
  } else 
    
    if (t <= 40) {
      
      hi <- t
      
    } else {
      
      alpha <- 61 + ((t - 68) * 1.2) + (rh * 0.094)
      hi <- 0.5*(alpha + t)
      
      if (hi > 79) {
        
        hi <- -42.379 + 2.04901523 * t + 10.14333127 * rh - 
          0.22475541 * t * rh - 6.83783 * 10^-3 * t^2 - 
          5.481717 * 10^-2 * rh^2 + 1.22874 * 10^-3 * t^2 * 
          rh + 8.5282 * 10^-4 * t * rh^2 - 1.99 * 10^-6 * 
          t^2 * rh^2
        
        if (rh <= 13 & t >= 80 & t <= 112) {
          
          adjustment1 <- (13 - rh) / 4
          adjustment2 <- sqrt((17 - abs(t - 95)) / 17)
          total.adjustment <- adjustment1 * adjustment2
          hi <- hi - total.adjustment
          
        } else 
          
          if (rh > 85 & t >= 80 & t <= 87) {
            
            adjustment1 <- (rh - 85) / 10
            adjustment2 <- (87 - t) / 5
            total.adjustment <- adjustment1 * adjustment2
            hi <- hi + total.adjustment
            
          }
      }
    }
  
  # Return
  hi
  
}
