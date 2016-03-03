#' Functions to convert unit systems. 
#' 
#' No rounding is ever done. 
#' 
#' @author Stuart K. Grange
#' 
#' @rdname miles_to_km
#' @export
miles_to_km <- function (x) x * 1.609344

#' @rdname miles_to_km
#' @export
km_to_miles <- function (x) x / miles_to_km(1)

#' @rdname miles_to_km
#' @export
n_mile_to_km <- function (x) x * 1.852

#' @rdname miles_to_km
#' @export
km_to_n_mile <- function (x) x / n_mile_to_km(1)

#' @rdname miles_to_km
#' @export
ms_to_km_h <- function (x) x * 3.6

#' @rdname miles_to_km
#' @export
km_h_to_ms <- function (x) x / ms_to_km_h(1)

#' @rdname miles_to_km
#' @export
km_h_to_km_min <- function (x) x * (1 / 60)

#' @rdname miles_to_km
#' @export
km_h_to_min_km <- function (x) 60 / x

#' @rdname miles_to_km
#' @export
min_km_to_km_h <- function (x) km_h_to_min_km(x) / 1

#' @rdname miles_to_km
#' @export
inch_to_mm <- function (x) x * 25.4

#' @rdname miles_to_km
#' @export
mm_to_inch <- function (x) x / inch_to_mm(1)

#' @rdname miles_to_km
#' @export
foot_to_metre <- function (x) x * 0.3048

#' @rdname miles_to_km
#' @export
metre_to_foot <- function (x) x / foot_to_metre(1)


# Volume
#' @rdname miles_to_km
#' @export
gallon_to_litre <- function (x, type = "imperial") {
  
  if (type == "imperial")
    x <- x * 4.54609
  
  if (type == "us") 
    x <- x * 3.785411784
  
  # Return
  x
  
}

#' @rdname miles_to_km
#' @export
litre_to_gallon <- function (x, type = "imperial") 
  x / gallon_to_litre(1, type = type)


# Fuel consumption
#' @export
#' @rdname miles_to_km
mpg_to_km_l <- function (x, type = "imperial") 
  x * miles_to_km(1) / gallon_to_litre(1, type = type)

#' @export
#' @rdname miles_to_km
mpg_to_l_100_km <- function (x, type = "imperial")
  100 / mpg_to_km_l(x, type)

#' @export
#' @rdname miles_to_km
l_100_km_to_km_l <- function (x) 100 / x # just the reciprocal

# Metric to other systems after talking to John
#' @export
#' @rdname miles_to_km
l_100km_to_mpg <- function (x, type = "imperial") mpg_to_l_100_km(1, type) / x


#' @export
#' @rdname miles_to_km
cubic_inches_to_cubic_centimetres <- function (x) x * 16.387064

#' @export
#' @rdname miles_to_km
cubic_centimetres_to_cubic_inches <- function (x) x / cubic_inches_to_cubic_centimetres(1)


# Pressure
#' @export
#' @rdname miles_to_km
bar_to_psi <- function (x) x * 14.5037738007

#' @export
#' @rdname miles_to_km
psi_to_bar <- function (x) x / bar_to_psi(1)

#' @export
#' @rdname miles_to_km
inch_hg_to_mb <- function (x) x * 33.8638866667

#' @export
#' @rdname miles_to_km
mb_to_inch_hg <- function (x) x / inch_hg_to_mb(1)


# Temperatures
#' @export
#' @rdname miles_to_km
fahrenheit_to_celsius <- function (x) (x - 32) * (5 / 9)

#' @export
#' @rdname miles_to_km
celsius_to_fahrenheit <- function (x) x * 9 / 5 + 32


# https://github.com/cran/weathermetrics
#' @export
#' @rdname miles_to_km
heat_index <- function (temp, rh, unit = "c") {
  
  if (unit == "c") 
    temp <- celsius_to_fahrenheit(temp)
  
  heat.index <- mapply(heat_index_calculation, temp, rh)
  
  if (unit == "c")
    heat.index <- fahrenheit_to_celsius(heat.index)
  
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


# Power
#' @export
#' @rdname miles_to_km
kw_to_hp <- function (x, metric = FALSE) {
  
  if (metric) {
    
    # Mechanical horse power
    y <- x * 1 / 0.74569987158227
    
  } else {
    
    # "Metric" horse power, PS auf Deutsch
    y <- x * 1 / 0.73549875
    
  }
  
  # Return
  y
  
}

#' @export
#' @rdname miles_to_km
hp_to_kw <- function (x, metric = FALSE) x / kw_to_hp(1, metric)


# Torque
#' @export
#' @rdname miles_to_km
newton_metre_to_kg_force <- function (x) x / 9.80665 # standard gravity

#' @export
#' @rdname miles_to_km
newton_metre_to_foot_pound <- function (x) x / 1.35581794833

#' @export
#' @rdname miles_to_km
foot_pound_to_newton_meter <- function (x) x / newton_metre_to_foot_pound(1)


# Mass
#' @export
#' @rdname miles_to_km
pound_to_kg <- function (x) x * 0.45359237

#' @export
#' @rdname miles_to_km
lb_to_kg <- pound_to_kg

#' @export
#' @rdname miles_to_km
kg_to_pound <- function (x) x / pound_to_kg(1)

#' @export
#' @rdname miles_to_km
stone_to_kg <- function (x) x * 6.35029318

#' @export
#' @rdname miles_to_km
kg_to_stone <- function (x) x / stone_to_kg(1)
