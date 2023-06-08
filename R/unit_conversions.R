#' Functions to convert unit systems. 
#' 
#' No rounding is ever done. 
#' 
#' @param x Input vector. 
#' 
#' @param type,metric Type options. 
#' 
#' @param temp,rh,rh_min Specific options for some functions. 
#' 
#' @author Stuart K. Grange
#' 
#' @rdname miles_to_km
#' @export
miles_to_km <- function(x) x * 1.609344

#' @rdname miles_to_km
#' @export
km_to_miles <- function(x) x / miles_to_km(1)

#' @rdname miles_to_km
#' @export
n_mile_to_km <- function(x) x * 1.852

#' @rdname miles_to_km
#' @export
km_to_n_mile <- function(x) x / n_mile_to_km(1)

#' @rdname miles_to_km
#' @export
ms_to_km_h <- function(x) x * 3.6

#' @rdname miles_to_km
#' @export
km_h_to_ms <- function(x) x / ms_to_km_h(1)

#' @rdname miles_to_km
#' @export
km_h_to_km_min <- function(x) x * (1 / 60)

#' @rdname miles_to_km
#' @export
km_h_to_min_km <- function(x) 60 / x

#' @rdname miles_to_km
#' @export
min_km_to_km_h <- function(x) km_h_to_min_km(x) / 1

#' @rdname miles_to_km
#' @export
inch_to_mm <- function(x) x * 25.4

#' @rdname miles_to_km
#' @export
mm_to_inch <- function(x) x / inch_to_mm(1)

#' @rdname miles_to_km
#' @export
foot_to_metre <- function(x) x * 0.3048

#' @rdname miles_to_km
#' @export
feet_to_metres <- foot_to_metre

#' @rdname miles_to_km
#' @export
metre_to_foot <- function(x) x / foot_to_metre(1)

#' @rdname miles_to_km
#' @export
metres_to_feet <- metre_to_foot

#' @rdname miles_to_km
#' @export
yard_to_metre <- function(x) 0.9144 * x

#' @rdname miles_to_km
#' @export
metre_to_yard <- function(x) x / yard_to_metre(1)

#' @rdname miles_to_km
#' @export
ms_to_knot <- function(x) x / (1.852 / 3.6)

#' @rdname miles_to_km
#' @export
knot_to_ms <- function(x) x / ms_to_knot(1)


# Volume
#' @rdname miles_to_km
#' @export
gallon_to_litre <- function(x, type = "imperial") {
  
  if (type == "imperial") {
    x <- x * 4.54609
  } else if (type == "us") {
    x <- x * 3.785411784
  }
  
  return(x)
  
}

#' @rdname miles_to_km
#' @export
litre_to_gallon <- function(x, type = "imperial") {
  x / gallon_to_litre(1, type = type)
}


# Fuel consumption
#' @export
#' @rdname miles_to_km
mpg_to_km_l <- function(x, type = "imperial") 
  x * miles_to_km(1) / gallon_to_litre(1, type = type)

#' @export
#' @rdname miles_to_km
mpg_to_l_100_km <- function(x, type = "imperial")
  100 / mpg_to_km_l(x, type)

#' @export
#' @rdname miles_to_km
l_100_km_to_km_l <- function(x) 100 / x # just the reciprocal

#' @export
#' @rdname miles_to_km
l_100km_to_mpg <- function(x, type = "imperial") mpg_to_l_100_km(1, type) / x

#' @export
#' @rdname miles_to_km
km_l_to_l_100_km <- function(x) 100 / x


#' @export
#' @rdname miles_to_km
cubic_inches_to_cubic_centimetres <- function(x) x * 16.387064

#' @export
#' @rdname miles_to_km
cubic_centimetres_to_cubic_inches <- function(x) x / cubic_inches_to_cubic_centimetres(1)


# Pressure
#' @export
#' @rdname miles_to_km
bar_to_psi <- function(x) x * 14.5037738007

#' @export
#' @rdname miles_to_km
psi_to_bar <- function(x) x / bar_to_psi(1)

#' @export
#' @rdname miles_to_km
inch_hg_to_mb <- function(x) x * 33.8638866667

#' @export
#' @rdname miles_to_km
mb_to_inch_hg <- function(x) x / inch_hg_to_mb(1)

#' @export
#' @rdname miles_to_km
pascal_to_psi <- function(x) x * bar_to_psi(1) / 100000

#' @export
#' @rdname miles_to_km
psi_to_pascal <- function(x) psi_to_bar(x) * 100000


# Temperatures
#' @export
#' @rdname miles_to_km
fahrenheit_to_celsius <- function(x) (x - 32) * (5 / 9)

#' @export
#' @rdname miles_to_km
celsius_to_fahrenheit <- function(x) x * 9 / 5 + 32


#' @export
#' @rdname miles_to_km
heat_index <- function(temp, rh, rh_min = NA) {
  
  # This function only takes temperature in degrees Celsius and percentatges for
  # relative humidity
  
  # Convert to Fahrenheit, the formula requires this
  temp <- celsius_to_fahrenheit(temp)
  
  # Calculate the heat index
  # https://pro.arcgis.com/en/pro-app/latest/help/analysis/raster-functions/heat-index.htm
  x <- (-42.379 + (2.04901523 * temp) + (10.14333127 * rh) -
          (0.22475541 * temp * rh)  - (6.83783e-3 * temp^2) - 
          (5.481717e-2 * rh^2) + (1.22874e-3 * temp^2 * rh) + 
          (8.5282e-4 * temp * rh^2) - (1.99e-6 * temp^2 * rh^2))
  
  # The heat index with a tempature of under 81 deg. F is invalid
  x <- if_else(temp >= 81, x, temp)
  
  # Apply a rh catch too
  if (!is.na(rh_min)) {
    x <- if_else(rh >= rh_min, x, temp)
  }
  
  # Push back to Celsius
  x <- fahrenheit_to_celsius(x)
  
  return(x)
  
}


# Power
#' @export
#' @rdname miles_to_km
kw_to_hp <- function(x, metric = FALSE) {
  
  if (metric) {
    
    # Mechanical horse power
    y <- x * 1 / 0.74569987158227
    
  } else {
    
    # "Metric" horse power, PS auf Deutsch
    y <- x * 1 / 0.73549875
    
  }
  
  return(y)
  
}

#' @export
#' @rdname miles_to_km
hp_to_kw <- function(x, metric = FALSE) x / kw_to_hp(1, metric)


# Torque
#' @export
#' @rdname miles_to_km
newton_metre_to_kg_force <- function(x) x / 9.80665 # standard gravity

#' @export
#' @rdname miles_to_km
newton_metre_to_foot_pound <- function(x) x / 1.35581794833

#' @export
#' @rdname miles_to_km
foot_pound_to_newton_meter <- function(x) x / newton_metre_to_foot_pound(1)


# Mass
#' @export
#' @rdname miles_to_km
pound_to_kg <- function(x) x * 0.45359237

#' @export
#' @rdname miles_to_km
lb_to_kg <- pound_to_kg

#' @export
#' @rdname miles_to_km
kg_to_pound <- function(x) x / pound_to_kg(1)

#' @export
#' @rdname miles_to_km
stone_to_kg <- function(x) x * 6.35029318

#' @export
#' @rdname miles_to_km
kg_to_stone <- function(x) x / stone_to_kg(1)


#' @export
#' @rdname miles_to_km
acre_to_metre <- function(x) 4046.8564224 * x

#' @export
#' @rdname miles_to_km
metre_to_acre <- function(x) x / acre_to_metre(1)


#' @export
#' @rdname miles_to_km
marathon <- function() 42.195

#' @export
#' @rdname miles_to_km
half_marathon <- function() marathon() / 2

#' @export
#' @rdname miles_to_km
absolute_zero <- function() -273.15

#' @export
#' @rdname miles_to_km
standard_atmosphere <- function() 101325


#' @export
#' @rdname miles_to_km
miles_h_to_ms <- function(x) x * (miles_to_km(1) * 1000 / (60 * 60))


#' @export
#' @rdname miles_to_km
miles_h_s_2_to_ms_2 <- function(x) x * miles_h_to_ms(1)


#' @export
#' @rdname miles_to_km
speed_of_light <- function() 299792458


#' @export
#' @rdname miles_to_km
gravity <- function() 9.80665


#' @export
#' @rdname miles_to_km
dry_adiabatic_lapse_rate <- function() -9.8
