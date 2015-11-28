#' Functions to convert gas concentrations between volume and mass unit systems.
#' 
#' Use \code{gas_volume_to_mass} when a gas concentration is in ppb or ppm and 
#' ug m-3 or mg m-3 is desired. 
#' Use \code{gas_mass_to_volume} when a gas concentration is in ug m-3 or mg m-3
#' and ppb or ppm is desired. 
#' 
#' These functions allow for input of temperatures and pressures which change the
#' coefficients used for the conversion between the unit systems.  
#' 
#' @param volume Gas concentration in ppb or ppm. 
#' 
#' @param mass Gas concentration in ug m-3 or mg m-3. 
#' 
#' @param gas A string such as \code{"o3"}, \code{"co"}, or \code{"co2"} which 
#' represents the input gas. This is used to find the molecular mass of a gas. 
#' Every gas-species will never be supported, therefore, use the 
#' \code{molecular_mass} argument if this needs to be overridden. 
#' 
#' @param molecular_mass Molecular mass of gas in g mol-1. For example, for 
#' ozone, this is 48.
#' 
#' @param unit_input 
#' 
#' @param unit_output
#' 
#' @param temperature Default is 0 degrees Celsius. However, values such as 20
#' or 25 degrees Celsius are often used too. 
#' 
#' @param pressure Default is 101325 Pa (a standard atmosphere). 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso
#' 
#' @examples 
#' \dontrun{
#' gas_volume_to_mass(400, "o3")
#' 
#' gas_mass_to_volume(800, "o3")
#' 
#' }
#' 
#' @export
gas_volume_to_mass <- function (volume, gas, molecular_mass = NA, 
                                unit_input = "ppb", unit_output = "ug_m3",
                                temperature = 0, pressure = 101325) {
  
  # Transform input units
  if (unit_input == "ppm") {
    # To ppm to ppb
    volume <- volume * 1000
  }
  
  # Get molecular mass
  if (is.na(molecular_mass)) {
    molecular_mass <- gas_string_to_mass(gas)
    
  } else {
    # or use argument
    molecular_mass <- molecular_mass
    
  }
  
  # Get coefficient
  molecular_volume <- calculate_molecular_volume(temperature, pressure)
  
  # The conversion
  mass <- volume * molecular_mass / molecular_volume
  
  # Transform output units
  if (unit_output == "mg_m3") {
    # ug_m3 to mg_m3
    mass <- mass / 1000
  }
  
  # Return
  mass
  
}


# No export
calculate_molecular_volume <- function (temperature, pressure) {
  
  # Static coefficient
  coefficient <- 22.41
  
  # The algorithm, watch the units
  volume <- coefficient * (273.15 + temperature) / 273.15 *  101325 / pressure
  
  # Return
  volume
  
}


# No export
gas_string_to_mass <- function (gas) {
  
  # Parse
  gas <- stringr::str_to_lower(gas)
  gas <- stringr::str_trim(gas)
  
  # Switch
  if (gas == "o3") {
    mass <- 48
  }
  
  if (gas == "co") {
    mass <- 28.01
  }
  
  if (gas == "no") {
    mass <- 30.01
  }
  
  if (gas == "no2") {
    mass <- 46.0055
  }
  
  # Use 100 % no2, may need altering for different fractions at some point
  if (gas == "nox") {
    mass <- 46.0055
  }
  
  if (gas == "co2") {
    mass <- 44.01
  }
  
  if (gas == "so2") {
    mass <- 64.066
  }
  
  if (gas == "h2s") {
    mass <- 34.08
  }
  
  if (gas == "benzene") {
    mass <- 78.11
  }
  
  # Return
  mass
  
}


#' @rdname gas_volume_to_mass
#' @export
gas_mass_to_volume <- function (mass, gas, molecular_mass = NA, 
                                unit_input = "ug_m3", unit_output = "ppb",
                                temperature = 0, pressure = 101325) {
  
  # Transform input units
  if (unit_input == "mg_m3") {
    # ug_m3 to mg_m3
    mass <- mass / 1000
  }
  
  # Get molecular mass
  if (is.na(molecular_mass)) {
    molecular_mass <- gas_string_to_mass(gas)
    
  } else {
    # or use argument
    molecular_mass <- molecular_mass
    
  }
  
  # Get coefficient
  molecular_volume <- calculate_molecular_volume(temperature, pressure)
  
  # The conversion
  volume <- mass / (molecular_mass / molecular_volume)
  
  # Transform output units
  if (unit_output == "ppm") {
    # To ppm to ppb
    volume <- volume * 1000
  }
  
  # Return
  volume
  
}
