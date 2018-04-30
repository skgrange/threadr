#' Functions to convert gas concentrations between volume- and mass-units.
#' 
#' Use \code{gas_volume_to_mass} when a gas concentration is in ppb or ppm and 
#' ug m-3 or mg m-3 is desired. Use \code{gas_mass_to_volume} when a gas 
#' concentration is in ug m-3 or mg m-3 and ppb or ppm is desired. 
#' 
#' These functions allow for input of temperatures and pressures which change 
#' the coefficients used for the conversion between the unit systems, they do
#' not use conversion factors. 
#' 
#' @param volume Gas concentration in ppb or ppm. 
#' 
#' @param mass Gas concentration in ug m-3 or mg m-3. 
#' 
#' @param gas A string such as \code{"o3"}, \code{"co"}, or \code{"co2"} which 
#' represents the input gas. This is used to find the molecular mass of a gas. 
#' Every gas-species will never be supported, therefore, use the 
#' \code{molecular_mass} argument if your gas is not implemented. 
#' 
#' @param molecular_mass Molecular mass of gas in g mol-1. For example, for 
#' ozone, this is 48 and for carbon monoxide this is 28.01. 
#' 
#' @param unit_input,unit_output Input and output units.
#' 
#' @param temp Default is 0 degrees Celsius. However, values such as 20 or 25 
#' degrees Celsius are often used.  
#' 
#' @param pressure Default is 101325 Pa (1 standard atmosphere). 
#' 
#' @param x Vector of gases to convert. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector.
#' 
#' @examples 
#' \dontrun{
#' 
#' # Ozone
#' # From ppb
#' gas_volume_to_mass(400, "o3")
#' 
#' # To ug m-3
#' gas_mass_to_volume(800, "o3")
#' 
#' # For monitoring sites in Europe where conversions are done at 20 degreees
#' gas_mass_to_volume(2.24, "no", temp = 20, unit_output = "ppb")
#' gas_mass_to_volume(24.53, "no2", temp = 20, unit_output = "ppb")
#' gas_mass_to_volume(37.74, "o3", temp = 20, unit_output = "ppb")
#' 
#' # CO usually uses different units
#' gas_mass_to_volume(0.39, "co", unit_input = "mg_m3", temp = 20, 
#'                    unit_output = "ppb")
#' 
#' # NOx as NO2
#' gas_volume_to_mass(14.64, "no2", temp = 20, unit_input = "ppb")
#' 
#' }
#' 
#' @export
gas_volume_to_mass <- function(volume, gas, molecular_mass = NA, 
                               unit_input = "ppb", unit_output = "ug_m3",
                               temp = 0, pressure = 101325) {
  
  # Check inputs
  unit_input <- stringr::str_to_lower(unit_input)
  
  if (!unit_input %in% c("ppb", "ppm"))
    stop("Input unit must be 'ppb' or ppm.", call. = FALSE)
  
  # Transform input units
  # To ppm to ppb
  if (unit_input == "ppm") volume <- ppm_to_ppb(volume)
  
  # Get molecular mass
  if (is.na(molecular_mass)) {
    
    molecular_mass <- gas_string_to_mass(gas)
    
  } else {
    
    # or use argument
    molecular_mass <- molecular_mass
    
  }
  
  # Get coefficient
  molecular_volume <- calculate_molecular_volume(temp, pressure)
  
  # The conversion
  mass <- volume * molecular_mass / molecular_volume
  
  # Transform output units, ug_m3 to mg_m3
  if (unit_output == "mg_m3") mass <- microgram_to_milligram(mass)
  
  return(mass)
  
}


#' @rdname gas_volume_to_mass
#' @export
gas_mass_to_volume <- function(mass, gas, molecular_mass = NA, 
                               unit_input = "ug_m3", unit_output = "ppb",
                               temp = 0, pressure = 101325) {
  
  # Check inputs
  unit_input <- stringr::str_to_lower(unit_input)
  # unit_input <- stringr::str_replace(unit_input, "mu", "u")
  
  if (!unit_input %in% c("ug_m3", "mg_m3"))
    stop("Input unit must be 'ug_m3' or mg_m3", call. = FALSE)
  
  # Transform input units
  # ug_m3 to mg_m3
  if (unit_input == "mg_m3") mass <- milligram_to_microgram(mass)
  
  # Get molecular mass
  if (is.na(molecular_mass)) {
    
    molecular_mass <- gas_string_to_mass(gas)
    
  } else {
    
    # or use argument
    molecular_mass <- molecular_mass
    
  }
  
  # Get coefficient
  molecular_volume <- calculate_molecular_volume(temp, pressure)
  
  # The conversion
  volume <- mass / (molecular_mass / molecular_volume)
  
  # Transform output units
  # To ppm to ppb
  if (unit_output == "ppm") volume <- ppb_to_ppm(volume)
  
  return(volume)
  
}


# No export
calculate_molecular_volume <- function(temp, pressure)
  22.41 * (273.15 + temp) / 273.15 *  101325 / pressure


# No export
gas_string_to_mass <- function(gas) {
  
  # Check
  stopifnot(length(gas) == 1)
  
  # Parse
  gas <- stringr::str_to_lower(gas)
  gas <- stringr::str_trim(gas)
  
  # Used for testing if gas is supported
  mass <- NA
  
  # Switch
  if (gas == "o3") mass <- 48
  
  if (gas == "co") mass <- 28.01
  
  if (gas == "no") mass <- 30.01
  
  if (gas == "no2") mass <- 46.0055
  
  # Use 100 % no2, may need altering for different fractions at some point
  if (gas == "nox") mass <- 46.0055
  
  if (gas == "co2") mass <- 44.01
  
  if (gas == "so2") mass <- 64.066
  
  if (gas == "h2s") mass <- 34.08
  
  if (gas == "benzene") mass <- 78.11
  
  if (gas %in% c("ch4", "methane")) mass <- 16.01
  
  # Oxygen O or O2? Use O2 here
  if (gas == "o2") mass <- 31.9988
  
  if (gas == "nh3") mass <- 17.031
  
  if (gas %in% c("ethane", "c2h6")) mass <- 30.07
  
  if (gas %in% c("propane", "c3h8")) mass <- 44.1
  
  # Check if conversion has occured
  if (is.na(mass)) 
    stop("`gas` not supported, use the `molecular_mass` argument...", call. = FALSE)
  
  return(mass)
  
}


#' @rdname gas_volume_to_mass
#' 
#' @export
milligram_to_microgram <- function(x) x * 1000

#' @rdname gas_volume_to_mass
#' 
#' @export
microgram_to_milligram <- function(x) x / milligram_to_microgram(1)

#' @rdname gas_volume_to_mass
#' 
#' @export
ppb_to_ppm <- function(x) x / 1000

#' @rdname gas_volume_to_mass
#' 
#' @export
ppm_to_ppb <- function(x) x / ppb_to_ppm(1)


#' @rdname gas_volume_to_mass
#' 
#' @export
ppt_to_ppb <- function(x) x / 1000


#' @rdname gas_volume_to_mass
#' 
#' @export
ppb_to_ppt <- function(x) x / ppt_to_ppb(1)
