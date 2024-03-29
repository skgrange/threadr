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
#' @seealso \code{\link{molecular_mass_table}}
#' 
#' @examples 
#' 
#' # Ozone
#' # From ppb
#' gas_volume_to_mass(400, "o3")
#' 
#' # To ug m-3
#' gas_mass_to_volume(800, "o3")
#' 
#' # For monitoring sites in Europe where conversions are done at 20 degrees
#' gas_mass_to_volume(2.24, "no", temp = 20, unit_output = "ppb")
#' gas_mass_to_volume(24.53, "no2", temp = 20, unit_output = "ppb")
#' gas_mass_to_volume(37.74, "o3", temp = 20, unit_output = "ppb")
#' 
#' # CO usually uses different units
#' gas_mass_to_volume(
#'   mass = 0.39, 
#'   gas = "co", 
#'   unit_input = "mg_m3",
#'   temp = 20, 
#'   unit_output = "ppb"
#'  )
#' 
#' # NOx as NO2
#' gas_volume_to_mass(14.64, "no2", temp = 20, unit_input = "ppb")
#' 
#' @export
gas_volume_to_mass <- function(volume, gas, molecular_mass = NA, 
                               unit_input = "ppb", unit_output = "ug_m3",
                               temp = 0, pressure = 101325) {
  
  # Check inputs
  unit_input <- stringr::str_to_lower(unit_input)
  
  if (!unit_input %in% c("ppb", "ppm")) {
    stop("Input unit must be `ppb` or `ppm`.", call. = FALSE)
  }
  
  # Transform input units
  # To ppm to ppb
  if (unit_input == "ppm") volume <- ppm_to_ppb(volume)
  
  # Get molecular mass
  molecular_mass <- find_molecular_mass_from_string(molecular_mass, gas)
  
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
  
  if (!unit_input %in% c("ug_m3", "mg_m3")) {
    stop("Input unit must be `ug_m3` `or mg_m3`.", call. = FALSE)
  }
  
  # Transform input units
  # ug_m3 to mg_m3
  if (unit_input == "mg_m3") mass <- milligram_to_microgram(mass)
  
  # Get molecular mass
  molecular_mass <- find_molecular_mass_from_string(molecular_mass, gas)
  
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
calculate_molecular_volume <- function(temp, pressure) {
  22.41 * (273.15 + temp) / 273.15 *  101325 / pressure
}


find_molecular_mass_from_string <- function(molecular_mass, gas) {
  
  if (is.na(molecular_mass)) {
    
    # Find molecular mass by using a look-up table
    molecular_mass <- molecular_mass_table(unique_names = TRUE) %>% 
      filter(name == !!clean_gas_string(gas)) %>% 
      pull(molecular_mass)
    
    # Check for no match
    if (length(molecular_mass) == 0) {
      stop("`gas` not supported, use the `molecular_mass` argument.", call. = FALSE)
    }
    
    # Check for multiple matches
    if (length(molecular_mass) >= 2) {
      stop("Multiple `gas` matches returned, check look-up table.", call. = FALSE)
    }
    
  } else {
    # or use argument
    molecular_mass <- molecular_mass
  }
  
  return(molecular_mass)
  
}


clean_gas_string <- function(x) {
  
  # Check
  if (length(x) != 1) {
    stop("Only one `gas` can be supplied.", call. = FALSE)
  }
  
  # Parse to ensure formatting is fixed
  x <- x %>%
    stringr::str_to_lower() %>%
    stringr::str_trim()
  
  return(x)
  
}


#' Functions to return a table containing molecular masses for use in 
#' \code{\link{gas_volume_to_mass}} and \code{\link{gas_mass_to_volume}}.
#' 
#' @param unique_names Should only unique names be returned? 
#' 
#' @author Stuart K. Grange and Shona Wilde.
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{gas_volume_to_mass}}, \code{\link{gas_mass_to_volume}}
#' 
#' @examples 
#' 
#' # Load and print entire table
#' molecular_mass_table() %>% 
#'   print(n = Inf)
#' 
#' @export
molecular_mass_table <- function(unique_names = TRUE) {
  
  df <- tibble::tribble(
    ~name,                    ~name_common,             ~group,        ~molecular_mass, ~unique_name, ~notes,                                                                   
    "o3",                     NA_character_,            NA_character_, 48,              NA_integer_,     NA_character_,                                                            
    "co",                     NA_character_,            NA_character_, 28.01,           NA_integer_,     NA_character_,                                                            
    "no",                     NA_character_,            NA_character_, 30.01,           NA_integer_,     NA_character_,                                                            
    "no2",                    NA_character_,            NA_character_, 46.0055,         NA_integer_,     NA_character_,                                                            
    "nox",                    NA_character_,            NA_character_, 46.0055,         NA_integer_,     "Use 100 % no2 & may need altering for different fractions at some point",
    "co2",                    NA_character_,            NA_character_, 44.01,           NA_integer_,     NA_character_,                                                            
    "so2",                    NA_character_,            NA_character_, 64.066,          NA_integer_,     NA_character_,                                                            
    "h2s",                    NA_character_,            NA_character_, 34.08,           NA_integer_,     NA_character_,                                                            
    "ch4",                    "methane",                NA_character_, 16.01,           NA_integer_,     NA_character_,                                                            
    "methane",                "methane",                NA_character_, 16.01,           NA_integer_,     NA_character_,                                                            
    "o2",                     "oxygen",                 NA_character_, 31.9988,         NA_integer_,     "O or O2? & using O2 here",                                               
    "nh3",                    "ammonia",                NA_character_, 17.031,          NA_integer_,     NA_character_,                                                            
    "ethane",                 "ethane",                 "vocs",        30.07,           NA_integer_,     NA_character_,                                                            
    "c2h6",                   "ethane",                 "vocs",        30.07,           NA_integer_,     NA_character_,                                                            
    "ethene",                 "ethene",                 "vocs",        28.05,           NA_integer_,     NA_character_,                                                            
    "c2h4",                   "ethene",                 "vocs",        28.05,           NA_integer_,     NA_character_,                                                            
    "acetylene",              "ethyne",                 "vocs",        26.04,           NA_integer_,     NA_character_,                                                            
    "ethyne",                 "ethyne",                 "vocs",        26.04,           NA_integer_,     NA_character_,                                                            
    "c2h2",                   "ethyne",                 "vocs",        26.04,           NA_integer_,     NA_character_,                                                            
    "propane",                "propane",                "vocs",        44.1,            NA_integer_,     NA_character_,                                                            
    "c3h8",                   "propane",                "vocs",        44.1,            NA_integer_,     NA_character_,                                                            
    "propene",                "propene",                "vocs",        42.08,           NA_integer_,     NA_character_,                                                            
    "c3h6",                   "propene",                "vocs",        42.08,           NA_integer_,     NA_character_,                                                            
    "iso-butane",             "iso-butane",             "vocs",        58.12,           NA_integer_,     NA_character_,                                                            
    "i-butane",               "iso-butane",             "vocs",        58.12,           NA_integer_,     NA_character_,                                                            
    "2-methylpropane",        "iso-butane",             "vocs",        58.12,           NA_integer_,     NA_character_,                                                            
    "n-butane",               "iso-butane",             "vocs",        58.12,           NA_integer_,     NA_character_,                                                            
    "c4h10",                  "iso-butane",             "vocs",        58.12,           NA_integer_,     NA_character_,                                                            
    "trans-2-butene",         "2-butene",               "vocs",        56.106,          NA_integer_,     NA_character_,                                                            
    "2-butene",               "2-butene",               "vocs",        56.106,          NA_integer_,     NA_character_,                                                            
    "c4h8",                   "2-butene",               "vocs",        56.106,          NA_integer_,     NA_character_,                                                            
    "cis-2-butene",           "2-butene",               "vocs",        56.1,            NA_integer_,     NA_character_,                                                            
    "2-butene",               "2-butene",               "vocs",        56.1,            0,            "different isomers & users need to use the specific name",                
    "c4h8",                   "2-butene",               "vocs",        56.1,            0,            "different isomers & users need to use the specific name",                
    "but-1-ene",              "1-butene",               "vocs",        56.11,           NA_integer_,     NA_character_,                                                            
    "1-butene",               "1-butene",               "vocs",        56.11,           NA_integer_,     NA_character_,                                                            
    "butene",                 "1-butene",               "vocs",        56.11,           NA_integer_,     NA_character_,                                                            
    "iso-pentane",            "iso-pentane",            "vocs",        72.15,           NA_integer_,     NA_character_,                                                            
    "i-pentane",              "iso-pentane",            "vocs",        72.15,           NA_integer_,     NA_character_,                                                            
    "2-methylbutane",         "iso-pentane",            "vocs",        72.15,           NA_integer_,     NA_character_,                                                            
    "n-pentane",              "n-pentane",              "vocs",        72.15,           NA_integer_,     NA_character_,                                                            
    "c5h12",                  "n-pentane",              "vocs",        72.15,           NA_integer_,     NA_character_,                                                            
    "1,3-butadiene",          "1,3-butadiene",          "vocs",        54.0916,         NA_integer_,     NA_character_,                                                            
    "butadiene",              "1,3-butadiene",          "vocs",        54.0916,         NA_integer_,     NA_character_,                                                            
    "trans-2-pentene",        "2-pentene",              "vocs",        70.13,           NA_integer_,     NA_character_,                                                            
    "2-pentene",              "2-pentene",              "vocs",        70.13,           NA_integer_,     NA_character_,                                                            
    "pent-1-ene",             "pentene",                "vocs",        70.13,           NA_integer_,     NA_character_,                                                            
    "pentene",                "pentene",                "vocs",        70.13,           NA_integer_,     NA_character_,                                                            
    "c5h10",                  "pentene",                "vocs",        70.13,           NA_integer_,     NA_character_,                                                            
    "2,3-methyl pentanes",    "2,3-methyl pentanes",    "vocs",        86.1754,         NA_integer_,     NA_character_,                                                            
    "2,3-dimethylbutane",     "2,3-methyl pentanes",    "vocs",        86.1754,         NA_integer_,     NA_character_,                                                            
    "hexane",                 "hexane",                 "vocs",        86.18,           NA_integer_,     NA_character_,                                                            
    "c6h14",                  "hexane",                 "vocs",        86.18,           NA_integer_,     NA_character_,                                                            
    "isoprene",               "isoprene",               "vocs",        68.12,           NA_integer_,     NA_character_,                                                            
    "c5h8",                   "isoprene",               "vocs",        68.12,           NA_integer_,     NA_character_,                                                            
    "heptane",                "heptane",                "vocs",        100.21,          NA_integer_,     NA_character_,                                                            
    "n-heptane",              "heptane",                "vocs",        100.21,          NA_integer_,     NA_character_,                                                            
    "c7h16",                  "heptane",                "vocs",        100.21,          NA_integer_,     NA_character_,                                                            
    "benzene",                "benzene",                "vocs",        78.11,           NA_integer_,     NA_character_,                                                            
    "c6h6",                   "benzene",                "vocs",        78.11,           NA_integer_,     NA_character_,                                                            
    "224-TMP",                "2,2,4-trimethylpentane", "vocs",        114.232,         NA_integer_,     NA_character_,                                                            
    "2,2,4-tmp",              "2,2,4-trimethylpentane", "vocs",        114.232,         NA_integer_,     NA_character_,                                                            
    "iso-octane",             "2,2,4-trimethylpentane", "vocs",        114.232,         NA_integer_,     NA_character_,                                                            
    "2,2,4-trimethylpentane", "2,2,4-trimethylpentane", "vocs",        114.232,         NA_integer_,     NA_character_,                                                            
    "octane",                 "octane",                 "vocs",        114.23,          NA_integer_,     NA_character_,                                                            
    "n-octane",               "octane",                 "vocs",        114.23,          NA_integer_,     NA_character_,                                                            
    "toluene",                "toluene",                "vocs",        92.14,           NA_integer_,     NA_character_,                                                            
    "methylbenzene",          "toluene",                "vocs",        92.14,           NA_integer_,     NA_character_,                                                            
    "c7h8",                   "toluene",                "vocs",        92.14,           NA_integer_,     NA_character_,
    "h2o",                    "water",                  NA_character_, 18.01528,        NA_integer_,     NA_character_   
  )
  
  if (unique_names) df <- filter(df, unique_name != 0L | is.na(unique_name))
  
  return(df)
  
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


#' convert_water_percent_to_mass <- function(percent, molecular_mass = 18.01528,
#'                                           temp = 0, pressure = 101325) {
#'   
#'   # Convert concentration to ppm
#'   concentration <- percent * 10000
#'   
#'   # Do the conversion to mass in ug.m-3
#'   concentration_mass <- gas_volume_to_mass(
#'     concentration, 
#'     molecular_mass = molecular_mass, 
#'     temp = temp,
#'     pressure = pressure,
#'     unit_output = "ug_m3"
#'   )
#'   
#'   # To g.m-3, the units usually used for absolute humidity
#'   concentration_mass <- concentration_mass / 1000
#'   
#'   return(concentration_mass)
#'   
#' }


#' convert_absolute_humidity_to_percent <- function(humidity, 
#'                                                  molecular_mass = 18.01528,
#'                                                  temp = 0, pressure = 101325) {
#'   
#'   # Convert input unit (assumed to be g.kg-1) to ug.m-3
#'   humidity <- humidity / 1000
#'   
#'   # Convert from mass to volume and return in ppb units
#'   concentration <- gas_mass_to_volume(
#'     humidity, 
#'     molecular_mass = molecular_mass, 
#'     temp = temp,
#'     pressure = pressure,
#'     unit_output = "ppb"
#'   )
#'   
#'   # Transform ppb to percent
#'   concentration <- concentration * 100
#'   
#'   return(concentration)
#'   
#' }
