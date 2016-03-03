#' Function to calculate oil/mixing ratios.
#' 
#' \code{oil_ratio} is useful for 2-stroke fuel-oil mixing. 
#' 
#' @param ratio Oil/mixing ratio in fuel:oil order. For example, \code{"25:1"}
#' or \code{"100:1"}. \code{ratio} can be vector. 
#' 
#' @param volume_fuel Volume of fuel in litres. If \code{volume_fuel} is not used
#' the a sequence of 1 to 10 litres will be used. \code{volume_fuel} can be 
#' vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tidy data frame with metric units. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Common mixing for garden equipment
#' oil_ratio(ratio = "50:1", volume_fuel = 5)
#' 
#' # Outboard mixing, depending on oil type and tank size
#' oil_ratio(ratio = c("50:1", "100:1"), volume_fuel = c(5, 10))
#'
#' }
#' 
#' @export
oil_ratio <- function (ratio, volume_fuel = NA) {
  
  # If volume_fuel is not used
  if (is.na(volume_fuel[1])) volume_fuel <- seq(1, 10, 1)
  
  # Replicate if necessary
  volume_fuel <- rep(volume_fuel, each = length(ratio))
  
  # Clean
  ratio <- stringr::str_replace_all(ratio, "\\.|-|;", ":")
  ratio_numeric <- as.numeric(str_split_fixed(ratio, ":", 2)[, 1])
  
  # Litres
  volume_oil <- volume_fuel / 1 / ratio_numeric
  # mL
  volume_oil_ml <- volume_oil * 1000
  
  # Build data frame
  df <- data.frame(
    ratio,
    ratio_numeric,
    volume_fuel,
    volume_oil,
    volume_oil_ml
  )
  
  # Arrange/order
  df <- dplyr::arrange(df, ratio_numeric, volume_fuel)
  
  # Return
  df
  
}
