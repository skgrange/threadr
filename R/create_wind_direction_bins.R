#' Function to transform numeric wind direction values (0--360) degrees to bins.
#' 
#' @param wd A numeric vector representing wind direction values. 
#' 
#' @author Stuart K. Grange
#' 
#' @return A factor vector with levels. 
#' 
#' @examples
#' 
#' # Bind wind directions
#' create_wind_direction_bins(seq(from = 0, to = 360, by = 10))
#' 
#' @export
create_wind_direction_bins <- function(wd) {
  
  # Check input
  stopifnot(inherits(wd, "numeric"))
  
  # Make the bins
  wd_bins <- cut(
    wd, 
    breaks = seq(22.5, 382.5, 45),
    labels = c("NE", "E", "SE", "S", "SW", "W", "NW", "N")
  )
  
  # To character
  wd_bins <- as.character(wd_bins)
  
  # For the first bin that is not coded for
  wd_bins <- if_else(wd <= 22.5 & is.na(wd_bins), "N", wd_bins)
  
  # To a factor with an order
  wd_bins <- factor(
    wd_bins, levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  )
  
  return(wd_bins)
  
}
