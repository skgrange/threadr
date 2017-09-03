#' Function to clean and convert non decimal degrees to decimal degrees. 
#' 
#' @param x Vector containing coordinates in this format: 
#' \code{53 deg 37\' 56.49\" N}, usually from \code{\link{file_metadata}}.
#' 
#' @param round Number of decimal points to round the coordinates to. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{file_metadata}}, \code{\link{dms_to_decimal}}
#' 
#' @export
clean_gps_coordinates <- function(x, round = 6) {
  
  # Split into pieces
  x <- stringr::str_split_fixed(x, " ", 5)
  x[, 3] <- stringr::str_replace(x[, 3], "'", "")
  x[, 4] <- stringr::str_replace(x[, 4], '"', "")
  
  # To decimal degrees
  coordinate <- gissr::dms_to_decimal(
    as.numeric(x[, 1]), 
    as.numeric(x[, 3]), 
    as.numeric(x[, 4])
  )
  
  # Negate nessassary
  coordinate <- ifelse(x[, 5] %in% c("W", "S"), coordinate * -1, coordinate)
  
  # Round
  if (!is.na(round)) coordinate <- round(coordinate, round)
  
  return(coordinate)
  
}
