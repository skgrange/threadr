#' Function to convert coordinate systems. 
#' 
#' @author Stuart K. Grange
#'
#' @export
#' 
convertCoordinateSystem <- function(x, to = '+init=epsg:4326') {
  
  # Convert coordinate system
  new.projection <- sp::CRS(to)
  
  # Apply conversion
  x <- sp::spTransform(x, new.projection)
  
  # Return 
  x
  
}
