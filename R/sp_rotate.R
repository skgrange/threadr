#' Function to rotate a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param amount Number of degrees to rotate object in degrees in a clock-wise
#' direction. 
#' 
#' @seealso \code{\link{elide}}
#' 
#' @export
sp_rotate <- function(sp, amount) {
  
  # Get projection string
  projection <- sp_projection(sp)
  
  # Do
  sp <- maptools::elide(sp, rotate = amount)
  
  # Add projection again
  sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  # Return
  sp
  
}
