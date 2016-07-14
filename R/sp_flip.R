#' Function to flip a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @seealso \code{\link{elide}}
#' 
#' @export
sp_flip <- function(sp) {
  
  # Get projection string
  projection <- sp_projection(sp)
  
  # Do
  sp <- maptools::elide(sp, flip = TRUE)
  
  # Add projection again
  sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  # Return
  sp
  
}
