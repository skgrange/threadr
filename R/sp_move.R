#' Function to move/shift a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param x Offset amount (in \code{sp} projection's units) in left-right 
#' dimension.
#' 
#' @param y Offset amount (in \code{sp} projection's units) in up-down 
#' dimension.
#' 
#' @seealso \code{\link{elide}}
#' 
#' @export
sp_move <- function(sp, x, y) {
  
  # Get projection string
  projection <- sp_projection(sp)
  
  # Do
  sp <- maptools::elide(sp, shift = c(x, y))   
  
  # Add projection again
  sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  # Return
  sp
  
  
}


# Wrap
#' @export
sp_shift <- sp_move