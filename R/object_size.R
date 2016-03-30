#' Convenience function to find size of R object. 
#' 
#' \code{object_size} is a simple wrapper for \code{object.size} and 
#' \code{format} which allows for quick return of an object's size in memory. 
#' 
#' @author Stuart K. Grange
#' 
#' @param object An R object. 
#' 
#' @param unit Units of size. See \link{object.size} for options. Defaults to 
#' \code{"Mb"} for megabytes.
#'
#' @export
object_size <- function(object, unit = "Mb") {
  
  y <- object.size(object)
  y <- format(y, unit)
  y
  
}
