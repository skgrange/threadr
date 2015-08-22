#' Convenience function to find size of R object. 
#' 
#' \code{object_size} is a simple wrapper for \code{object.size} and 
#' \code{format} which allows for quick reutrn of an object's size in memory. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x A R object. 
#' @param unit. Units of size. See \link{object.size} for options. 
#'
#' @export
#'
object_size <- function (x, unit = "Mb") {
  
  y <- object.size(x)
  y <- format(y, unit)
  y
  
}
