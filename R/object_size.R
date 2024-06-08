#' Convenience function to find size of R object. 
#' 
#' \code{object_size} is a simple wrapper for \code{object.size} and 
#' \code{format} which allows for quick return of an object's size in memory. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x An R object. 
#' 
#' @param unit Unit of size. See \link{object.size} for options. Defaults to 
#' \code{"Mb"} for megabytes. 
#' 
#' @param as_fs_bytes Should the return be in \code{fs}'s bytes data type? If
#' \code{TRUE}, \code{unit} is ignored. 
#' 
#' @return Character or \code{fs_bytes} vector with length of \code{1}. 
#'
#' @export
object_size <- function(x, unit = "Mb", as_fs_bytes = FALSE) {
  
  # Get object's size
  size <- object.size(x)
  
  if (as_fs_bytes) {
    # To the fs bytes data type
    size <- fs::as_fs_bytes(size)
  } else {
    # Format to a character string with units
    size <- format(size, units = unit, digits = 1)
  }
  
  return(size)
  
}
