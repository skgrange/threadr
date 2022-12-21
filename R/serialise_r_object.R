#' Functions to serialise and unseralise R objects. 
#' 
#' @param x An R object.
#' 
#' @param compression Optional, a compression type to use for the serialisation
#' or unseralisation process. See \code{\link{memCompress}} for options. 
#' 
#' @return Raw vector. 
#' 
#' @seealso \code{\link{serialize}}, \code{\link{memCompress}}
#' 
#' @export
serialise_r_object <- function(x, compression = NA) {
  
  # Serialise object into a raw vector
  x <- serialize(x, connection = NULL, ascii = FALSE)
  
  # Compress
  if (!is.na(compression)) {
    x <- memCompress(x, type = compression)
  }
  
  return(x)
  
}


#' @rdname serialise_r_object
#' @export
unserialise_r_object <- function(x, compression = NA) {
  
  # Decompress if needed
  if (!is.na(compression)) {
    x <- memDecompress(x, type = compression)
  }
  
  # Back to a valid R object
  x <- unserialize(x)
  
  return(x)
  
}
