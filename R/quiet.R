#' Function to catch \code{cat} messages and make them invisible. 
#' 
#' @param x An expression. 
#' 
#' @return Invisible. 
#' 
#' @export
quiet <- function(x) {
  
  # # http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
  
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
  
}
