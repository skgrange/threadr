#' Function to catch \code{cat} messages and make them invisible. 
#' 
#' @param x An expression. 
#' 
#' @return Invisible. 
#' 
#' @export
quiet <- function(x) {
  
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
  
}


#' @rdname quiet
#' @export
gc_quiet <- function() {
  quiet(gc())
}
