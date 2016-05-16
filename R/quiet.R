# http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
# Function to catch \code{cat} messages and make them invisible. 
# 
#' @export
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
