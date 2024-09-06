#' Function to wrap \code{\link{readLines}} with different defaults. 
#' 
#' @param connection File name, URL, or connection to load. 
#' 
#' @param n Number of lines to read. Default is all (\code{-1}). 
#' 
#' @param collapse Should the vector which is loaded be collapsed to a length of
#' \code{1}. 
#' 
#' @param encoding Coding of input. Default is \code{"UTF-8"}. 
#' 
#' @param skip_null Should nulls be skipped? 
#' 
#' @param warn Should \code{readLines} give warning? Default is \code{FALSE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
read_lines <- function(connection, n = -1, collapse = FALSE, encoding = "UTF-8", 
                       skip_null = FALSE, warn = FALSE) {
  
  # Load
  x <- readLines(
    connection, 
    n = as.integer(n), 
    warn = warn, 
    encoding = encoding,
    skipNul = skip_null
  )
  
  # Collapse
  if (collapse) x <- stringr::str_c(x, collapse = "")
  
  return(x)
  
}
