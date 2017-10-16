#' Function to wrap \code{\link{readLines}} with different defaults. 
#' 
#' @param connection File name, URL, or connection to load. 
#' 
#' @param n Number of lines to read. Default is all (\code{-1}). 
#' 
#' @param collapse Should the vector which is loaded be collapsed to a length of
#' \code{1}. 
#' 
#' @param encoding Coding of input. Defult is \code{"UTF-8"}. 
#' 
#' @param warn Should \code{readLines} give warning? Default is \code{FALSE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
read_lines <- function(connection, n = -1, collapse = FALSE, encoding = "UTF-8", 
                       warn = FALSE) {
  
  # Load
  x <- readLines(connection, n = as.integer(n), warn = warn, encoding = encoding)
  
  # Collapse
  if (collapse) x <- stringr::str_c(x, collapse = "")
  
  return(x)
  
}
