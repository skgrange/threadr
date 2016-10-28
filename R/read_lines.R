#' Function to wrap \code{\link{readLines}} with different defaults. 
#' 
#' @param connection File name, URL, or connection to load. 
#' 
#' @param n Number of lines to read. Default is all (\code{-1}). 
#' 
#' @param collapse Should the vector which is loaded be collapsed to a length of
#' \code{1}. 
#' 
#' @param warn Should \code{readLines} give warning? Default is \code{FALSE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
read_lines <- function(connection, n = -1, collapse = FALSE, warn = FALSE) {
  
  # Load
  text <- readLines(connection, n = as.integer(n), warn = warn)
  
  # Collapse
  if (collapse) text <- stringr::str_c(text, collapse = "")
  
  # Return
  text
  
}
