#' Function to remove all non-ASCII characters from a string. 
#' 
#' \code{str_rm_non_ascii} is useful when data cleaning. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
#' 
str_rm_non_ascii <- function (x) {
  
  # Remove non-ASCII characters
  x <- stringr::str_replace_all(x, "[^\\x00-\\x7F]", "")
  
  # Return
  x
  
}
