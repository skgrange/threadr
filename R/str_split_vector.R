#' Function to split string and return a length-of-one-vector
#' 
#' Uses stringr::str_split to split string
#' Returning more than one element at times
#' Not complete
#'
#'@author Stuart K. Grange
#'
#'@export
#'
#'
str_split_vector <- function (x, pattern = '/', n = 1) {
  
  stop('Needs to be fixed.')
  
  # Split string, unlist result, and get a single element
  x <- sapply(x, function(y) tail(unlist(stringr::str_split(y, pattern), 
                                         use.names = FALSE), n))
  
  # Return 
  x
  
}

