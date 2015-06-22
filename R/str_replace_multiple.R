#' Function to replace multiple vectors with multiple vectors. 
#' 
#' 
#' Not complete
#' 
#' @author Stuart Grange
#'
#' @export
#' 
str_replace_multiple <- function (x, input = c('a', 'b'), 
                                  output = c('z', 'y'),
                                  boundary = FALSE) {
  
  
  if (boundary) {
    
    for (i in seq_along(input)) {
      
      x <- stringr::str_replace(x, stringr::str_c('\\b', input[i], '\\b'),
                                   output[i])
      
    }
    

  } else {
    
    for (i in seq_along(input)) {
      
      x <- stringr::str_replace(x, input[i], output[i])
      
    }  
    
  }
  
  # Return
  x
  
}