#' Function to format a hyperline (href) string so when used in DT:dataTable or
#' googleVis::table, the link is active and click-able
#' 
#' 
#'
#' @author Stuart K. Grange
#'
#'
#' @export
#' 
#' 
formatHyperlink <- function (x, check = TRUE) {
  
  if (check) {
    
    if (grepl('http://', x[1])) {
      
      x <- stringr::str_c('<a href=\"', x, '\">', str_split_vector(x), '</a>')
      
    } else {
      
      x <- x
      
    }
    
  } else {
    
    x <- stringr::str_c('<a href=\"', x, '\">', str_split_vector(x), '</a>')
    
  }
  
  # Return
  x
  
}
