#' Function to load \code{XML} document as a list.
#' 
#' @author Stuart K. Grange
#' 
#' @param file File name or character vector of an XML document. 
#' 
#' @param clean_names Should duplicated names be made unique? 
#' 
#' @return Named list.
#' 
#' @export
read_xml <- function(file, clean_names = TRUE) {
  
  # Load
  list_xml <- XML::xmlToList(file)
  
  # Clean names
  if (clean_names) list_xml <- unique_names(list_xml)
  
  return(list_xml)
  
}


unique_names <- function(x) {
  
  # Get names
  y <- names(x)
  
  if (any(class(y) %in% c("character", "factor"))) {
    
    # Make unique
    y <- make.unique(y, sep = "_")
    
  }
  
  # Give names
  names(x) <- y
  
  return(x)
  
}
