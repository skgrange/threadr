#' Function to read JSON files. 
#' 
#' Simple wrapper for \code{jsonlite::fromJSON}. 
#' 
#' @param file File name of JSON file or character vector containing a JSON
#' string. 
#' 
#' @param flatten Should the JSON file be flattened? 
#' 
#' @return An R data structure representing the JSON content. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
read_json <- function(file, flatten = TRUE) {
  
  # Parse
  x <- jsonlite::fromJSON(file, flatten)
  
  # If data frame, make tibble if possible
  if (inherits(x, "data.frame")) {
    
    # Get data classes
    classes <- purrr::map_chr(x, class)
    classes <- unique(classes)
    
    # If simple data frame, make tibble
    if (!any(classes %in% c("data.frame", "list"))) x <- as_tibble(x)
    
  }
  
  return(x)
  
}
