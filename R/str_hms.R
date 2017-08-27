#' Function to format a \code{hms} vector to a clean character vector. 
#' 
#' @param x A \code{hms} vector. 
#' 
#' @param round Number of decimal points to round to. The default is \code{NA}
#' which means no rounding. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector with length of \code{x}. 
#' 
#' @export 
str_hms <- function(x, round = NA) {
  
  # Check class
  stopifnot("hms" %in% class(x))
  
  if (!is.na(round)) {
    
    # Drop precision  
    x <- round(x, round)
    
    # Alters type, push it back
    x <- hms::as.hms(x)
    
  }
  
  # To character
  x <- format(x)
  
  # Precding white space for NAs
  x <- stringr::str_trim(x)
  
  # NA to true NAs
  x <- ifelse(x == "NA", NA, x)
  
  return(x)
  
}
