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
  stopifnot(any(c("hms", "numeric", "integer") %in% class(x)))
  
  # Check data type
  if (any(c("numeric", "integer") %in% class(x))) x <- hms::as_hms(x)
  
  if (!is.na(round)) {
    
    # Drop precision  
    x <- round(x, round)
    
    # Alters type, push it back
    x <- hms::as_hms(x)
    
  }
  
  # To character
  x <- format(x)
  
  # Preceding white space for NAs
  x <- stringr::str_trim(x)
  
  # NA to true NAs
  x <- ifelse(x == "NA", NA, x)
  
  return(x)
  
}
