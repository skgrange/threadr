#' Function to convert a logical vector to a "Yes/No" vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Logical vector
#' 
#' @param as_factor Should the return be a factor? 
#' 
#' @param na_as_false Should missing elements (\code{NA}) be as regarded as 
#' \code{FALSE}?
#' 
#' @return Ordered factor or character vector.
#' 
#' @examples
#' 
#' # A vector
#' x <- c(TRUE, TRUE, TRUE, FALSE, TRUE, NA, FALSE, TRUE, TRUE, TRUE)
#' 
#' # To yes/no
#' logical_to_yes_no(x)
#' 
#' # Replace NAs with FALSE
#' logical_to_yes_no(x, na_as_false = TRUE)
#' 
#' # As a character vector
#' logical_to_yes_no(x, as_factor = FALSE)
#' 
#' @export
logical_to_yes_no <- function(x, as_factor = TRUE, na_as_false = FALSE) {
  
  # Check type
  if (class(x)[1] != "logical") {
    stop("Input must be a logical vector.", call. = FALSE)
  }
  
  # Make missing elements false
  if (na_as_false) x <- if_else(is.na(x), FALSE, x)
  
  # To character
  x <- if_else(x, "Yes", "No")
  
  # To ordered factor
  if (as_factor) x <- factor(x, levels = c("Yes", "No"), ordered = TRUE)
  
  return(x)
  
}
