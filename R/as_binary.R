#' Function to convert integers to binary strings. 
#' 
#' \code{as_binary} converts integers to their binary representation. 
#' \code{as_binary} does not work for integers which are larger than 32 bit. 
#' 
#' @param x Integer to convert into its binary from.
#'  
#' @param n Number of digits/bits to return. Default is 32. 
#'     
#' @author Stuart K. Grange
#' 
#' @seealso \href{http://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r}{stackoverflow}
#' 
#' @examples
#' \dontrun{
#' 
#' # Vector with length of one
#' as_binary(5L)
#'
#' # Sampled vector 
#' as_binary(sample(1L:1000L, 1000))
#' 
#' }
#' 
#' @export 
as_binary <- function(x, n = 32) {
  
  # Check type
  if (!is.integer(x)) stop("Input must be an integer.", call. = FALSE)
  
  # Do
  x <- sapply(x, function(x) integer_to_binary(x, n))
  
  # Return
  x
  
}
  

# Main function to convert integers to binary
# No export
integer_to_binary <- function(x, n) {
  
  # Convert to a vector of integers
  x <- intToBits(x)
  
  # Drop leading zeros
  x <- as.integer(x)
  
  # Filter to a certain number of bits
  x <- x[1:n]
  
  # Reverse order of vector
  x <- rev(x)
  
  # Collapse vector into string
  x <- stringr::str_c(x, collapse = "")
  
  # Return 
  x
  
}
