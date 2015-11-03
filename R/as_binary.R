#' Function to convert integers to binary strings. 
#' 
#' \code{as_binary} converts integers to their binary representation. 
#' \code{as_binary} is vectorised.
#'
#' \code{as_binary} does not work for integers which are larger than 32 bit. 
#' To-do: Fix this. 
#' 
#' @seealso \url{http://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r}
#' 
#' @param x Integer to convert into its binary form. 
#' @param n Number of digits/bits to return. Default is 32. 
#'     
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' as_binary(5)
#' "00000000000000000000000000000101"
#' 
#' vector <- sample(1:1000, 5)
#' vector 
#' 721 409 703 538 806
#' 
#' as_binary(vector)
#' "00000000000000000000001011010001" "00000000000000000000000110011001"
#' "00000000000000000000001010111111" "00000000000000000000001000011010"
#' "00000000000000000000001100100110"
#' 
#' }
#' 
#' @export 
as_binary <- function (x, n = 32) {
  
  # Vectorise the function
  x <- unlist(lapply(x, function (x) integer_to_binary(x, n)))
  # Return
  x
  
}


# Main function to convert integers to binary
# No export
integer_to_binary <- function (x, n) {
  
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
