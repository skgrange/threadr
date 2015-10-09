#' Function to convert Two's Complement integer to a "standard" integer. 
#' 
#' \code{twos_complement} is vectorised and will convert integers to-and-from
#' binary form to decode the Two's Complement rule. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # A hexadecimal number
#' hex <- "fff4d"
#' 
#' # Hexadecimal as a base 10 decimal
#' integer <- strtoi(hex, 16)
#' integer
#' 1048397
#' 
#' # Decode the Two's Complement rule
#' twos_complement(integer)
#' -179
#' 
#' }
#' 
#' @export
#' 
twos_complement <- function (x) {
  
  # Vectorise function
  x <- unlist(lapply(x, parse_twos_complement))
  # Return
  x
  
}

# Main function
# Parse Two's Complement integer, will convert to-and-from binary
parse_twos_complement <- function (x) {
  
  # 
  if (bitwAnd(x, bitwShiftL(1, 20 - 1)) != 0) {
    x <- x - (bitwShiftL(1, 20))
  }
  
  # Return
  x
  
}

