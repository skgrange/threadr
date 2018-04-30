#' Function to convert Two's Complement integer to a standard integer. 
#' 
#' \code{twos_complement} is vectorised and will convert integers to-and-from
#' (8-bit) binary form to decode the Two's Complement rule. 
#' 
#' @param x Vector of integers. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector. 
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
twos_complement <- function(x) 
  purrr::map_dbl(x, twos_complement_worker)


twos_complement_worker <- function(x) {
  
  # Ported from a Python example
  # http://stackoverflow.com/questions/1604464/twos-complement-in-python
  
  if (bitwAnd(x, bitwShiftL(1, 20 - 1)) != 0) x <- x - (bitwShiftL(1, 20))
  
  return(x)
  
}
