#' Function to convert Two's Complement integer to a standard integer. 
#' 
#' \code{twos_complement} is vectorised and will convert integers to-and-from
#' (20-bit) binary form to decode the Two's Complement rule. 
#' 
#' @param x Vector of 20-bit integers. 
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
  return(ifelse(x > (2^19 - 1), x - 2^20, x))
