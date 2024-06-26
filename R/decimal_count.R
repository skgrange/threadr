#' Function to count the number of digits on the right of a decimal point (
#' sometimes called the mantissa). 
#' 
#' @param value A numeric vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \url{http://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r}
#' 
#' @examples 
#' \dontrun{
#' 
#' decimal_count(5.89)
#' 
#' decimal_count(c(5.89, 2, 56.454545, 5.1))
#' 
#' }
#' 
#' @export
decimal_count <- function(value) purrr::map_int(value, decimal_counter)


# The worker
decimal_counter <- function(x) {

  # Check
  stopifnot(class(x) == "numeric")

  # If NA, return NA
  if (is.na(x)) {
    x <- NA_integer_
  } else {

    # If contains a period
    if (grepl("\\.", x)) {
      x <- stringr::str_replace(x, "0+$", "")
      x <- stringr::str_replace(x, "^.+[.]", "")
      x <- stringr::str_length(x)
    } else {
      x <- 0L
    }

  }

  return(x)

}

# https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
# decimal_count_ <- function(x) {
#   
#   if_else(
#     abs(x - round(x)) > .Machine$double.eps ^ 0.5,
#     nchar(sub("^\\d+\\.", "", sub("0+$", "", as.character(x)))),
#     0L
#   )
#   
# }
