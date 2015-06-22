#' Function to round all numerical variables in a data frame
#'
#' @param x A data frame containing numeric vairables
#' @param round Number of decimal places, used in \code{base::round}
#' 
#' @author Stuart Grange
#' 
#' @examples
#' 
#' # Round all numeric variables to two decimal places
#' \dontrun{
#' data.tidy.round <- roundNumeric(data.tidy, round = 2)
#' }
#' 
#' # or, make all numeric variables integers
#' \dontrun{
#' data.tidy.integer <- roundNumeric(data.tidy, round = 0)
#' }
#' 
#' @export
#'
roundNumeric <- function(x, round = 2) {
  
  # Apply function to all the numeric variables in data frame
  x[ , sapply(x, is.numeric)] <- lapply(x[ , sapply(x, is.numeric)], 
                                        function(y) round(y, round))
  
  # Return
  x
  
}
