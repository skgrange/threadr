#' Function to print all rows in a tibble. 
#' 
#' @param x Object to print. 
#' 
#' @return Invisible \code{x}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Load a package
#' library(dplyr)
#' 
#' # Truncated print
#' tibble(x = 1:30)
#' 
#' # Print all
#' tibble(x = 1:30) %>% print_all()
#' 
#' @export
print_all <- function(x) {
  
  if ("tbl" %in% class(x)) {
    print(x, n = Inf)
  } else {
    print(x)
  }
  
  return(invisible(x))
  
}
