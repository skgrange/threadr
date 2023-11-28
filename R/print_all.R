#' Function to print all rows in a tibble. 
#' 
#' @param x Object to print, usually a tibble. 
#' 
#' @param n Number of rows to print.
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
#' # Standard printing will not print all rows
#' tibble(x = 1:30)
#' 
#' # Print all rows
#' tibble(x = 1:30) %>% print_all()
#' 
#' @export
print_all <- function(x, n = Inf) {
  
  if (any(c("tbl", "sf") %in% class(x))) {
    print(x, n = n)
  } else {
    print(x)
  }
  
  return(invisible(x))
  
}
