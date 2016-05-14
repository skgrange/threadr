#' Function to print all functions in a package. 
#' 
#' \code{print_functions} will include unexported functions. 
#' 
#' @author Joshua Ulrich 
#' 
#' @seealso \href{http://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package/12115495#12115495}{stackoverflow.com}
#' 
#' @export
print_functions <- function(x) ls(getNamespace(x), all.names = TRUE)
