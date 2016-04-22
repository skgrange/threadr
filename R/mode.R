#' Function to calculate mode of a vector
#' 
#' @param x Input vector. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode}{stackoverflow}
#' 
#' @author Ken Williams
#' 
#' @export
mode <- function(x) {
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
  
}
