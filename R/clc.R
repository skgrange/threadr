#' Function to clear the R console.
#' 
#' Found here: \url{http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Clear console
#' clc()
#' 
#' # Or without loading package
#' threadr::clc()
#' 
#' }
#' 
#' @export
clc <- function() cat("\014")
