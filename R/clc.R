#' Function to clear the R console.
#' 
#' Found here: \url{http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r}
#' 
#' @author Hadley Wickham
#' 
#' @export
#' 
clc <- function() cat(rep("\n",50))
