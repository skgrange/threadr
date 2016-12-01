#' Function to get operating system type.
#' 
#' @author Stuart K. Grange
#' 
#' @export
detect_os <- function() as.character(.Platform$OS.type)
