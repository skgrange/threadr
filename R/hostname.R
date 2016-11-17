#' Function to find system's hostname. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
hostname <- function() as.character(unname(Sys.info()["nodename"]))
