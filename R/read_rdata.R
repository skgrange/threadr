#' Function to read \code{RData} files but not their name so standard assignment 
#' can be used. 
#' 
#' If a single object needs to be saved in R's native format, use 
#' \code{\link{saveRDS}} rather than \code{\link{save}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Load an RData file with standard assignment. 
#' data_air <- read_rdata("ADA16_2015.RData")
#' }
#' 
#' @export
read_rdata <- function(file) {
  
  # Load RData
  r_data <- load(file)
  
  # Change imported object's name
  object <- get(r_data)
  
  # Return
  object
  
}
