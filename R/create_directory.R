#' Function to create directories if they are not present. 
#' 
#' If the directory exists, no function will be called. \code{create_directory} 
#' will not create multiple levels of directories 
#' 
#' @author Stuart K. Grange
#' 
#' @param directory Name or path of directory to be created if id does not exist. 
#' \code{directory} can take many values and will create multiple levels of new
#' directories. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Create a directory
#' create_directory("data_dump")
#' 
#' # Create many directories
#' create_directory(c("data_dump", "new_directory"))
#' 
#' }
#' 
#' @export
create_directory <- function (directory) {
  # Vectorise function
  plyr::l_ply(directory, create)
  
}

# The actual function
create <- function (x) {
  # Create if does not exist
  if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE)
  }
  
  # No return
  
}
