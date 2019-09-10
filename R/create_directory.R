#' Function to create directories if they are not present. 
#' 
#' If the directory exists, no function will be called. \code{create_directory} 
#' will create multiple levels of directories (it is recursive). 
#' 
#' @author Stuart K. Grange
#' 
#' @param directory Name or path of directory to be created if id does not exist. 
#' 
#' @param quiet Should the function give a message when a directory is created? 
#' Default is \code{FALSE}. 
#' 
#' \code{directory} can take many values and is recursive. 
#' 
#' @examples
#' \dontrun{
#' # Create a directory
#' create_directory("data_dump")
#' 
#' # Create two directories
#' create_directory(c("data_dump", "new_directory"))
#' 
#' }
#' 
#' @export
create_directory <- function(directory, quiet = TRUE) {
  
  # Soon to be dropped
  .Defunct(
    msg = "`create_directory` is deprecated, please use dir.create."
  )
  
  purrr::walk(directory, directory_creator, quiet)
  
}
  

# The actual function
directory_creator <- function(x, quiet) {
  
  if (!dir.exists(x)) {
    
    # Create
    dir.create(x, recursive = TRUE)
    
    # Message
    if (!quiet) message(stringr::str_c("Created '", x, "' directory."))
    
  }

}
