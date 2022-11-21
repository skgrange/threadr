#' Function to return a script's file name and directory from within itself. 
#'
#' \code{script_paths} is most useful when calling a script as a programme. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list with two elements. 
#'
#' @export
script_paths <- function() {
  
  # Get script's file name
  if (!interactive()) {
    file <- scriptName::current_filename()
  } else {
    file <- rstudioapi::getActiveDocumentContext()$path
  }
  
  # Build a list return
  list(
    file = file,
    directory = fs::path_dir(file)
  )
  
}
