#' Function to get/download file from an \code{URL}. 
#' 
#' @param file_remote Vector of \code{URL}s/remote file names. 
#' 
#' @param file_local Vector of local file names, the destination files for 
#' \code{file_remote}. 
#' 
#' @param verbose Should the function give messages about download progress? 
#' 
#' @param mode Mode with which to write the file.
#' 
#' @param method Method for downloading. 
#' 
#' @seealso \code{\link{download.file}}
#' 
#' @author Stuart K. Grange
#' 
#' @export
get_remote_file <- function(file_remote, file_local, verbose = TRUE, mode = "w", 
                            method = "auto") {
  
  # Check
  stopifnot(length(file_remote) == length(file_local))
  
  # Do
  purrr::walk2(
    file_remote, 
    file_local, 
    get_remote_file_worker,
    verbose = verbose,
    mode = mode, 
    method = method
  )
  
  # No return
  
}


get_remote_file_worker <- function(file_remote, file_local, verbose, mode, method) {
  
  tryCatch({
    
    download.file(
      url = file_remote, 
      destfile = file_local, 
      quiet = !verbose, 
      mode = mode,
      method = method
    )
    
  }, error = function(e) {
    
    message <- stringr::str_c("Could not download ", file_remote, "...")
    warning(message, call. = FALSE)
    
  })
  
  # No return
  
}