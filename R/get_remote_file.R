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
#' @param sleep If a number, this is the number of seconds to sleep between 
#' download iterations. This can help with keeping some webservers happy. 
#' 
#' @seealso \code{\link{download.file}}
#' 
#' @author Stuart K. Grange
#' 
#' @export
get_remote_file <- function(file_remote, file_local, verbose = TRUE, mode = "w", 
                            method = "auto", sleep = FALSE) {
  
  # Check
  stopifnot(length(file_remote) == length(file_local))
  stopifnot(all(dir.exists(dirname(file_local))))
  
  # Build data frame
  df <- data_frame(
    file_remote,
    file_local,
    verbose = verbose,
    mode = mode,
    method = method,
    sleep = sleep,
    index = seq(1, length(file_remote)),
    length = length(file_remote)
  )
  
  # Do, pwalk will use the names of the df to match the arguments in the worker
  purrr::pwalk(df, get_remote_file_worker)
  
  # No return
  
}


get_remote_file_worker <- function(index, file_remote, file_local, verbose, mode, 
                                   method, sleep, length) {
  
  # No need for sleep for the last iteration
  if (index == length) sleep <- FALSE
  
  tryCatch({
    
    download.file(
      url = file_remote, 
      destfile = file_local, 
      quiet = !verbose, 
      mode = mode,
      method = method
    )
    
  }, error = function(e) {
    
    warning("Could not download ", file_remote, "...", call. = FALSE)
    
  })
  
  # Sleep between iterations
  if (!is.logical(sleep)) {
    
    if (verbose) message("Sleeping...\n")
    Sys.sleep(sleep)
    
  }
  
  # No return
  
}
