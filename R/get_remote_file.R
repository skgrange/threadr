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
#' @param method Method for downloading remote files. 
#' 
#' @param cache_ok Is a server-side cached value acceptable?
#' 
#' @param sleep If a number, this is the number of seconds to sleep between 
#' download iterations. This can help with keeping some web servers happy. 
#' 
#' @return Invisible \code{file_local}. 
#' 
#' @seealso \code{\link{download.file}}
#' 
#' @author Stuart K. Grange
#' 
#' @export
get_remote_file <- function(file_remote, file_local, verbose = TRUE, mode = "w", 
                            method = "auto", cache_ok = TRUE, sleep = FALSE) {
  
  # Check
  stopifnot(length(file_remote) == length(file_local))
  stopifnot(all(dir.exists(dirname(file_local))))
  
  # Build mapping tibble
  df <- tibble(
    file_remote,
    file_local,
    verbose = verbose,
    mode = mode,
    method = method,
    cache_ok = cache_ok,
    sleep = sleep,
    index = seq(1, length(file_remote)),
    length = length(file_remote)
  )
  
  # Do, pwalk will use the names of the df to match the arguments in the worker
  purrr::pwalk(df, get_remote_file_worker)
  
  return(invisible(file_local))
  
}


get_remote_file_worker <- function(index, file_remote, file_local, verbose, mode, 
                                   method, cache_ok, sleep, length) {
  
  # No need for sleep for the last iteration
  if (index == length) {
    sleep <- FALSE
  }
  
  tryCatch({
    download.file(
      url = file_remote, 
      destfile = file_local, 
      quiet = !verbose, 
      mode = mode,
      method = method,
      cacheOK = cache_ok
    )
  }, error = function(e) {
    cli::cli_alert_warning("`{file_remote}` could not be downloaded...")
  })
  
  # Sleep between iterations
  if (!is.logical(sleep)) {
    if (verbose) {
      message("Sleeping...\n")
    }
    Sys.sleep(sleep)
  }
  
  return(invisible(file_local))
  
}


#' @rdname get_remote_file
#' 
#' @export
get_remote_file_if_new <- function(file_remote, file_local, verbose = TRUE,
                                   cache_ok = TRUE) {
  
  # Only a single file can be used
  stopifnot(length(file_remote) == 1L)
  
  # Expand path
  file_local_temp <- fs::path(tempdir(), fs::path_file(file_local))
  
  # Get hash of current file if it already exists
  hash_file_local <- unname(tools::md5sum(file_local))
  
  # Download file to temp. directory
  get_remote_file(
    file_remote,
    file_local_temp, 
    verbose = verbose,
    cache_ok = cache_ok
  )
  
  # Get hash of downloaded file
  hash_file_remote <- unname(tools::md5sum(file_local_temp))
  
  # Is the file new?
  is_file_new <- !(hash_file_local == hash_file_remote && !is.na(hash_file_local))
  
  # Move file if new, delete if not new
  if (is_file_new) {
    if (verbose) {
      cli::cli_alert_info("`{file_local}` is new, moving to local directory...")
    }
    fs::file_move(file_local_temp, file_local)
  } else {
    if (verbose) {
      cli::cli_alert_info("`{file_local}` is not new, not moving to local directory...")
    }
    fs::file_delete(file_local_temp)
  }
  
  return(invisible(file_local))
  
}
