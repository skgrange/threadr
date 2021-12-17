#' Function to list files in a directory.
#' 
#' \code{list_files} is a wrapper for \code{\link{list.files}} but 
#' \code{full.names} has the default value of \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param path Path of directory.
#' 
#' @param pattern Pattern for regular expression matching. 
#' 
#' @param all.files Logical to force the return of hidden files. 
#' 
#' @param full.names Logical to force the full file path to be returned. The 
#' default is \code{TRUE}. 
#' 
#' @param recursive Logical to operate recursively.  
#' 
#' @param ignore.case Logical for \code{pattern}. 
#' 
#' @param include.dirs Should sub-directory names be included? 
#' 
#' @param no.. Should \code{"."} and \code{".."} be dropped from return? 
#' 
#' @param fs_path Should the return be an \strong{fs} path? Useful for tidying 
#' paths and colour printing.
#' 
#' @param verbose Should the function give messages?
#' 
#' @return Vector of file names or paths.
#' 
#' @export
list_files <- function(path = ".", pattern = NULL, all.files = FALSE, 
                       full.names = TRUE, recursive = FALSE, ignore.case = FALSE,
                       include.dirs = FALSE, no.. = FALSE, fs_path = FALSE,
                       verbose = FALSE) {
  
  x <- list.files(
    path = path, 
    pattern = pattern, 
    all.files = all.files, 
    full.names = full.names, 
    recursive = recursive, 
    ignore.case = ignore.case, 
    include.dirs = include.dirs,
    no.. = no..
  )
  
  # To fs
  if (fs_path) x <- fs::as_fs_path(x)
  
  # Give a message
  if (verbose) message(date_message(), "`", length(x), "` file(s)...")
  
  return(x)
  
}
