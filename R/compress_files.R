#' Function to compress files. 
#' 
#' @param files File to compress. 
#' @param algorithm Compression algorithm to use. 
#' @param keep Should the original file be kept? Default is \code{TRUE}. 
#' @param progress Type of progress bar. Default is \code{"none"}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
compress_files <- function(files, algorithm = "bzip2", keep = TRUE, 
                           progress = "none") {
  
  # Vectorise function
  plyr::l_ply(files, file_compression_worker, algorithm, keep, .progress = progress)
  
  # No return
  
}


# The worker
# No export
file_compression_worker <- function(file, algorithm, keep) {
  
  if (algorithm == "bzip2") {
    
    # Build command
    command <- stringr::str_c("bzip2 -f -z ", file)
    
    if (keep) command <- stringr::str_c(command, " -k")
    
    # System call
    system(command)
    
  }
  
  if (algorithm == "zip") {
    
    # Build command
    file_output <- stringr::str_c(file, ".zip")
    command <- stringr::str_c("zip", file_output, file, sep = " ")
    
    # System call
    system(command, ignore.stdout = TRUE)
    
    # Delete file
    if (!keep) quiet(file.remove(file))
    
  }
  
  # No return
  
}
