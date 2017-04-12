#' Function to read an \code{.rds} file from a \code{URL}. 
#' 
#' @param file_remote Remote file name. 
#' 
#' @param quiet Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return An R data class. 
#' 
#' @export
read_rds_remote <- function(file_remote, quiet = TRUE) {
  
  # Remote read
  if (grepl("^http", file_remote, ignore.case = TRUE)) {
    
    # Make temp local copy
    file_local <- basename(file_remote)
    file_local <- file.path(tempdir(), file_local)
    
    # Download file
    download.file(file_remote, file_local, quiet = quiet, mode = "wb")
    
    # Load file
    df <- readRDS(file_local)
    
  } else {
    
    # Direct read
    df <- readRDS(file_remote)
    
  }
  
  # Return
  df
  
}
