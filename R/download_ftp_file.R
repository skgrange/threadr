#' Functions to download files from an FTP or SFTP server. 
#' 
#' \code{download_ftp_file} is not vectorised yet. 
#' 
#' @param url. The url of the file which is to be downloaded. 
#' @param credentials Credentials for a FTP or SFTP server.
#' @param directory Directory where the files are to be downloaded to.
#' 
#' @seealso \code{\link{RCurl}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Set credentials in this format
#' credentials <- "username:password"
#' 
#' # Get the file "hour_5" in the directory "test" from a FTP server
#' download_ftp_file("ftp://195.174.23.76/test/hour_5.csv", 
#'                   credentials, "ftp_data/")
#'                   
#'
#' # List files on a ftp server
#' list_files_ftp("ftp://195.174.23.76/test", credentials)
#' "ftp://195.174.23.76/test/hour_5.csv"
#' 
#' }
#'
#' @export
download_ftp_file <- function (url, credentials, directory = NA) {
  
  if (is.na(directory)) {
    stop("A directory must be specified.")
  }
  
  # Download the file as a binary object
  data_bin <- RCurl::getBinaryURL(url, userpwd = credentials, 
                                  ftp.use.epsv = FALSE)
  
  # Create file name
  file_name <- basename(url)
  
  # Create if does not exist
  threadr::create_directory(directory)
  
  # Add file path
  directory <- file.path(directory, file_name)
  
  # Save binary object as file
  writeBin(data_bin, directory)
  
}


#' @rdname download_ftp_file
#' 
#' @export
list_files_ftp <- function (url, credentials) {
  
  # Ensure the directory has a trailing separator
  url <- stringr::str_c(url, .Platform$file.sep)
  
  # Get the file list
  file_list <- RCurl::getURL(url, userpwd = credentials, ftp.use.epsv = FALSE, 
                             dirlistonly = TRUE)
  
  # Clean
  file_list <- stringr::str_c(url, stringr::str_split(file_list, "\n")[[1]])
  file_list <- stringr::str_trim(file_list)
  
  # Return
  file_list
  
}

# download_ftp_files <- function (file, credentials, directory, progress = "text") {
#   
#   plyr::l_ply(file, downloader, credentials = credentials, 
#               directory = directory, .progress = progress)
#   
# }
