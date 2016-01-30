#' Functions to download files from an FTP or SFTP server. 
#' 
#' @param url. The url(s) of the file(s) which are to be downloaded. 
#' 
#' @param credentials Credentials for a FTP or SFTP server. Do not use 
#' \code{credentials} if the server does not require authentication. 
#' \code{credentials} take the format: \code{"username:password"}. 
#' 
#' @param directory Directory where the files are to be downloaded to.
#' 
#' @param curl Should \strong{RCurl} be used to download the files or base R's 
#' \code{download.file}? Default is \code{TRUE}. 
#' 
#' @seealso \strong{RCurl}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
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
#' @aliases download_ftp_files
#' @export
download_ftp_file <- function (file, credentials = "", directory = NA, 
                               curl = TRUE, progress = "text") {
  
  # Apply function over files
  plyr::l_ply(file, downloader, credentials, directory, curl, 
              .progress = progress)
  
}


downloader <- function (url, credentials = "", directory = NA, curl = TRUE) {
  
  if (is.na(directory)) directory <- getwd()
  
  # Create file name
  file_name <- basename(url)
  
  # Create if does not exist
  create_directory(directory)
  
  # Add file path
  file_name <- file.path(directory, file_name)
  
  if (curl) {
    # Download the file as a binary object
    data_bin <- RCurl::getBinaryURL(url, userpwd = credentials, 
                                    ftp.use.epsv = FALSE)
    
    # Save binary object as file
    writeBin(data_bin, file_name)
    
  } else {
    download.file(url, file_name, quiet = TRUE)
    
  }
  
  # No return
  
}


#' Function to list files on an FTP or SFTP server. 
#' 
#' @export
list_files_ftp <- function (url, credentials = "") {
  
  # url must be prefixed with ftp or sftp
  if (!grepl("^ftp://|^sftp://", url)) {
    stop("URL must be prefixed with 'ftp://' or 'sftp://'", call. = FALSE)
  }
  
  # Ensure the directory has a trailing separator
  url <- stringr::str_c(url, .Platform$file.sep)
  
  # Get the file list
  # If credentials are blank, this will still work
  file_list <- RCurl::getURL(url, userpwd = credentials, ftp.use.epsv = FALSE, 
                             dirlistonly = TRUE)
  
  # Clean
  file_list <- stringr::str_c(url, stringr::str_split(file_list, "\n")[[1]])
  file_list <- stringr::str_trim(file_list)
  
  # Return
  file_list
  
}


#' Function to upload a files to an FTP or SFTP server. 
#' 
#' @export
upload_to_ftp <- function (file, url, credentials = "", progress = "text") {
  
  # Apply function to length of file
  plyr::l_ply(file, uploader, url = url, credentials = credentials, 
              .progress = progress)
  
}


uploader <- function (file, url, credentials) {
  
  # url must be prefixed with ftp or sftp
  if (!grepl("^ftp://|^sftp://", url)) {
    stop("URL must be prefixed with 'ftp://' or 'sftp://'", call. = FALSE)
  }
  
  # Ensure the directory has a trailing separator
  url <- stringr::str_c(url, .Platform$file.sep)
  
  # Add file name to url
  url <- stringr::str_c(url, basename(file))
  
  # Upload
  RCurl::ftpUpload(file, url, userpwd = credentials)
  
  # No return
  
}

