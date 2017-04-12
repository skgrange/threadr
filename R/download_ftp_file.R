#' Function to download files from an FTP or SFTP server. 
#' 
#' @param url The url(s) of the file(s) which are to be downloaded. 
#' 
#' @param credentials Credentials for a FTP or SFTP server. Do not use 
#' \code{credentials} if the server does not require authentication. 
#' \code{credentials} takes the format: \code{"username:password"}. 
#' 
#' @param file_output Name of downloaded files. \code{file_output} should be the
#' same length as \code{url}. If not used, the \code{url} basename will be used. 
#' The directory will be created if it does not exist. 
#' 
#' @param curl Should \strong{RCurl} be used to download the files or base R's 
#' \code{download.file}? 
#' 
#' @param verbose Should the function give messages about download progress? 
#' Only works when \code{curl = FALSE}. 
#' 
#' @param progress Progress bar type.
#' 
#' @seealso \strong{\link{RCurl}}, \code{\link{list_files_ftp}}, 
#' \code{\link{upload_ftp_file}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Download a file from a server which does not need credentials
#' url <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
#' download_ftp_file(url, "~/Desktop/noaa_data.csv")
#' 
#' }
#' 
#' @export
download_ftp_file <- function(url, file_local = NA, credentials = "", 
                              curl = FALSE, verbose = FALSE, progress = "none") {
  
  # Do
  plyr::l_ply(url, download_ftp_file_worker, file_local, credentials, curl, 
              verbose, .progress = progress)
  
}


# No export
download_ftp_file_worker <- function(url, file_local, credentials, curl, 
                                     verbose) {
  
  # If no file is used
  if (is.na(file_local[1])) file_local <- basename(url)
  
  directory <- dirname(file_local)

  # Create if does not exist
  create_directory(directory)
  
  # Add file path if not in working directory
  if (directory == ".") file_local <- file.path(directory, file_local)
  
  if (curl) {
  
    # Download the file as a binary object
    data_bin <- RCurl::getBinaryURL(url, userpwd = credentials, 
                                    ftp.use.epsv = FALSE)
    
    # Save binary object as file
    writeBin(data_bin, file_local)
    
  } else {
  
    download.file(url, file_local, quiet = !verbose)
    
  }
  
  # No return
  
}


#' Function to list files on an FTP or SFTP server. 
#' 
#' @param url The url of remote directory. 
#' 
#' @param credentials Credentials for a FTP or SFTP server. Do not use 
#' \code{credentials} if the server does not require authentication. 
#' \code{credentials} takes the format: \code{"username:password"}. 
#' 
#' @seealso \strong{\link{RCurl}}, \code{\link{download_ftp_file}}, 
#' \code{\link{upload_ftp_file}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Set credentials in this format
#' credentials <- "username:password"
#'
#' # List files on a ftp server
#' list_files_ftp("ftp://195.174.23.76/test", credentials)
#' 
#' }
#' 
#' @export
list_files_ftp <- function(url, credentials = "") {
  
  # For each url
  list_files <- plyr::llply(url, list_files_ftp_worker, 
                            credentials = credentials)
  
  # Just a vector bitte
  list_files <- unlist(list_files)
  
  # Return
  list_files
  
}


list_files_ftp_worker <- function(url, credentials) {
  
  # url must be prefixed with ftp or sftp
  if (!grepl("^ftp://|^sftp://", url))
    stop("URL must be prefixed with 'ftp://' or 'sftp://'", call. = FALSE)

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
#' \code{upload_to_ftp} will create directories if necessary. 
#' 
#' @param url The url of remote directory. 
#' 
#' @param credentials Credentials for a FTP or SFTP server. Do not use 
#' \code{credentials} if the server does not require authentication. 
#' \code{credentials} takes the format: \code{"username:password"}. 
#' 
#' @param basename Should only the basename be used for transferring the files? This is
#' useful when files to be uploaded are outside of the working directory, but the
#' directory structure will be lost on the remote server.
#' 
#' @param progress Progress bar type. Default is \code{"text"}, for no bar, use
#' \code{"none"}. 
#' 
#' @seealso \strong{\link{RCurl}}, \code{\link{download_ftp_file}}, 
#' \code{\link{list_files_ftp}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get file list
#' file_list <- list.files(recursive = TRUE, full.names = TRUE)
#' 
#' # Exclude some files
#' file_list <- grep("cache|.Rmd$", file_list, value = TRUE, invert = TRUE)
#' 
#' # Upload files to server
#' upload_to_ftp(file_list, url, "user:pass")
#' 
#' }
#' 
#' @export
upload_to_ftp <- function(file, url, credentials = "", basename = FALSE, 
                          progress = "text") {
  
  # Apply function to length of file
  plyr::l_ply(file, upload_to_ftp_worker, url = url, credentials = credentials, 
              basename = basename, .progress = progress)
  
}


# No export
upload_to_ftp_worker <- function(file, url, credentials, basename) {
  
  # url must be prefixed with ftp or sftp
  if (!grepl("^ftp://|^sftp://", url))
    stop("URL must be prefixed with 'ftp://' or 'sftp://'", call. = FALSE)
  
  # Trim to last element
  if (basename) file <- basename(file)
  
  # Ensure the directory has a trailing separator
  url <- stringr::str_c(url, .Platform$file.sep)
  
  # Add file name to url
  url <- stringr::str_c(url, file)
  
  # Upload, will create directories if needed
  RCurl::ftpUpload(file, url, userpwd = credentials, 
                   .opts = list(ftp.create.missing.dirs = TRUE))
  
  # No return
  
}
