#' Function to download files from an \code{FTP} or \code{SFTP} server. 
#' 
#' @param file_remote \code{URL}s of the files to be downloaded.
#' 
#' @param file_output File names for the local version of \code{file_remote}.
#' \code{download_ftp_file} will create directories if they do not exist and are
#' used. 
#' 
#' @param credentials Credentials for a \code{FTP} or \code{SFTP} server. Do not 
#' use \code{credentials} if the server does not require authentication. 
#' \code{credentials} takes the format: \code{"username:password"}. 
#' 
#' @param curl Should \strong{RCurl} be used to download the files or base R's 
#' \code{download.file}? If \code{credentials} are used, \strong{RCurl} will 
#' always be used. 
#' 
#' @param verbose Should the function give messages about download progress? 
#' Only works when \code{curl = FALSE}. 
#' 
#' @param progress Progress bar type.
#' 
#' @seealso \code{\link{list_files_ftp}}, \code{\link{upload_to_ftp}}
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
download_ftp_file <- function(file_remote, file_output, credentials = "", 
                              curl = FALSE, verbose = FALSE, progress = "none") {
  
  # Check
  if (!length(file_remote) == length(file_output))
    stop("Remote and output vectors need to be the same length...", call. = FALSE)
  
  # Build mapping data frame
  df_map <- data.frame(
    file_remote = file_remote,
    file_output = file_output,
    stringsAsFactors = FALSE
  )
  
  # Do
  plyr::a_ply(
    df_map, 
    1, 
    download_ftp_file_worker, 
    credentials, 
    curl,
    verbose, 
    .progress = progress
  )
  
  # No return
  
}


# No export
download_ftp_file_worker <- function(df_map, credentials, curl, verbose) {
  
  # Get vectors
  file_remote <- df_map$file_remote[1]
  file_output <- df_map$file_output[1]
  
  # Create if does not exist
  directory <- dirname(file_output)
  create_directory(directory)
  
  # If credentials are used, use rcurl
  if (credentials != "") curl <- TRUE
  
  if (curl) {
  
    # Download the file as a binary object
    data_bin <- RCurl::getBinaryURL(
      file_remote, 
      userpwd = credentials, 
      ftp.use.epsv = FALSE
    )
    
    # Save binary object as file
    writeBin(data_bin, file_output)
    
  } else {
  
    download.file(file_remote, file_output, quiet = !verbose)
    
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
#' \code{\link{download_ftp_file}}, \code{\link{upload_to_ftp}}
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
  file_list <- plyr::llply(
    url, 
    list_files_ftp_worker, 
    credentials = credentials
  )
  
  # Just a vector bitte
  file_list <- unlist(file_list)
  
  # Return
  return(file_list)
  
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
  
  return(file_list)
  
}


#' Function to upload a files to an FTP or SFTP server. 
#' 
#' \code{upload_to_ftp} will create directories if necessary. 
#' 
#' @param file File to upload. 
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
#' @seealso \code{\link{download_ftp_file}}, \code{\link{list_files_ftp}}
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
  plyr::l_ply(
    file, 
    upload_to_ftp_worker, 
    url = url, 
    credentials = credentials, 
    basename = basename, 
    .progress = progress
  )
  
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
  RCurl::ftpUpload(
    file, 
    url, 
    userpwd = credentials, 
    .opts = list(ftp.create.missing.dirs = TRUE)
  )
  
  # No return
  
}
