#' Function to download files from an FTP or SFTP server. 
#' 
#' @param url The url(s) of the file(s) which are to be downloaded. 
#' 
#' @param credentials Credentials for a FTP or SFTP server. Do not use 
#' \code{credentials} if the server does not require authentication. 
#' \code{credentials} takes the format: \code{"username:password"}. 
#' 
#' @param directory Directory where the files are to be downloaded to. If 
#' \code{directory} does not exist, it will be created. 
#' 
#' @param curl Should \strong{RCurl} be used to download the files or base R's 
#' \code{download.file}? Default is \code{TRUE}. 
#' 
#' @param progress Progress bar type. Default is \code{"text"}, for no bar, use
#' \code{"none"}. 
#' 
#' @seealso \strong{\link{RCurl}}, \code{\link{list_files_ftp}}, 
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
download_ftp_file <- function(file, credentials = "", directory = NA, 
                              curl = TRUE, progress = "text") {
  
  # Apply function over files
  plyr::l_ply(file, downloader, credentials, directory, curl, 
              .progress = progress)
  
}


# No export
downloader <- function(url, credentials = "", directory = NA, curl = TRUE) {
  
  # If not directory is used
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
  plyr::l_ply(file, uploader, url = url, credentials = credentials, 
              basename = basename, .progress = progress)
  
}


# No export
uploader <- function(file, url, credentials, basename) {
  
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



#' Function to download many files to a directory. 
#' 
#' \code{download_file} uses \code{download.file} but can download many files 
#' and write them to a single directory. \code{download_file} will create the
#' destination directory if it does not exist. 
#'
#' @param url URL/file name of file to download. 
#' 
#' @param directory Name of destination directory where files are to be 
#' downloaded to. If \code{directory} is not used, the working directory will be
#' the destination. 
#' 
#' @param file_output Name of downloaded files. \code{file_output} should be the
#' same length as \code{url}. If not used, the \code{url} basename will be used. 
#' 
#' @param quiet Should \code{download.file}'s messages be suppressed? Default is
#' \code{TRUE}. 
#' 
#' @param progress Type of progress bar to display. Default is none, but it can
#' be \code{"text"} or \code{"time"} too. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Download a single file
#' url <- "http://cdr.eionet.europa.eu/lv/eu/aqd/d/envvfk_0q/REP_D-LV_LEGMC_20150911_D-001.xml"
#' download_file(url, "downloaded/")
#' 
#' 
#' # Download many files with information
#' url <- c("http://cdr.eionet.europa.eu/lv/eu/aqd/d/envvfk_0q/REP_D-LV_LEGMC_20150911_D-001.xml",
#'          "http://cdr.eionet.europa.eu/lv/eu/aqd/g/envvnkijg/REP_D-LV_LEGMC_20151222_G-001.xml")
#'          
#' download_file(url, "downloaded/", quiet = FALSE)
#' 
#' 
#' # Use a vector for filenames
#' file_names <- c("file_one.xml", "file_two.xml")
#' 
#' # Download
#' download_file(url, "downloaded/", file_names, quiet = FALSE)
#' 
#' }
#'
#' @export 
download_file <- function(url, directory = NA, file_output = NA, quiet = TRUE, 
                          progress = "none") {
  
  # Check
  if (!length(url) == length(file_output) & !is.na(file_output[1]))
    stop("'url' and 'file_output' must be the same length.", call. = FALSE)
  
  # Use working directory by default
  if (is.na(directory[1])) directory <- getwd()
  if (is.na(file_output[1])) file_output <- basename(url)
  
  # Catch factors
  url <- as.character(url)
  directory <- as.character(directory)
  file_output <- as.character(file_output)
  
  # Create directory if needed
  create_directory(directory, quiet)
  
  # Build two dimensional object
  df_map <- data.frame(
    url = url, 
    directory = directory, 
    file_output = file_output,
    stringsAsFactors = FALSE
  )
  
  # Download multiple files
  plyr::a_ply(df_map, 1, download_to_directory, quiet, .progress = progress)
  
  # No return
  
}


# No export
download_to_directory <- function(df_map, quiet) {
  
  # Get things from mapping data frame
  file <- file.path(df_map$directory, df_map$file_output)
  
  # Download file
  download.file(df_map$url, file, quiet = quiet)
  
  # No return
  
}

