#' Function to download files from an \code{FTP} or \code{SFTP} server. 
#' 
#' @param file_remote \code{URL}s of the files to be downloaded.
#' 
#' @param file_local File names for the local version of \code{file_remote}.
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
#' @param sleep Number of seconds to wait between querying server. 
#' 
#' @param verbose Should the function give messages about download progress? 
#' 
#' @seealso \code{\link{list_files_ftp}}, \code{\link{upload_to_ftp}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' # Download a file from a server which does not need credentials
#' url <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
#' download_ftp_file(url, "~/Desktop/noaa_data.csv", verbose = TRUE)
#' }
#' 
#' @export
download_ftp_file <- function(file_remote, file_local, credentials = "", 
                              curl = FALSE, sleep = NA, verbose = FALSE) {
  
  # Do
  purrr::walk2(
    file_remote,
    file_local,
    ~download_ftp_file_worker(
      file_remote = .x,
      file_local = .y,
      credentials = credentials,
      curl = curl,
      sleep = sleep,
      verbose = verbose
      
    )
  )
  
}


download_ftp_file_worker <- function(file_remote, file_local, credentials, 
                                     curl, sleep, verbose) {
  
  # Message to user
  if (verbose) message(date_message(), "`", file_remote, "`...")
  
  # Create if does not exist
  fs::dir_create(fs::path_dir(file_local))
  
  # If credentials are used, use rcurl
  if (credentials != "") curl <- TRUE
  
  if (curl) {
  
    # Download the file as a binary object
    data_bin <- RCurl::getBinaryURL(
      file_remote, 
      userpwd = credentials, 
      ftp.use.epsv = FALSE,
      forbid.reuse = TRUE
    )
    
    # Save binary object as file
    writeBin(data_bin, file_local)
    
  } else {
    download.file(file_remote, file_local, quiet = !verbose)
  }
  
  if (!is.na(sleep[1])) Sys.sleep(sleep)
  
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
#' @param sleep Number of seconds to wait between querying server. 
#' 
#' @param sort Should the files be sorted alphabetically? 
#' 
#' @param verbose Should the function give messages? 
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
list_files_ftp <- function(url, credentials = "", sleep = NA, sort = FALSE, 
                           verbose = FALSE) {
  
  # Do
  x <- url %>% 
    purrr::map(
      ~list_files_ftp_worker(
        url = .,
        credentials = credentials,
        sleep = sleep,
        verbose = verbose
      )
    ) %>% 
    purrr::flatten_chr()
  
  # Sort remote file names
  if (sort) x <- sort(x)
  
  return(x)
  
}


list_files_ftp_worker <- function(url, credentials, sleep, verbose) {
  
  # Message to user
  if (verbose) message(date_message(), "`", url, "`...")
  
  # url must be prefixed with ftp or sftp
  if (!grepl("^ftp://|^sftp://", url)) {
    stop("URL must be prefixed with 'ftp://' or 'sftp://'", call. = FALSE)
  }
  
  # Ensure the directory has a trailing separator
  url <- stringr::str_c(url, .Platform$file.sep)
  
  # Get the file list
  # If credentials are blank, this will still work
  file_list <- tryCatch({
    
    RCurl::getURL(
      url, 
      userpwd = credentials, 
      ftp.use.epsv = FALSE, 
      dirlistonly = TRUE,
      forbid.reuse = TRUE,
      .encoding = "UTF-8"
    )
    
  }, error = function(e) {
    as.character()
  })
  
  # Make a vector
  if (length(file_list) != 0) {
    file_list <- stringr::str_c(url, stringr::str_split(file_list, "\n")[[1]])
    file_list <- stringr::str_trim(file_list)
  }
  
  if (!is.na(sleep[1])) Sys.sleep(sleep)
  
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
#' @param verbose Should the function give messages? 
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
                          verbose = FALSE) {
  
  # Apply function to length of file
  purrr::walk(
    file, 
    upload_to_ftp_worker, 
    url = url, 
    credentials = credentials, 
    basename = basename, 
    verbose = verbose
  )
  
}


upload_to_ftp_worker <- function(file, url, credentials, basename, verbose) {
  
  # url must be prefixed with ftp or sftp
  if (!grepl("^ftp://|^sftp://", url)) {
    stop("URL must be prefixed with 'ftp://' or 'sftp://'", call. = FALSE)
  }
  
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
