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


#' Function to download a file to R's temporary directory. 
#' 
#' @author Stuart K. Grange
#' 
#' @param url URL of a file to download. 
#' @param quiet Should the function be quiet about its activities? Default is 
#' \code{TRUE}. 
#' 
#' @export
download_to_temporary <- function(url, quiet = TRUE) {
  
  # Get temp directory
  directory <- tempdir()
  
  # Download file
  download_file(url, directory = directory, quiet = quiet)
  
  # No return
  
}
