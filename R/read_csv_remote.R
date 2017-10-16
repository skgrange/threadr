#' Function to read a \code{.csv} file and return as a data frame from a 
#' \code{URL}. 
#' 
#' @param url Remote file to read. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
read_csv_remote <- function(url, verbose = FALSE) {
  
  # Build output file name
  file_remote_base <- basename(url)
  file_local <- file.path(tempdir(), file_remote_base)
  
  # Escape some special characters
  # GitHub specific
  if (grepl("\\?raw=true$", file_local))
    file_local <- stringr::str_replace(file_local, "\\?raw=true$", "")
  
  # Get file
  download.file(url, file_local, quiet = !verbose)
  
  df <- suppressMessages(
    data.frame(readr::read_csv(file_local, progress = FALSE))
  )
  
  return(df)
  
}


#' Function to read a \code{.csv} file and return as a data frame from a 
#' GitHub repository. 
#' 
#' @param user GitHub user. 
#' 
#' @param repository GitHub repository. 
#' 
#' @param branch Repository's branch. 
#' 
#' @param file Repository's file, including directory.  
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Use the defaults
#' data_hour <- read_csv_git_hub(file = "data/wacl/2016_kirb_hour_data.csv.bz2")
#' 
#' }
#' 
#' @export
read_csv_git_hub <- function(user = "skgrange", repository = "web.server", 
                             branch = "master", file) {
  
  # Strip suffix if present, it is added later
  if (grepl("\\?raw=true$", file))
    file <- stringr::str_replace(file, "\\?raw=true$", "")
  
  # Build url string
  url <- stringr::str_c("https://github.com/", user, "/", repository, "/blob/",
                        branch, "/", file, "?raw=true")
  
  # Get and read file
  df <- read_csv_remote(url)
  
  return(df)
  
}
