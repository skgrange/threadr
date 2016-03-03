#' Function to read JSON files. 
#' 
#' Very simple wrapper for \code{jsonlite::fromJSON}. 
#' 
#' @param file File name of JSON file.
#' @param flatten Should the JSON file be flattened? Default is \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
read_json <- function (file, flatten = TRUE) {
  jsonlite::fromJSON(file, flatten)
}
