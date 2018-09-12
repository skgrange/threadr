#' Function to export a data frame as new line JSON, also known as \code{ndjson}. 
#' 
#' @param df Data frame to export. 
#' 
#' @param file File name for exported new line JSON file. 
#' 
#' @param append Should the data be appended to \code{file}?
#' 
#' @param pagesize Number of lines to write to \code{file} per iteration. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible. 
#' 
#' @export
write_json_lines <- function(df, file, append = FALSE, pagesize = 500, 
                             verbose = FALSE) {
  
  # Open connection
  if (append) {
    
    con <- file(file, open = "a+")
    
  } else {
    
    con <- file(file, open = "w")
    
  }
  
  # Export stream
  jsonlite::stream_out(df, con, pagesize = pagesize, verbose = verbose)
  
  # Close connection
  close(con)
  
  # No return
  
}


#' Function to read a new line JSON, also known as \code{ndjson} file.
#' 
#' @param file File name of new line JSON file. 
#' 
#' @param pagesize Number of lines to write to \code{file} per iteration. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return An R data object, dependent on contents of \code{file}.
#' 
#' @export
read_json_lines <- function(file, pagesize = 500, verbose = FALSE) {
  
  # Open read only connection
  con <- file(file, open = "r")
  
  # Read
  x <- suppressWarnings(
    jsonlite::stream_in(con, pagesize = pagesize, verbose = verbose)
  )
  
  # Close connection
  close(con)
  
  # If data frame, make tibble if possible
  if (class(x) == "data.frame") {
    
    # Get data classes
    classes <- purrr::map_chr(x, class)
    classes <- unique(classes)
      
    # If simple data frame, make tibble
    if (!any(classes %in% c("data.frame", "list"))) x <- as_tibble(x)
    
  }
  
  return(x)
  
}
