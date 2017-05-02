#' Function to detect number plates from an image. 
#' 
#' \code{detect_number_plate} is built on-top of 
#' (\href{https://github.com/openalpr/openalpr}{\code{openalpr} }) and this 
#' needs to be installed before this function will work. 
#' 
#' @param file Vector of file names.
#' 
#' @param region Region of number plates.
#' 
#' @param warn Should the function raise a warning if unable to detect a number
#' plate? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
detect_number_plate <- function(file, region = "gb", warn = FALSE) {
  
  # Build command
  file_basename <- basename(file)
  file_collapse <- stringr::str_c(file, collapse = " ")
  command <- stringr::str_c("alpr -j -n 1 -c ", region, " ",  file_collapse)
  
  # Use system library
  text <- system(command, intern = TRUE)
  
  # Parse json return
  list_json <- plyr::llply(text, read_json)
  
  # Add file into list, x is an index here
  list_json <- lapply(1:length(list_json), function(x) {
    
    z <- list_json[[x]]
    z <- append(z, list(file = file_basename[x]))
    z
    
  })
  
  # Clean and format json return
  df <- plyr::ldply(list_json, clean_alpr_json_return, warn = warn)
  
  # Return
  df
  
}


clean_alpr_json_return <- function(x, warn) {
  
  # Get clean date
  date_unix <- x$epoch_time
  date_unix_start <- stringr::str_sub(date_unix, end = 10)
  date_unix_end <- stringr::str_sub(date_unix, start = 11)
  date_unix <- stringr::str_c(date_unix_start, ".", date_unix_end)
  date_unix <- as.numeric(date_unix)
  date <- parse_unix_time(date_unix, tz = "UTC")
  
  # Get other things
  img_width <- x$img_width
  img_height <- x$img_height
  version <- x$version
  file <- x$file
  
  # Get data
  df_results <- x$results
  
  # Build data frame
  df <- data.frame(
    file,
    img_width,
    img_height,
    version,
    stringsAsFactors = FALSE
  )
  
  if (length(df_results) == 0) {
    
    if (warn) {
      
      warning(stringr::str_c("'", file, "' no number plate detected..."), 
              call. = FALSE)
      
    }
    
  } else {
    
    # Select a few variables
    df_results <- df_results[, c("plate", "confidence", "processing_time_ms")]
    
    df <- cbind(
      df,
      df_results
    )
    
  }
  
  # Return
  df
  
}