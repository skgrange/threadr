#' Function to read JSON files to a data frame. 
#' 
read_json <- function (file, flatten = TRUE) {
  
  # Load file
  df <- jsonlite::fromJSON(file, flatten)
  
  # If only one observation
  if (length(df) == 1) {
    df <- data.frame(df)
  }
  
  # Return
  df
  
}
