#' Function to read JSON files. 
#' 
read_json <- function (file, flatten = TRUE) {
  
  # Load file
  df <- jsonlite::fromJSON(file, flatten)
  
  # If only one observation
  if (!is.data.frame(df)) df <- data.frame(df)
  
  # Return
  df
  
}
