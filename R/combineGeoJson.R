#' Function to read many \code{geojson} files and combine them as a 
#' JavaScript variable.
#' 
#' Useful for when using \code{geojson} for leafet.
#' 
#' @author Stuart Grange
#' 
#' @importFrom stringr str_c
#' @importFrom stringr str_trim
#' 
#' @export
#' 
#' 
combineGeoJson <- function(dir.input = '~/Desktop/', var = 'counties_data', 
                           dir.export = '~/Desktop/', remove = TRUE) {
  
  # Define a function
  combine <- function(x) {
    
    a <- readLines(x, warn = FALSE)
    a <- str_trim(a)
    b <- str_c(a, collapse = '')
    c <- str_c(b, ',')
    
    c
    
  }
  
  # Get names of exported files
  file.list <- list.files(dir.input, '.geojson$', full.names = TRUE)
  
  # Read all geojson files
  list.test <- plyr::llply(file.list, combine)
  
  # Format for leaflet and js
  character <- unlist(list.test)
  character <- str_c(character, collapse = '')
  character <- str_c('var ', var, ' = [', character, '];')
  
  # Write the file
  writeLines(character, str_c(dir.export, var, '.js'))
  
  if(remove) {
    
    plyr::l_ply(file.list, file.remove)
    
  }
  
}
