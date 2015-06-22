#' Function to generate \code{geojson} JavaScript variables from a data frame
#' containing points.
#'
#' @author Stuart Grange
#' 
#' @export
#' 
writeJavaScriptPointData <- function(x = data.done, name = 'points_done', 
                                     output = 'uk_location_tracking/', 
                                     latitude = 'latitude', 
                                     longitude = 'longitude') {
  
  # Define coordinates and convert dataframe to spatial dataframe
  sp::coordinates(x) <- c(longitude, latitude)
  
  # Produce geojson object
  y <- geojsonio::geojson_json(x, pretty = TRUE)
  
  # Add the js formating for an object
  y <- stringr::str_c('var ', name, ' = [ ', y, ' ];')
  
  # Export as text
  writeLines(y, stringr::str_c(output, '/', name, '.js'))
  
}