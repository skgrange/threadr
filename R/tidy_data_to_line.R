#' Function for converting a data frame into spatial lines.
#' 
#' tidy_data_to_line returns a SpatialLinesDataFrame with a single line object.
#' Multiple identifiers will currently not be preserved, but will be in the 
#' future. In general usage \code{latitude} and \code{longitude} should be 
#' projected in WGS84. 
#' 
#' @param df Data frame to be converted into SpatialLinesDataFrame. 
#' @param latitude \code{df}'s latitude variable name.
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @author Stuart K. Grange
#' 
#' @import sp
#' 
#' @examples 
#' 
#' \dontrun{
#' spatial.lines <- tidy_data_to_line(data.gps.track, "latitude", "longitude")
#' }
#' 
#' @export
#' 
tidy_data_to_line <- function (df, latitude = "latitude", 
                               longitude = "longitude") {
  
  # Make an identifier variable for lines
  if (!"id" %in% names(df)) {
    df[, "id"] <- 1
  }
  
  # Get data part for the SpatialLinesDataFrame
  data.extras <- data.frame(id = unique(df[, "id"]))
  # row.names(data.extras) <- data.extras[, "id"]
  
  # Make sp points object
  coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp.object <- df
  
  # From
  # http://stackoverflow.com/questions/24284356/convert-spatialpointsdataframe-to-spatiallinesdataframe-in-r
  # Generate lines for each id
  lines <- lapply(split(sp.object, sp.object$id), 
                  function(x) Lines(list(Line(sp::coordinates(x))), x$id[1L]))
  
  # Create spatial object with a forced projection
  lines <- SpatialLines(lines)
  
  # 
  proj4string(lines) = CRS("+proj=longlat +datum=WGS84")
  
  # Make SpatialLinesDataFrame
  lines.data <- SpatialLinesDataFrame(lines, data.extras)
  
  # Return
  lines.data
  
}
