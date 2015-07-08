#' Function for converting tidy data into spatial lines.
#' 
#' @author Stuart K. Grange
#' 
#' @export
#' 
tidy_data_to_line <- function (df, latitude = "lat", longitude = "long") {
  
  # Make sp points object
  sp::coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp.object <- df
  
  # Force projection, not ideal
  sp::proj4string(sp.object) <- "+proj=longlat +datum=WGS84"
  
  # From
  # http://stackoverflow.com/questions/24284356/convert-spatialpointsdataframe-to-spatiallinesdataframe-in-r
  # Generate lines for each id
  lines <- lapply(split(sp.object, sp.object$id), 
                  function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
  
  # Make spatial object
  lines <- sp::SpatialLines(lines)
  
  # Get identifiers
  data.extras <- data.frame(id = unique(df$id))
  
  # Add row names
  rownames(data.extras) <- data.extras$id
  
  # Make SpatialLinesDataFrame
  lines.data <- sp::SpatialLinesDataFrame(lines, data.extras)
  
  # Force projection for lines
  sp::proj4string(lines.data) <- "+proj=longlat +datum=WGS84"
  
  # Return
  lines.data
  
}