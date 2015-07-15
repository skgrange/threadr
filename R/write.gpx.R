#' Function to write a data frame to a GPX file with usage analogous to 
#' \code{write.table}.
#' 
#' \code{write.gpx} uses \code{rgdal::writeOGR} as the GPX writer. Unlike the 
#' standard \code{writeOGR} function, \code{write.gpx} will automatically expand 
#' file paths and can overwrite previous files if necessary. 
#'
#' \code{latitude} and \code{longitude} must not contain \code{NA} values.
#' 
#' @param df Data frame to be written to a GPX file.
#' @param file File name of GPX file.
#' @param latitude \code{df}'s latitude variable name.
#' @param longitude \code{df}'s longitude variable name.
#' @param name Name of variable which will be added in the \code{"name"} element
#' of the GPX file. Optional. 
#' @param layer Type of layer to be written to GPX file. Can either be 
#' \code{"points"} or \code{"lines"}. Default is \code{"points"}. 
#'   
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' # Export a GPX file containing points
#' write.gpx(data.bus.stations, "~/Desktop/bus_stations.gpx")
#' 
#' # Export GPX file which contains a line object
#' write.gpx(data.gpx.track, "~/Desktop/drive_to_bath.gpx", layer = "lines")
#' }
#'
#' @export
#' 
write.gpx <- function (df, file, latitude = "latitude", longitude = "longitude", 
                       name = NULL, layer = "points") {
  
  # Check
  if (!layer %in% c("points", "lines")) {
    stop("Layer must either be 'points' or 'lines'.")
  }
  
  # Add a name element
  if (!is.null(name)) {
    df[, "name"] <- df[, name]
  }
  
  # Make spatial points
  if (layer == "points") {
    
    # For writeOGR
    layer.vector <- "points"
    
    # A catch for when only coordinates are present, i.e. there are no extra 
    # identifiers in df. To-do: do this better. 
    if (ncol(df) == 2) {
      df[, "name"] <- ""
    }
    
    # Make sp points object
    sp::coordinates(df) <- c(longitude, latitude)
    
    # Reassign
    sp.object <- df
    
    # Force projection, not ideal
    sp::proj4string(sp.object) <- "+proj=longlat +datum=WGS84"
    
  }
  
  # Make spatial lines
  if (layer == "lines") {
    
    # For writeOGR
    layer.vector <- "lines"
    
    sp.object <- data_frame_to_line(df, latitude, longitude)
    
  }
  
  # Write GPX file
  # Make sure file name is expanded
  file <- path.expand(file)
  
  # Delete file, rgdal does not do this
  if (file.exists(file)) {
    file.remove(file)
  }
  
  # Export file
  rgdal::writeOGR(sp.object, file, layer = layer.vector, driver = "GPX", 
                  dataset_options = "GPX_USE_EXTENSIONS=yes")
  
}
