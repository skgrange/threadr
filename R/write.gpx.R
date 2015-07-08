#' Function to write a data frame to a GPX file with usage analogous to 
#' \code{write.table} and its derivatives.
#' 
#' \code{write.gpx} uses \code{rgdal::writeOGR} as the GPX writer. Unlike the 
#' standard \code{rgdal} function, write.gpx will automatically expand file 
#' paths and overwrite previous files. 
#' 
#' @param df Data frame to be written as a GPX file.
#' @param latitude df's latitude variable name.
#' @param longitude df's longitude variable name.
#' @param file File name of exported GPX file
#' @param name Name of variable which will be copied and added in the "name"
#' element of the GPX file. 
#' @param layer 
#'   
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' write.gpx(data.bus.stations, "~/Desktop/bus_stations.gpx")
#' }
#'
#' @export
#' 
write.gpx <- function (df, latitude = "latitude", longitude = "longitude", 
                       file = "", name = NULL, layer = "") {
  
  # Check
  if (!layer %in% c("points", "lines")) {
    stop("Layer must either 'points' and 'lines'.")
  }
  
  # Make sure file name is expanded
  file <- path.expand(file)
  
  # Delete file, rgdal does not do this
  if (file.exists(file)) {
    file.remove(file)
  }
  
  # Add a name element
  if (!is.null(name)) {
    df[, "name"] <- df[, name]
  }
  
  if (layer == "points") {
    
    # For writeOGR
    layer.vector <- 'points'
    
    # Make sp points object
    sp::coordinates(df) <- c(longitude, latitude)
    
    # Reassign
    sp.object <- df
    
    # Force projection, not ideal
    proj4string(sp.object) <- "+proj=longlat +datum=WGS84"
    
  }
  
  if (layer == "lines") {
    
    # For writeOGR
    layer.vector <- "lines"
    
    sp.object <- tidy_data_to_line(df, latitude = latitude, longitude = longitude)
    
  }
  
  # Write gpx file
  rgdal::writeOGR(sp.object, file, layer = layer.vector, driver = "GPX", 
                  dataset_options = "GPX_USE_EXTENSIONS=yes")
  
}
