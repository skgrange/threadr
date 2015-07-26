#' Function to find distances between two spatial objects in metres or 
#' kilometres. 
#' 
#' \code{sp_distance} calculates the distance between a spatial object and 
#' another spatial object. An example of usage is when an object containing 
#' locations of cities is tested against coastline information to determine how
#' far cities are away from the coast.
#' 
#' \code{sp_distance} uses \code{rgeos::gDistance} for the distance calculations.
#' This function returns distances based on the unit of the projection system 
#' contained within the spatial objects. By default, the Mollweide projection 
#' is used. The Mollweide projection is applicable in any location on Earth, but
#' the accuracy of the returned values is dependent on location.
#' 
#' If the spatial objects are located in a zone which has a more appropriate 
#' projection system, it is highly recommended that this is used. For example, 
#' spatial data in New Zealand should be projected in the New Zealand Transverse 
#' Mercator 2000 while data in the UK should be projected in British National 
#' Grid; both of which have metre units (\code{+units=m}).
#' 
#' \code{sp_distance} supports parallel processing by forking the 
#' \code{rgeos::gDistance} function across multiple cores. This can make the 
#' distance calculations much faster.
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{gDistance}}, \code{\link{spTransform}}, 
#' \code{\link{mclapply}}
#' 
#' @param sp1 Spatial object one. 
#' @param sp2 Spatial object two. 
#' @param proj A proj4 string for the distance calculations. Defaults to the 
#' Mollweide projection, but should be changed to a location-specific projection
#' if possible. 
#' @param cores Number of cores for the function to use. Not available for 
#' Windows systems. 
#' @param unit If \code{"km"}, the returned vector is returned in kilometres 
#' rather than metres. 
#' 
#' @examples
#' \dontrun{
#' # Simple usage
#' # Calculate the distances of places from the coastline
#' distances <- sp_distance(sp.places, sp.coast.line, unit = "km")
#' 
#' # Speed the function up by using multiple system cores
#' distances <- sp_distance(sp.places, sp.coast.line, unit = "km", cores = 4)
#' 
#' 
#' # Usage for transforming a data frame
#' # Load shapefiles
#' # Coastline, spatial lines
#' sp.coast.line <- readOGR("coastlines, "coastlines")
#' 
#' # Places in the UK, spatial points
#' sp.places <- readOGR("great-britain", "places")
#' 
#' # Get a data frame from the sp.places object
#' data.places <- data.frame(sp.places)
#' 
#' # Find distances between every place in sp.places and sp.coast.line
#' # We are in the UK, therefore use British National Grid for the projection
#' proj <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
#'
#' # Apply parallel function and add variable to data frame
#' data.places$distance <- sp_distance(
#'   sp.places, sp.coast.line, unit = "km", cores = 8, proj = proj)
#'
#' # Have a look
#' head(data.places)
#' 
#' London 33.4
#' Basingstoke 43.5
#' York 37
#' Tobermory 0.22
#' Charlbury 60.3
#' 
#' # London will be dependent on where the coastline is set after the River
#' # Thames's mouth. 
#' }
#' 
#' @export
#' 
sp_distance <- function (sp1, sp2, proj = NA, cores = 1, unit = "m") {
  
  # Change projection to one with metres as the unit, a good start is ESRI:54009
  # but accuracy depends where you are in the globe. 
  if (is.na(proj)) {
    # Mollweide projection/ESRI:54009
    proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  }
  
  # Need to degrade spatial data frame to just spatial for rgeos::gDistance
  # Loss of projection so need to state it again
  if (grepl("data", class(sp1), ignore.case = TRUE)) {
    sp1 <- sp::SpatialPoints(sp1, sp::CRS("+proj=longlat +datum=WGS84"))
  }
  
  # Transform projection systems
  sp1 <- sp_transform(sp1, to = proj)
  sp2 <- sp_transform(sp2, to = proj)
  
  # Do the test
  if (cores == 1) {
    
    # Use a standard vectorised loop 
    distance <- sapply(1:length(sp1), function(i) rgeos::gDistance(sp1[i], sp2))
    
  } else {
    
    # Use a multi-threaded vectorised loop
    distance.list <- parallel::mclapply(1:length(sp1), function(i) 
      rgeos::gDistance(sp1[i], sp2), mc.cores = getOption("mc.cores", cores))
    
    # Make vector
    distance <- unlist(distance.list)
    
  }
  
  # Transform units
  if (unit == "km") {
    distance <- distance / 1000
  }
  
  # Return
  distance
  
}
