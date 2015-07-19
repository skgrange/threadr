#' Convenience function to transform a spatial object's projection system to 
#' WGS84 latitude and longitude. 
#' 
#' \code{sp_transform} is a simple wrapper for sp::spTransform which has been 
#' written so spatial objects can be transformed quickly without the need to 
#' remember the WGS84 proj4 string. 
#' 
#' @param sp Spatial object which is to be transformed.
#' @param to A proj4 string for the projection-transformation.
#' @param round Number of decimal places the transformed coordinates will be
#' rounded to. 
#' 
#' @seealso \code{\link(spTransform)}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Load a shape file of canal locks for the UK
#' shape.file <- readOGR("uk-canals", "locks")
#' projection(shape.file)
#' "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"
#' 
#' # Convert the shape file's projection (UK's Ordnance Survey National Grid)
#' to WGS84 latitude and longitude
#' shape.file <- sp_transform(shape.file)
#' 
#' projection(shape.file)
#' "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#' }
#' 
#' @import sp
#' 
#' @export
#' 
sp_transform <- function (sp, to = "+proj=longlat +datum=WGS84", round = NA) {
  
  # Convert projection system to WG
  sp <- sp::spTransform(sp, CRS(to))
  
  # Round
  if (!is.na(round)) {
    sp@coords <- round(sp@coords, round)
  }
  
  
  # Return
  sp
  
}
