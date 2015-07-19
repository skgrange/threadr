#' Function to convert coordinates within a data frame. 
#' 
#' \code{transform_coordinates} converts coordinate pairs from one projection 
#' system to another. \code{transform_coordinates} is useful when tabular data 
#' are supplied and the coordinate system is different that what is desired. 
#' 
#' \code{transform_coordinates} works by coercing the input data frame to a 
#' spatial object, applies \code{sp::spTransform} to convert the coordinates, 
#' converts the spatial object back to a data frame and then returns the data 
#' frame with the transformed coordinates. The transformed coordinates can be 
#' optionally renamed and reordered. 
#' 
#' \code{transform_coordinates} requires a CRS projection string for the 
#' \code{from} and \code{to} arguments. \code{to} by default is set as 
#' WGS 84/EPSG:4326 (\code{+proj=longlat +datum=WGS84}).
#' 
#' @param df Data frame with coordinates to be transformed. 
#' @param x Name of \code{x} variable. 
#' @param y Name of \code{y} variable. 
#' @param from A proj4 string which represents what coordinate system the
#' data frame \code{x} and \code{y} are in. 
#' @param to A proj4 string which represents what coordinate system the 
#' converted coordinates will be converted to.
#' @param rename Should the converted coordinates be renamed to a generic 
#' \code{x} and \code{y}?
#' @param reorder Should the converted coordinates be placed in the first two 
#' columns of the returned data frame? 
#' @param round How many decimal points should the converted coordinates be
#' rounded to? Default is 6. 
#' 
#' @seealso See \code{\link{spTransform}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' \dontrun{
#' # Convert British National Grid/Ordnance Survey National Grid/OSGB36/EPSG:7405
#' # to latitude and longitude (WGS 84/EPSG:4326)
#' 
#' # The proj4 string for British National Grid (very long string!)
#' bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs"
#' 
#' data.oxford.transform <- transform_coordinates(
#'   data.oxford, x = "x_coordinate", y = "y_coordinate", from = bng)
#' }
#'
#' @export
#' 
transform_coordinates <- function (df, x = "easting", y = "northing", from = "", 
                                   to = "+proj=longlat +datum=WGS84", 
                                   rename = TRUE, reorder = TRUE, round = 6) {
  
  # Check argument
  if (from == "") {
    stop("A 'from' projection string must defined.")
  }
  
  # Make standard data frame, catch for tbl_df
  df <- data.frame(df)
  
  # Make sp points object, x, y order
  sp::coordinates(df) <- c(x, y)
  
  # Give input a projection
  sp::proj4string(df) <- from
  
  # Convert coordinate system
  new.projection <- sp::CRS(to)
  
  # Do the projection conversion
  df <- sp::spTransform(df, new.projection)
  
  # Back to data frame
  df <- data.frame(df)
  
  # Remove optional variable
  if ("optional" %in% names(df)) {
    df$optional <- NULL
  }
  
  # Get the indices
  x.index <- which(names(df) == x)
  y.index <- which(names(df) == y)
  other.index <- which(names(df) %ni% c(x, y))
  
  # Round coordinates
  df[, x.index] <- round(df[, x.index], round)
  df[, y.index] <- round(df[, y.index], round)
  
  # Reorder df
  if (reorder) {
    df <- df[, c(y.index, x.index, other.index)]
  }
  
  # Rename variables
  if (rename) {
    names(df) <- ifelse(names(df) == x, "x", names(df))
    names(df) <- ifelse(names(df) == y, "y", names(df))
  }
  
  # Return
  df
  
}
