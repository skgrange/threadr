#' Function for a point-in-polygon test. 
#' 
#' \code{left_join_spatial} tests if a point is within a polygon and joins 
#' tabular data from a spatial object (the data part of the spatial-polygon data
#' frame) if the match is TRUE. This process is analogous to a SQL left join 
#' with the match being a spatial intersection. 
#' 
#' Points to be tested are stored in a data frame with latitude and longitude 
#' pairs while the polygons must be stored in a spatial object. The result is 
#' the input data frame with the joined data contained within the 
#' spatial-polygon data frame.
#' 
#' Ensure that the data frame containing points and the polygon object are both 
#' projected as WGS84 (\code{"+proj=latlong +datum=WGS84"; see sp::spTransform}). 
#' 
#' \code{sp::over} is used for the point-in-polygon test
#' 
#' \code{left_join_spatial} can be rather slow when many points and many 
#' polygons are to be joined. 
#' 
#' @param df Data frame containing latitude and longitude variables. 
#' @param latitude \code{df}'s latitude variable name.
#' @param longitude \code{df}'s longitude variable name.
#' @param polygons A spatial-polygon data frame to be joined to \code{df}.
#' 
#' @seealso See \code{\link{spTransform}}, \code{\link{over}}, 
#' \code{\link{merge}}, \code{\link{join}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Join PM10 air quality zones to 280 000 latitude and longitude pairs
#' data.join <- left_join_spatial(data.tidy, latitude = "latitude", 
#'   longitude = "longitude", polygons = shape.file.pm10)
#' 
#'  
#' # I am in London, I know the latitude and longitude. Which borough am I in? 
#' 
#' # ... load shape file containing the London boroughs
#' 
#' # Make latitude and longitude a data frame
#' data.point <- data.frame(latitude = 51.523595, longitude = -0.027114)
#' 
#' # Test point with 33 polygons (boroughs) in the shape file
#' left_join_spatial(data.point, polygons = shape.file)
#' "Tower Hamlets"
#' }
#' 
#' @export
#' 
left_join_spatial <- function (df, latitude = "latitude", 
                               longitude = "longitude", polygons = NA) {
  
  # Check the spatial object
  if (!grepl("polygon", class(polygons), ignore.case = TRUE)) {
    stop("Spatial polygons must be defined in the 'polygon' argument. ")
  }
  
  # Catch for dplyr's data frame class
  if ("tbl" %in% class(df)) {
    df <- data.frame(df)
  }
  
  # A message to the user
  message("This function assumes points and polygons are projected in WGS84.")
  
  # Make sp points object
  sp::coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp.object <- df
  
  # Force projection, not ideal
  proj4string(sp.object) <- "+proj=longlat +datum=WGS84"
  
  # The point in polygon function
  match.df <- sp::over(sp.object, polygons, fn = NULL)
  
  # Input back to data frame
  df <- data.frame(sp.object)
  
  # Drop logical optional variable which occurs during sp::over
  if ("optional" %in% names(df)) {
    df[, "optional"] <- NULL
  }
  
  # Add joined variable to data frame
  df <- cbind(df, match.df)
  
  # Return
  df
  
}
