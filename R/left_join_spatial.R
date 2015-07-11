#' Function for a point-in-polygon test. 
#' 
#' left_join_spatial tests if a point is within a spatial polygon and joins 
#' tabular data from a spatial object if the match TRUE. This process is 
#' analogous to a SQL left join with the match being a spatial intersection. 
#' 
#' Points to be tested are stored in a data frame with latitude and longitude 
#' (WGS84) while the polygons must be stored in a spatial object. The result is 
#' the input data frame with the joined data within the spatial polygon. 
#' 
#' Ensure that the data frame containing points and the polygons are both 
#' projected as WGS84 ("+proj=latlong +datum=WGS84"; see sp::spTransform). 
#' 
#' sp::over is used for the point-in-polygon test
#' 
#' Function can be rather slow when many points and many polygons are to be 
#' joined. 
#' 
#' @param df A data frame containing latitude and longitude variables. 
#' @param latitude \code{df}'s latitude variable name.
#' @param longitude \code{df}'s longitude variable name.
#' @param polygons A spatial polygon object to be joined to \code{df}.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Join air quality zone (for PM10) information for 280 000 latitude and 
#' longitude pairs within Europe
#' data.join <- left_join_spatial(data.tidy, latitude = "latitude", 
#'   longitude = "longitude", polygons = shape.file.pm10)
#' }
#' 
#' @export
#' 
left_join_spatial <- function (df, latitude = "latitude", 
                               longitude = "longitude", polygons = NULL) {
  
  # Check the spatial object
  if (!grepl("polygon", class(polygons), ignore.case = TRUE)) {
    stop("Spatial polygons must be defined in the 'polygon' argument. ")
  }
  
  # A message to the user
  message("This function assumes polygons are projected in WGS84.")
  
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
  
  # Add joined variable to data frame
  df <- cbind(df, match.df)
  
  # Return
  df
  
}
