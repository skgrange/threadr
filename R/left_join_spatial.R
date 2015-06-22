#' Function to do a point-in-polygon test. 
#' 
#' left_join_spatial tests if a point is within a spatial polygon and returns a 
#' vector if the match TRUE. This process is analogous to a SQL left join with 
#' the match being a spatial intersection. 
#' 
#' Points are stored in a data frame with latitude and longitude (WGS84) 
#' while the polygons must be a spatial object. The result is the input date 
#' frame with the joined variable. 
#' 
#' If multiple variables are to be joined, use left_join_spatial and then do a 
#' standard join or merge on \code{data.frame(spatial.object)} using 
#' left_join_spatial return as the key variable.  
#' 
#' Ensure that the data frame containing points and the polygons are both 
#' projected as WGS84 ('+proj=latlong +datum=WGS84'). 
#' 
#' sp::over is used for the point-in-polygon test
#' 
#' Function can be rather slow when many points and many polygons are to be 
#' joined. To-do: see if this can be improved.
#' 
#' @author Stuart K. Grange
#' 
#' @export
#' 
left_join_spatial <- function(data, latitude = 'latitude', 
                              longitude = 'longitude', 
                              polygon = shape.file, 
                              variable = 'Zonename') {
  
  # A message to the user
  message('This function assumes polygons are projected in WGS84.')
  
  # Make sp points object
  sp::coordinates(data) <- c(longitude, latitude)
  # Force projection, not ideal
  proj4string(data) <- '+proj=longlat +datum=WGS84'
  
  # The point in polygon function
  x <- sp::over(data, polygon, fn = NULL)
  
  # Only one variable return
  x <- as.vector(x[, variable])
  
  # Return
  x
}

