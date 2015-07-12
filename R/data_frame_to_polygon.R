#' Function for converting a data frame to spatial polygons.
#' 
#' \code{data_frame_to_polygon} conveniently transforms a data frame to a 
#' SpatialPolygonsDataFrame. 
#' 
#' \code{data_frame_to_polygon} will create closed polygons by joining the first
#' and last observations together in a straight line if the input data frame's 
#' first and last coordinate pairs to not match. 
#' 
#' \code{data_frame_to_polygon} correctly deals with holes if the variable "hole"
#' is present in the input data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Convert a hand-drawn line from an online application to a polygon
#' polygon <- data_frame_to_polygon(data.drawn.line, "latitude", "longitude")
#' }
#' 
#' @export
#' 
data_frame_to_polygon <- function (df, latitude = "latitude", 
                                   longitude = "longitude") {
  
  # Add group variable if it does not exist
  if (!"group" %in% names(df)) {
    df[, "group"] <- 1
  }
  
  # Get extras from input data frame
  other.index <- which(names(df) %ni% c(latitude, longitude))
  # Only first row. Ok? 
  data.extras <- df[other.index][1, ]
  
  # A catch for when only group is present, i.e. there are no extra identifiers
  # in df. To-do: do this better. 
  if (class(data.extras) == "numeric") {
    data.extras <- data.frame(group = data.extras)
  }
  
  # Get coordinate list
  # A list element will represent each group within a feature 
  # Long-lat order is important
  coordinates <- plyr::dlply(df, "group", function(x) 
    data.matrix(x[, c(longitude, latitude)]))
  
  # List of individual polygons
  polygons <- lapply(coordinates, sp::Polygon)
  # Polygons as one object
  polygons <- sp::Polygons(polygons, 1)
  
  # Make sp class
  projection.string <- "+proj=longlat +datum=WGS84"
  polygons.sp <- sp::SpatialPolygons(list(polygons), 
                                     proj4string = sp::CRS(projection.string))
  
  # Make sp dataframe
  polygons.sp <- sp::SpatialPolygonsDataFrame(polygons.sp, data.extras)
  
  # Return
  polygons.sp
  
}

# Define the negative %in% function
`%ni%` <- Negate(`%in%`)
