#' Function for converting tidy data, usually post \code{ggplot2::fortify}
#' into spatial polygons. Object can be exported as geojson or retained as
#' an object.
#' 
#' spatialPolygonsfromTidyData correctly deals with holes.
#' 
#' @author Stuart Grange
#' 
#' @export
#' 
#' 
spatialPolygonsfromTidyData <- function(x, latitude = 'lat', longitude = 'long', 
                                        export = FALSE, dir.output = '~/Desktop/', 
                                        name = 'Name') {
  
  # To-do: This function correctly deal with holes, how does it do this? 
  
  # Get extras
  to.drop <- names(x) %in% c('long', 'lat', 'order', 'hole', 'piece', 'group', 
                             'id')
  info.extras <- x[!to.drop][1, ]
  
  # Get coordinate list
  # A list element will represent each group within a feature
  # Long-lat order is important
  coordinates <- plyr::dlply(x, 'group', function(x) 
    data.matrix(x[, c(longitude, latitude)]))
  
  # List of individual polygons
  polygons <- lapply(coordinates, sp::Polygon)
  # Polygons as one object
  polygons <- sp::Polygons(polygons, 1)
  
  # Make sp class
  projection.string <- '+proj=longlat +datum=WGS84'
  polygons.sp <- sp::SpatialPolygons(list(polygons), 
                                     proj4string = sp::CRS(projection.string))
  
  # Make sp dataframe
  data.sp <- cbind(info.extras)
  polygons.sp.df <- sp::SpatialPolygonsDataFrame(polygons.sp, data.sp)
  
  if(export) {
    
    # Get identifiers
    info.extras <- data.frame(polygons.sp.df)
    name.vector <- info.extras[, name][1]
    
    # Write file
    geojsonio::geojson_write(polygons.sp.df, file = 
                               str_c(dir.output, name.vector, '.geojson'))
    
  } else {
    
    # Return
    polygons.sp.df
    
  }
  
}
