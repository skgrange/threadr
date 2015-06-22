invalidateOzoneData <- function(x = data.bind, 
                                file.invalidation = 'auckland_ozone_network_sensors_invalidation.csv', 
                                file.sensors = 'auckland_ozone_network_sensors.csv', invalidate.zero = T) {
  
  
  # Load data ---------------------------
  # Sensor information
  info.sensors <- read.csv(file.sensors, stringsAsFactors = F, comment.char = '#', 
                           colClasses = c('imei' = 'character')) %>%
    distinct(imei) %>%
    select(imei, sensor.name)
    
  # Load invalidation table
  info.invalidation <- read.csv(file.invalidation, skip = 1) %>%
    mutate(date.start = parse_date_time(date.start, c('ymd', 'ymd_hm', 'ymd_hms')), 
           date.end = parse_date_time(date.end, c('ymd', 'ymd_hm', 'ymd_hms'))) %>%
    merge(info.sensors, by = 'sensor.name')
    
  
  # Invalidate data ---------------------------
  # Pre-allocate with logged ozone
  x$o3.valid <- x$o3.logged
  
  # Invalidate zeros because sensor heads do not read negatives,
  # therefore this is missing data
  if(invalidate.zero) {
    x$o3.valid[x$o3.valid == 0] <- NA
  }
  
  # Conditional merge
  for(i in 1:nrow(info.invalidation)) {
    x$o3.valid[x$imei == info.invalidation$imei[i] & x$date <= info.invalidation$date.end[i] & 
                x$date >= info.invalidation$date.start[i]] <- NA
  }
  
  # Return
  x
}


conditionalJoin <- function (x = data.table, x.key = 'pollutant', 
                             y = look.up.table,  y.key = 'xml.variable',
                             y.insert = 'variable') {
  
  # A for-loop in R?!
  # To-do: vectorise
  # For each row in look-up table
  for (i in seq_along(y)) {
    
    # Conditionally match and replace if it does match
    x[, x.key][x[, x.key] == y[, y.key][i]] <- y[, y.insert][i]
    
  }
  
  # Return
  x
  
}