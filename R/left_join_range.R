#' Function to join two data frames together based on a range condition. 
#' 
#' \code{left_join_range} will test if a variable in table \code{x} is within a 
#' range of two variables in table \code{y} and join the other variables if 
#' \code{TRUE}.
#' 
#' @param x,y Data frames to join. 
#' 
#' @param x_test Variable name in \code{x} which should be tested for a range. 
#' 
#' @param y_min,y_max Variables in \code{y} which define the range for the test
#' against \code{x_test}. 
#' 
#' @param keep Should the \code{y_min} and \code{y_max} be kept in the joined 
#' return? 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Code urban area population into defined groups
#' data_join <- left_join_range(
#'   data_urban, 
#'   data_urban_class, 
#'   "population", 
#'   "size_min", 
#'   "size_max"
#'  )
#' 
#' }
#' 
#' @export
left_join_range <- function(x, y, x_test, y_min, y_max, keep = FALSE) {
  
  # Drop dplyr's data tables
  x <- base_df(x)
  y <- base_df(y)
  
  # Give y an integer key
  y <- add_row_numbers(y, name = "row_number")
  
  y_map <- y[, c("row_number", y_min, y_max)]
  names(y_map) <- ifelse(names(y_map) == y_min, "minimum", names(y_map))
  names(y_map) <- ifelse(names(y_map) == y_max, "maximum", names(y_map))
  
  # For every observation in mapping table
  index <- alply(y_map, 1, function(z) join_conditional_worker(x[, x_test], z))

  # Make a matrix
  index <- do.call("rbind", index)
  
  # Make a vector
  # Observations with greater id will over-ride observations with lesser id
  index <- apply(index, 2, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  
  # Add index variable to x
  x[, "row_number"] <- index
  
  # Drop range
  if (!keep) y[, c(y_min, y_max)] <- NULL
  
  # Do the join
  x <- left_join(x, y, by = "row_number")
  
  # Drop joining key
  x[, "row_number"] <- NULL
  
  # Return
  x
  
}


# No export needed
join_conditional_worker <- function(x_test, y_map) {
  
  # The range tester
  index <- within_range(x_test, y_map$minimum, y_map$maximum)
  
  # Switch logical to index variable
  index <- ifelse(index, y_map$row_number, NA)
  
  # Return
  index
  
}
