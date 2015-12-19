#' Function to add a row number variable. 
#' 
#' Convert row numbers to a variable which is an integer, unlike 
#' \code{dplyr::add_rownames} which adds a character variable. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame to be transformed.
#' @param name Name of variable to use. 
#'
#' @seealso \link{dplyr::add_rownames}
#' 
#' @examples 
#' \dontrun{
#' # Add row numbers
#' data_zones <- add_row_numbers(data_zones)
#' 
#' # Or change the name
#' data_zones <- add_row_numbers(data_zones, name = "order")
#' 
#' }
#' 
#' @export
add_row_numbers <- function (df, name = "row_number") {
  
  # Create sequence of integers
  sequence <- seq(1, to = nrow(df))
  
  # Add sequence to data frame
  df[, name] <- sequence
  
  # Move variable to the first column position
  df <- arrange_left(df, name)
  
  # Return
  df
  
}
