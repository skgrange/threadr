#' Function to add a row number variable to a data frame. 
#' 
#' Row numbers will be a variable which is an integer, unlike 
#' \code{dplyr::add_rownames} which adds a character variable. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame to be transformed.
#' 
#' @param name Name of variable to use. \code{add_row_numbers} will silently 
#' drop an existing variable of the same name if present in \code{df}. 
#'
#' @seealso \link{add_rownames}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Add row numbers
#' data_zones <- add_row_numbers(data_zones)
#' 
#' # Or change the name
#' data_zones <- add_row_numbers(data_zones, name = "order")
#' 
#' }
#' 
#' @export
add_row_numbers <- function(df, name = "row_number") {
  
  # Drop variable if exists
  if (name %in% names(df)) df[, name] <- NULL
  
  # Create sequence of integers
  sequence <- seq.int(1, to = nrow(df))
  
  # if (start != 1) sequence <- sequence + start
  
  # Add sequence to data frame
  df[, name] <- sequence
  
  # Move variable to the first column position
  df <- arrange_left(df, name)
  
  # Return
  df
  
}
