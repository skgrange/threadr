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
  df <- move_variable(df, name)
  
  # Return
  df
  
}


#' Function to move a variable (column)  to the first position of a data frame.
#' 
#' @author Stuart K. Grange
#' 
#' @export 
move_variable <- function (df, name) {
  
  # Get location of variable
  index <- grep(stringr::str_c("\\<", name, "\\>"), names(df))
  
  # Rearrange
  df <- df[, c(c(index), (1:ncol(df))[-index])]
  
  # Return
  df
  
}
