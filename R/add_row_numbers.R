#' Function to add a row number variable to a data frame. 
#' 
#' Row numbers will be a variable which is an integer, unlike 
#' \code{rownames_to_column} which adds a character variable. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame to be transformed.
#' 
#' @param name Name of variable to use. \code{add_row_numbers} will silently 
#' drop an existing variable of the same name if present in \code{df}. 
#' 
#' @param zero_based Should the numbering start at \code{0} rather than \code{1}? 
#' Default is \code{FALSE}. 
#'
#' @seealso \code{\link{add_rownames}}
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
#' # Use zero-based numbering
#' data_zones <- add_row_numbers(data_zones, name = "id", zero_based = TRUE)
#' 
#' }
#' 
#' @export
add_row_numbers <- function(df, name = "row_number", zero_based = FALSE) {
  
  # Drop variable if exists
  if (name %in% names(df)) df[, name] <- NULL
  
  # Create sequence of integers
  sequence <- seq.int(1, to = nrow(df))
  
  if (zero_based) sequence <- sequence - 1L
  
  # Add sequence to data frame
  df[, name] <- sequence
  
  # Move variable to the first column position
  df <- arrange_left(df, name)
  
  # Return
  df
  
}
