#' Function to replicate rows in a data frame. 
#' 
#' @param df Data frame to be replicated. 
#' 
#' @param n Number of times each row is to be replicated. 
#' 
#' @param reset Should row names be reset? Default is \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' data_replicated <- replicate_rows(data_test, 10)
#' 
#' }
#' 
#' @export
replicate_rows <- function(df, n, reset = TRUE) {
  
  # Reset row names
  if (reset) row.names(df) <- NULL
  
  # Replicate
  df <- df[rep(seq_len(nrow(df)), each = n), ]
  
  # Reset row numbers
  if (reset) row.names(df) <- NULL
  
  # Return
  df
  
}