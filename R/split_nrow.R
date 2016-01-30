#' Function to split a data frame into a list with \emph{n} elements based on 
#' number of rows. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame to be split. 
#' 
#' @param rows Number of rows for \code{df} to be split by. 
#' 
#' @return list with \emph{n} elements.
#' 
#' @examples 
#' \dontrun{
#' # Split a large data frame into chunks
#' data_large_list <- split_nrow(data_large, 500000)
#' 
#' }
#' 
#' @export
split_nrow <- function (df, rows) {
  
  # Split data frame into a list with n elements
  suppressWarnings(
    list <- split(df, (0:nrow(df) %/% rows))
  )
  
  # Return
  list
  
}

