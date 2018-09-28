#' Function to split a data frames and vectors into lists with \emph{n} elements
#' based on number of rows or length. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame to be split. \code{df} can also be a vector.  
#' 
#' @param rows Number of rows for \code{df} to be split by. In the case of a 
#' vector, \code{rows} is length. 
#' 
#' @return list with \emph{n} elements.
#' 
#' @examples 
#' \dontrun{
#' 
#' # Split a large data frame into chunks
#' data_large_list <- split_nrow(data_large, 500000)
#' 
#' }
#' 
#' @export
split_nrow <- function(df, rows) {
  
  if (any(grepl("data.frame", class(df)))) {
    
    # Split data frame into a list with n elements
    suppressWarnings(
      list_split <- split(df, (0:nrow(df) %/% rows))
    )
    
  } else if (any(class(df) %in% c("character", "integer", "numeric", "factor"))) {
    
    list_split <- split(df, ceiling(seq_along(df) / rows))
    
  }
  
  return(list_split)
  
}
