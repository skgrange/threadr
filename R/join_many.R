#' Function for joining many data frames together. Also called a multi-merge. 
#' 
#' @param dfs A \emph{list} of data frames.
#' @param all \code{merge}'s argument for a left-join. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Join three data frames together by a common key variable
#' data_join <- join_many(list(data_dates, data_min, data_max))
#' }
#' 
#' @export
join_many <- function (dfs, all = TRUE) {
  
  df <- Reduce(function(...) merge(..., all = all), dfs)
  
  # Return
  df
  
}
