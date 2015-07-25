#' Function for joining many data frames together. Also called a multi-merge. 
#' 
#' @param dfs A \emph{list} of data frames.
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Join three data frames together by a common key variable
#' data.join <- join_many(list(data.dates, data.min, data.max))
#' }
#' 
#' @export
#' 
join_many <- function (dfs) {
  
  df <- Reduce(function(...) merge(..., all = TRUE), dfs)
  
  df
  
}