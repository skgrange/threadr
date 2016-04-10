#' Function to test if a vector is within one, or many ranges. 
#' 
#' \code{within_range} works well for testing unix time dates.  
#' 
#' @param vector Vector to test. 
#' @param begin Start of range. 
#' @param end End of range.
#' 
#' @return Logical vector of length of \code{vector}. 
#' 
#' @seealso \code{\link{which}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Test a single range
#' within_range(1:10, 8, 9)
#' 
#' 
#' # Test many ranges
#' # A vector
#' vector <- 1:10
#' 
#' # Build look-up table
#' look_up <- data.frame(
#'   start = c(1, 5),
#'   end = c(2, 9)
#' )
#' 
#' # Test vector
#' within_range(vector, look_up$start, look_up$end)
#' 
#' }
#'  
#' @author Stuart K. Grange
#' 
#' @export
within_range <- function(vector, start, end) {
  
  if (!length(start) == length(end))
    stop("'start' and 'end' must be the same length.", call. = FALSE)
  
  # Build data frame
  df_look <- data.frame(start, end)
  
  # Test each range
  logical_list <- plyr::alply(df_look, 1, function(x) 
    between_tester(vector, x$start, x$end))
  
  # Make a vector
  logical <- do.call("rbind", logical_list)
  logical <- apply(logical, 2, function(x) any(x))
  
  # Return
  logical
  
}


# No export
between_tester <- function(vector, start, end)
  ifelse(vector >= start & vector <= end, TRUE, FALSE)
