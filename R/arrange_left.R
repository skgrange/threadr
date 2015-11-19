#' Function to arrange variables to the left of a data frame.
#' 
#' \code{arrange_left} is useful when some variables/columns want to be moved to
#' the left-side of a data frame. 
#' 
#' @param df Data frame which is to be transformed. 
#' @param variable Variable(s) to be moved to the left side of \code{df}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples  
#' \dontrun{
#' # Move a single variable to the first position
#' data_ozone <- arrange_left(data_ozone, "date")
#' 
#' # Move many variables to left hand side of data frame
#' data_ozone <- arrange_left(data_ozone, c("date", "site", "site_name, "sensor"))
#' 
#' }
#' 
#' @export
arrange_left <- function (df, variable) {
  
  # Bound strings
  variable <- stringr::str_c("\\b", variable, "\\b")
 
  # Create index
  index <- sapply(variable, function (x) grep(x, names(df)), USE.NAMES = FALSE)
  
  # Rearrange
  df <- df[, c(c(index), (1:ncol(df))[-index])]
  
  # Return
  df
  
}
