#' Function to round all numerical variables in a data frame.
#' 
#' round_numeric is useful when many variables are contained within a data frame 
#' and all numerical values want to be rounded without individual naming.
#'
#' @param df A data frame containing numeric variables.
#' @param round Number of decimal places, used in \code{round}. Default is 1.
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' # Round all numeric variables to two decimal places
#' \dontrun{
#' data.tidy.round <- round_numeric(data.tidy, round = 2)
#' 
#' # or, make all numeric variables integers
#' data.tidy.integer <- round_numeric(data.tidy, round = 0)
#' }
#' 
#' @export
#'
round_numeric <- function (df, round = 1) {
  
  # Get index
  index.numeric <- sapply(df, is.numeric)
  
  # Apply function to all the numeric variables in data frame
  df[index.numeric] <- lapply(df[index.numeric], function(x) round(x, round))
  
  # Return
  df
  
}