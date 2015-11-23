#' Function to round all numerical variables in a data frame.
#' 
#' \code{round_numeric} is useful when many variables are contained within a 
#' data frame  and all numerical values want to be rounded without individual 
#' naming.
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
#' data_tidy_round <- round_numeric(data_tidy, round = 2)
#' 
#' # or, make all numeric variables integers
#' data_tidy_integer <- round_numeric(data_tidy, round = 0)
#' 
#' }
#' 
#' @export
round_numeric <- function (df, round = 1) {
  
  # Get index
  index <- sapply(df, is.numeric)
  
  # Apply function to all the numeric variables in data frame
  df[index] <- lapply(df[index], function(x) round(x, round))
  
  # Return
  df
  
}
