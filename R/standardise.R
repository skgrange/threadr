#' Function to standardise all variables in a data frame. 
#' 
#' \code{standardise} will transform all numeric variables so they have a mean 
#' of 0 and a standard deviation of 1. This is also known as the z-score. 
#'
#' @seealso \link{scale} 
#' 
#' @author and Stuart K. Grange
#' 
#' @param df Data frame containing numeric variables.
#' 
#' @examples 
#' \dontrun{
#' data_standardise <- standardise(data_csv)
#'
#' }
#' 
#' @export
standardise <- function (df) {
  
  # Centers and scales the variables
  matrix <- scale(df)
  
  # Back to data frame
  df <- data.frame(matrix)
  
  # Return
  df
  
}
