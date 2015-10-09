#' Function to standardise all variables in a data frame. 
#' 
#' \code{standardise} will transform all numeric variables so they have a mean 
#' of 0 and a standard deviation of 1. 
#' 
#' @author http://inseaddataanalytics.github.io/INSEADAnalytics/Report_s45.html 
#' and Stuart K. Grange
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
#' 
standardise <- function (df) {
  
  # http://inseaddataanalytics.github.io/INSEADAnalytics/Report_s45.html
  x <- apply(df, 2, function(r) {
    if (sd(r) != 0) res = (r - mean(r)) / sd(r)
    else res = 0 * r
    res
    
  })
  
  x <- data.frame(x)
  
}
