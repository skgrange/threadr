#' Function to calculate percentage change between new and old values. 
#' 
#' @param previous Previous or older value. 
#' 
#' @param new Newer value. 
#' 
#' @return Numeric vector.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # A simple calculation
#' percentage_change(100, 50)
#' 
#' @export
percentage_change <- function(previous, new) ((new - previous) / previous) * 100
