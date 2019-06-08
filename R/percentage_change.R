#' Function to calculate percentage change between new and old values. 
#' 
#' @param previous Previous or older value. 
#' 
#' @param new Newer value. 
#' 
#' @param as_decimal Should the percentage be represented in a 0 to 1 scale? 
#' 
#' @return Numeric vector.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Simple calculations
#' percentage_change(100, 50)
#' percentage_change(50, 100)
#' 
#' # Global mean CO2 concentrations
#' percentage_change(277, 400)
#' percentage_change(277, 400, as_decimal = TRUE)
#' 
#' @export
percentage_change <- function(previous, new, as_decimal = FALSE) {
  x <- ((new - previous) / previous) * 100
  if (as_decimal) x <- x / 100
  return(x)
}


#' Function to calculate the value of a percentage of another value. 
#' 
#' @param value Numeric vector of values.  
#' 
#' @param percent Percentage of \code{value} to calculate. \code{percent} is not
#' a decimal, \emph{i.e.} five percent is represented by 5. 
#' 
#' @return Numeric vector with length of \code{value}.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Five percent of 33864
#' percent_of(33864, 5)
#' 
#' @export
percent_of <- function(value, percent) value * (percent / 100)


#' Function to calculate the percentage lost between two values. 
#' 
#' @param value Numeric vector of values.  
#' 
#' @param value_two Numeric vector of values.  
#' 
#' @param as_decimal Should the percentage be represented in a 0 to 1 scale? 
#' 
#' @return Numeric vector with length of \code{value}.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Check the calculation, take 20 from 100
#' percent_lost(100, 80)
#' 
#' # Take 1693.2 from 33864
#' percent_lost(33864, 32170.8)
#' percent_lost(33864, 32170.8, as_decimal = TRUE)
#' 
#' @export
percent_lost <- function(value, value_two, as_decimal = FALSE) {
  x <- ((value - value_two) / value) * 100
  if (as_decimal) x <- x / 100
  return(x)
}
