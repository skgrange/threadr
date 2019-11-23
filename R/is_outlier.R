#' Function to identify outliers in a numeric vector by uisng the same logic 
#' used for R's base boxplots. 
#' 
#' @param x Numeric vector. 
#' 
#' @param coef A coefficient which determines the classification of an outlier. 
#' Use 0 for no outliers to be identified. 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Logical vector with the length of \code{x}. 
#' 
#' @seealso \code{\link{boxplot.stats}}. 
#' 
#' @export
is_outlier <- function(x, coef = 10) {
  stopifnot(is.numeric(x))
  y <- grDevices::boxplot.stats(x, coef = coef)$out
  x <- x %in% y
  return(x)
}
