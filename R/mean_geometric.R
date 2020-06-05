#' Function to calculate the geometric mean of a numeric vector. 
#' 
#' @author Stuart K. Grange and Steven P. Millard. 
#' 
#' @param x Numeric vector. 
#' 
#' @param na.rm Should \code{NA}s be removed from the summary? 
#' 
#' @return Numeric vector with a length of one. 
#' 
#' @seealso \url{https://github.com/cran/EnvStats/blob/master/R/geoMean.R}.
#' 
#' @examples
#' 
#' mean_geometric(sample(1:500, size = 10))
#' 
#' @export
mean_geometric <- function(x, na.rm = FALSE) {
  
  if (!is.vector(x, mode = "numeric") || is.factor(x)) {
    stop("'x' must be a numeric vector")
  }
  
  wna <- which(is.na(x))
  
  if (length(wna)) {
    if (na.rm) {
      x <- x[-wna]
    } else {
      return(NA) 
    }
  }
  
  if (any(x <= 0)) {
    warning("Non-positive values in `x`...", call. = FALSE)
    return(NA_real_)
  }
  
  else return(exp(mean(log(x))))
  
}
