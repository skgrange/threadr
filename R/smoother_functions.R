#' Function to smooth an \emph{x}-\emph{y} pair with loess (local regression). 
#' 
#' @param x Numeric vector. 
#' @param y Numeric vector. 
#' @param span Smoother span. 
#' @param degree Degree of the polynomials to be used.
#' 
#' @return Numeric vector with the length of \code{x} or \code{y}
#' 
#' @author Stuart K. Grange
#' 
#' @export
loess_smoother <- function(x, y, span = 0.75, degree = 2) {
  
  # Date type change if a date
  if (lubridate::is.POSIXt(x)) x <- as.numeric(x)
  
  # Model, warning suppression is for a low-level n warning
  suppressWarnings(
    model <- loess(y ~ x, na.action = na.exclude, span = span, degree = degree)
  )
  
  # Predict vector
  smooth <- predict(model, na.action = na.exclude, se = FALSE)
  
  return(smooth)
  
}


#' Function to smooth an \emph{x}-\emph{y} pair with a generalised additive 
#' model (GAM).
#' 
#' @param x Numeric vector. 
#' @param y Numeric vector. 
#' @param method Smoothing estimation method. 
#' 
#' @return Numeric vector with the length of \code{x} or \code{y}
#' 
#' @author Stuart K. Grange
#' 
#' @export
gam_smoother <- function(x, y, method = "GCV.Cp") {
  
  # Date type change if a date
  if (lubridate::is.POSIXt(x)) x <- as.numeric(x)
  
  # Model
  model <- mgcv::gam(
    y ~ s(x, bs = "cr"), na.action = na.exclude, method = method
  )
  
  # Predict vector
  smooth <- predict(model)
  
  # Drop names
  attributes(smooth) <- NULL
  
  return(smooth)
  
}
