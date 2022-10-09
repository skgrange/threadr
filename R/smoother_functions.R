#' Function to smooth an \emph{x}-\emph{y} pair with loess (local regression). 
#' 
#' @param x Numeric vector. 
#' 
#' @param y Numeric vector. 
#' 
#' @param span Smoother span. 
#' 
#' @param degree Degree of the polynomials to be used.
#' 
#' @param surface A \code{loess.control} option. Can take the value 
#' \code{"interpolate"} or \code{"direct"}, If \code{"direct"}, the loess model
#' will predict outside its training space. 
#' 
#' @return Numeric vector with the length of \code{x} or \code{y}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
loess_smoother <- function(x, y, span = 0.75, degree = 2, surface = "interpolate") {
  
  # Date type change if a date
  if (lubridate::is.POSIXt(x)) x <- as.numeric(x)
  
  # Put vectors into a data frame for modelling and prediction, this allows 
  # for the prediction to interpolate missing data if they exist
  df <- data.frame(y = y, x = x)
  
  # Model, warning suppression is for a low-level n warning
  suppressWarnings(
    model <- loess(
      y ~ x, 
      data = df, 
      span = span, 
      degree = degree, 
      control = stats::loess.control(surface = surface),
      na.action = na.exclude
    )
  )
  
  # Predict vector
  smooth <- unname(predict(model, df, se = FALSE))
  
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
