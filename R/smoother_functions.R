#' Function to smooth an \emph{x}-\emph{y} pair with loess (local regression). 
#' 
#' @author Stuart K. Grange
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
#' @export
loess_smoother <- function(x, y, span = 0.75, degree = 2, 
                           surface = "interpolate") {
  
  # Date type change if a date
  if (lubridate::is.POSIXt(x)) {
    x <- as.numeric(x)
  }
  
  # Put vectors into a data frame for modelling and prediction, this allows 
  # for the prediction to interpolate missing data if they exist
  df <- tibble(y = y, x = x)
  
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
  value_predict <- unname(predict(model, df, se = FALSE))
  
  return(value_predict)
  
}


#' Function to smooth an \emph{x}-\emph{y} pair with a generalised additive 
#' model (GAM).
#' 
#' @author Stuart K. Grange
#' 
#' @param x Numeric vector. 
#' 
#' @param y Numeric vector. 
#' 
#' @param method Smoothing estimation method. 
#' 
#' @return Numeric vector with the length of \code{x} or \code{y}.
#' 
#' @export
gam_smoother <- function(x, y, method = "GCV.Cp") {
  
  # Date type change if a date
  if (lubridate::is.POSIXt(x)) {
    x <- as.numeric(x)
  }
  
  # Model
  model <- mgcv::gam(
    y ~ s(x, bs = "cr"), na.action = na.exclude, method = method
  )
  
  # Predict
  value_predict <- predict(model)
  
  # Drop names
  attributes(value_predict) <- NULL
  
  return(value_predict)
  
}


#' Function to predict the linear function of \emph{x}-\emph{y} pair. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Numeric vector. 
#' 
#' @param y Numeric vector. 
#' 
#' @param robust Should robust linear regression be used rather than ordinary 
#' least squares? 
#' 
#' @return Numeric vector with the length of \code{x} or \code{y}. 
#' 
#' @export
linear_fit <- function(x, y, robust = FALSE) {
  
  # Put vectors into a data frame for modelling and prediction
  df <- tibble(y = y, x = x)
  
  # Train model
  if (robust) {
    model <- MASS::rlm(y ~ x, data = df)
  } else {
    model <- lm(y ~ x, data = df)
  }
  
  # Predict vector
  value_predict <- unname(predict(model, df, se = FALSE))
  
  return(value_predict)
  
  
}
