#' Functions to calculate various error statistics. 
#' 
#' All functions will drop any missing elements before the calculation is done. 
#' 
#' @author Stuart K. Grange
#' 
#' @param value_predict A numeric vector to test against the "truth" 
#' (\code{value}). 
#' 
#' @param value A numeric vector that is considered the "truth" or observed.
#' 
#' @param method For \code{correlation}, what method to use? Usually, this 
#' will be either \code{"pearson"} or \code{"spearman"}. 
#' 
#' @return Numeric vector with length of 1. 
#' 
#' @export
rmse <- function(value_predict, value) {
  
  # Create a tibble and drop any missing pairs
  df <- tibble(value_predict, value) %>%
    tidyr::drop_na()
  
  # Calculate the statistic
  x <- mean((df$value - df$value_predict) ^ 2) ^ 0.5
  
  return(x)
  
}


#' @rdname rmse
#' @export
mean_bias <- function(value_predict, value) {
  
  # Create a tibble and drop any missing pairs
  df <- tibble(value_predict, value) %>%
    tidyr::drop_na()
  
  # Calculate the statistic
  x <- mean(df$value_predict - df$value)
  
  return(x)
  
}


#' @rdname rmse
#' @export
mean_bias_normalised <- function(value_predict, value) {
  
  # Create a tibble and drop any missing pairs
  df <- tibble(value_predict, value) %>%
    tidyr::drop_na()
  
  # Divide the difference by the reference value
  x <- sum(df$value_predict - df$value) / sum(df$value)
  
  return(x)
  
}


#' @rdname rmse
#' @export
correlation <- function(value_predict, value, method = "pearson") {
  
  # Create a tibble and drop any missing pairs
  df <- tibble(value_predict, value) %>%
    tidyr::drop_na()
  
  # If no pairs, return NA, the function will error otherwise
  if (nrow(df) == 0L) {
    return(NA_real_)
  }
  
  # Calculate the statistic
  suppressWarnings(
    x <- stats::cor.test(df$value_predict, df$value, method = method)
  )
  
  # Extract the estimate from the object
  x <- unname(x$estimate)
  
  return(x)
  
}


#' @rdname rmse
#' @export
n_pairs <- function(value_predict, value) {
  
  # Create a tibble and drop any missing pairs
  df <- tibble(value_predict, value) %>%
    tidyr::drop_na()
  
  # Calculate the statistic
  x <- nrow(df)
  
  return(x)
  
}


#' @rdname rmse
#' @export
mae <- function(value_predict, value) {
  
  # Create a tibble and drop any missing pairs
  df <- tibble(value_predict, value) %>% 
    tidyr::drop_na()
  
  # Calculate the statistic
  x <- sum(abs(df$value - df$value_predict)) / nrow(df)
  
  return(x)
  
}
