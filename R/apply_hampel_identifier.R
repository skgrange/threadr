#' Function to apply the Hampel identifier, a median absolute deviation (MAD) 
#' outlier detector. 
#' 
#' The Hampel identifier is robust against outliers and omits all missing 
#' (\code{NA}) elements before calculation. 
#' 
#' @author Hans W. Borchers and Stuart K. Grange
#' 
#' @param x A numeric vector, typically a time series.
#' 
#' @param k An integer representing the window length of \code{x} that is 
#' evaluated for the outlier detection.
#' 
#' @param t0 The outlier threshold. The default of 3 is Ron Pearson's 
#' three-sigma edit rule. 
#' 
#' @param values Should the filtered verson of the \code{x} vector be returned 
#' rather than a logical vector representing the identification of outliers? 
#' 
#' @param na_preserve If \code{value} is \code{TRUE}, should the missing 
#' elements of \code{x} be propagated into the returned numeric vector? 
#' 
#' @seealso \url{https://github.com/cran/pracma}
#' 
#' @return Numeric or logical vector depending on \code{values}. 
#' 
#' @export
apply_hampel_identifier <- function(x, k = 60, t0 = 3, values = FALSE, 
                                    na_preserve = TRUE) {
  
  # Check data type
  stopifnot(inherits(x, "numeric"))
  
  # Does the vector have any missing elements?
  has_na <- anyNA(x)
  
  # If the vector contains missing observations, the original is needed after
  # na omitting
  if (has_na) {
    
    # Where are the missing elements?
    x_na_index <- which(is.na(x))
    
    # Store original vector
    x_original <- x
    
    # Drop nas
    x <- na.omit(x)
    
    # Drop the added attribute
    attr(x, "na.action") <- NULL
    
  } else {
    # Used for replacement logic
    x_na_index <- as.integer()
  }
  
  # Apply the hampel identifier, a median absolute deviation filter
  list_filter <- hampel(x, k = k, t0 = t0)
  
  if (values) {
    # Extract the numeric vector from the return
    x_filter <- list_filter$y
  } else {
    # Create a logical vector with correct length
    x_filter <- logical(length(x))
    # Set outliers
    x_filter[list_filter$ind] <- TRUE
  }
  
  # Insert missing elements to filtered vector if they were dropped
  if (has_na) {
    x_filter <- replace(x_filter, !is.na(x_original), x_filter)
  }
  
  # For the logical outlier vector, missing values should always be missing
  if (!values) {
    x_filter[x_na_index] <- NA
  }
  
  # Replace the missing values in the original vector with NA too
  if (values && na_preserve) {
    x_filter[x_na_index] <- NA
  }
  
  return(x_filter)
  
}


# Pulled from the pracma package
# https://github.com/cran/pracma/blob/master/R/hampel.R
##
##  h a m p e l . R  MAD Outlier in Time Series
##
hampel <- function (x, k, t0 = 3)
{
  #   x:  vector or time series
  #   k:  window [x_(i-k),...,x_i,...,x_(i+k)]
  n   <- length(x)
  y   <- x         # corrected x vector
  ind <- c()       # indices of outliers
  
  L  <- 1.4826     # constants for normal distributions
  # t0 <- 3        # Pearson's 3 sigma edit rule
  
  # we don't look at outliers at the end parts of x !
  for ( i in (k+1):(n-k) ) {
    x0 <- median( x[(i-k):(i+k)] )
    S0 <- L * median( abs(x[(i-k):(i+k)] - x0) )
    if ( abs(x[i]-x0) > t0 * S0 ) {
      y[i] <- x0
      ind  <- c(ind, i)
    }
  }
  # return a list with 2 components
  list(y=y, ind=ind)
}
