#' Function to apply a rolling mean to a numeric vector.
#' 
#' @param x Numeric vector. 
#' 
#' @param k Number of steps to apply the mean function to.
#' 
#' @param align Should the index of the result be centred, left- or 
#' right-aligned? 
#' 
#' @param na.rm Should \code{NA}s be removed for the aggregation? 
#' 
#' @seealso \code{\link{mean}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Define a vector, this a daily time series
#' x <- c(
#'   1, 1, 10, 10, 10, 13, 11, 31, 33, 61, 62, 73, 49, 69, 192, 
#'   212, 334, 357, 433, 420, 328, 1064, 1088, 1214, 836, 1147, 693, 
#'   547, 1464, 1244, 1072, 1118, 1308, 723, 433, 1310, 1137, 1016, 
#'   878, 928, 486, 281, 925, 715, 613, 559, 309, 262, 219, 245, 425, 
#'   328, 321, 291, 144, 86, 277, 213, 158, 208, 167, 87, 61, 173, 
#'   153, 120, 101, 83, 47, 22, 90, 73, 79, 46, 59, 39, 15, 46, 42, 
#'   37, 37, 41, 15, 12, 34, 36, 29, 11, 19, 14, 10, 15, 18, 25, 30, 
#'   21, 10, 7, 3, 22, 23, 17, 15, 10, 8, 25, 11, 24, 25, 34, 10, 
#'   12, 32, 21, 18, 18, 37, 31, 13, 56, 37, 64, 48, 91, 35, 22, 158, 
#'   136, 102, 127, 96, 26, 37, 105, 107, 96, 97, 110, 70, 46, 120, 
#'   128, 120, 106, 116, 80, 41, 127, 138, 117, 143, 148, 94, 55, 
#'   193, 187, 220, 211, 163, 94, 80, 193, 180, 153, 155, 183, 129, 
#'   99, 280, 255, 247, 265, 224, 86, 0
#' )
#' 
#' # Apply a rolling mean to vector, 7 is for seven days/weekly
#' rolling_mean(x, k = 7, na.rm = TRUE)
#' 
#' @export
rolling_mean <- function(x, k, align = "center", na.rm = FALSE) {
  
  # Catch Inf values
  id_inf <- which(is.infinite(x))
  if (length(id_inf)) x[id_inf] <- NA
  
  # Apply function
  x <- zoo::rollmean(x, k = k, fill = NA, align = align, na.rm = na.rm)
  
  return(x)
  
}
