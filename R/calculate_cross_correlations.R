#' Function to calculate cross-correlations between two vectors and return 
#' correlation statistics by lags.
#' 
#' @param x,y Numeric vectors. 
#' 
#' @param max_lag Maximum lag to test. 
#' 
#' @param optimum_only Should the return only include the best lag? 
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' # Build vectors
#' # COVID-19 cases
#' x <- c(
#'   0.04, 0.04, 0.04, 0.04, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
#'   0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.04, 
#'   0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.05, 0.05, 0.06, 0.06, 0.07, 
#'   0.08, 0.1, 0.11, 0.13, 0.15, 0.15, 0.18, 0.21, 0.24, 0.27, 0.31, 
#'   0.33, 0.35, 0.4, 0.45, 0.51, 0.57, 0.64, 0.68, 0.7, 0.79, 0.86, 
#'   0.91, 0.95, 0.98, 0.99, 0.99, 1, 1, 0.99, 0.97, 0.94, 0.92, 0.91, 
#'   0.88, 0.84, 0.8, 0.76, 0.73, 0.72, 0.7, 0.67, 0.63, 0.6, 0.58, 
#'   0.55, 0.54, 0.53, 0.51, 0.5, 0.49, 0.48, 0.47, 0.47, 0.47, 0.46, 
#'   0.46, 0.46, 0.47, 0.48, 0.48, 0.48, 0.49, 0.5, 0.52, 0.52, 0.52, 
#'   0.53, 0.53, 0.53, 0.54, 0.54, 0.53, 0.52, 0.52, 0.52, 0.51, 0.5, 
#'   0.5, 0.47, 0.43, 0.42, 0.42, 0.41, 0.41, 0.41, 0.41, 0.41, 0.41, 
#'   0.41
#' )
#' 
#' # COVID-19 deaths
#' y <- c(
#'   0.01, 0.02, 0.02, 0.02, 0.02, 0.03, 0.03, 0.03, 0.03, 0.03, 
#'   0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.02, 0.03, 0.03, 0.02, 0.02, 
#'   0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 
#'   0.02, 0.02, 0.02, 0.02, 0.02, 0.03, 0.03, 0.04, 0.05, 0.05, 0.06, 
#'   0.06, 0.07, 0.09, 0.1, 0.12, 0.15, 0.18, 0.21, 0.23, 0.24, 0.28, 
#'   0.32, 0.37, 0.39, 0.45, 0.51, 0.57, 0.63, 0.7, 0.73, 0.81, 0.83, 
#'   0.87, 0.92, 0.93, 0.94, 0.98, 0.97, 0.99, 0.97, 0.98, 0.98, 1, 
#'   0.96, 0.95, 0.95, 0.94, 0.96, 0.97, 0.94, 0.92, 0.94, 0.95, 0.96, 
#'   0.95, 0.95, 0.95, 0.96, 0.94, 0.95, 0.92, 0.89, 0.9, 0.89, 0.89, 
#'   0.91, 0.89, 0.93, 0.96, 0.95, 0.96, 0.96, 0.97, 0.96, 0.97, 0.95, 
#'   0.95, 0.94, 0.94, 0.89, 0.9, 0.89, 0.87, 0.84, 0.84, 0.81, 0.83, 
#'   0.79, 0.73
#' )
#' 
#' # Calculate lags
#' calculate_cross_correlations(x, y, max_lag = 30)
#' calculate_cross_correlations(x, y, max_lag = 30, optimum_only = TRUE)
#' 
#' @export
calculate_cross_correlations <- function(x, y, max_lag = 50,
                                         optimum_only = FALSE) {
  
  # Calculate cross-correlations lags
  x <- tryCatch({
    ccf(
      x, 
      y, 
      lag.max = max_lag, 
      type = "correlation",
      plot = FALSE, 
      na.action = na.pass
    )
  }, error = function(e) {
    NULL
  })
  
  # Return empty tibble if the function errors
  if (is.null(x)) return(tibble())
  
  # Build tibble to return
  df <- tibble(
    lag = as.integer(x$lag),
    value = as.numeric(x$acf)
  )
  
  # Filter to best lag 
  if (optimum_only) df <- filter(df, value == max(value))
  
  return(df)
  
}
