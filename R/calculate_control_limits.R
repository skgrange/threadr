#' Function to calculate control limits, usually used for \code{XmR} control 
#' charts.
#' 
#' @param x A numeric vector. 
#' 
#' @param control_constant Constant used to transform the mean delta to 
#' sequential deviation. Through simulations, this is usually set as 1.128. 
#' 
#' @param control_multiplier Multiplier for sequential deviation to get the 
#' lower and upper control limits, typically 3. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{plot_control_chart}}
#' 
#' @export
calculate_control_limits <- function(x, control_constant = 1.128, 
                                     control_multiplier = 3) {
  
  # Some documentation here:
  # https://r-bar.net/xmr-control-chart-tutorial-examples
  
  # Drop missing data
  x <- na.omit(x)
  
  # Calculate the statistics
  # Mean
  mean <- mean(x)
  
  # Median, not used in control charts but still useful
  median <- median(x)
  
  # The absolute deltas between sequential observations
  delta <- abs(diff(x))
  
  # Mean delta
  mean_delta <- mean(delta)
  
  # Calculate sequential deviation
  sequential_deviation <- mean_delta / control_constant
  
  # Calculate upper and lower control limits
  lower <- mean - control_multiplier * sequential_deviation
  upper <- mean + control_multiplier * sequential_deviation
  
  # And range
  upper_lower_range <- upper - lower
  
  # Build a tibble return
  df <- tibble(
    mean,
    median,
    mean_delta,
    sequential_deviation,
    lower,
    upper,
    upper_lower_range
  )
  
  return(df)
  
}
