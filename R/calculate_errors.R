#' Function to calculate errors for an aggregation. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Numeric vector to be summarised. 
#' 
#' @param type Type of error to calculate, either \code{"se"} or \code{"ci"}. 
#' 
#' @param level When \code{type} is \code{"ci"}, what confidence interval should
#' be calculated. \code{level} can be 90, 95, or 99. 
#' 
#' @return Data frame. 
#' 
#' @export
calculate_errors <- function(x, type = "se", level = NA) {
  
  # Check inputs
  type <- stringr::str_to_lower(type)
  stopifnot(type %in% c("se", "standard_error", "ci", "confidence_interval"))
  stopifnot(is.numeric(x))
  
  # Drop all nas
  x <- x[!is.na(x)]
  
  # Get summaries
  n <- length(x)
  sd <- sd(x)
  mean <- mean(x)
  
  # Calculate standard error
  se <- sd / sqrt(n)
  
  if (type == "ci") {
    
    # Get z score for confidence interval calculations
    if (is.na(level)) level <- 95
    
    # Get z score
    z <- ifelse(level == 90, 1.645, NA)
    z <- ifelse(level == 95, 1.96, z)
    z <- ifelse(level == 99, 2.576, z)
    
    # Multiply error by z
    error <- se * z
    
  }
  
  # Calculate bounds
  lower <- mean - error
  upper <- mean + error
  
  # Bind together into a data frame
  df <- data_frame(
    n, 
    sd, 
    mean, 
    se, 
    error_type = type, 
    error_level = as.numeric(level), 
    error, 
    lower, 
    upper
  )
  
  return(df)
  
}
