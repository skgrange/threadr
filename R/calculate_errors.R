#' Function to calculate errors for an aggregation. 
#' 
#' @param x Numeric or date vector to be summarised. 
#' 
#' @param type Type of error to calculate, either \code{"se"}, \code{"ci"}, or 
#' \code{"range"}. 
#' 
#' @param level When \code{type} is \code{"ci"}, what confidence interval should
#' be calculated. \code{level} can be 90, 95, 98, 99, or 99.9 (which represent 
#' percentage). 
#' 
#' @author Stuart K. Grange. 
#' 
#' @return Tibble.
#' 
#' @examples 
#' 
#' # Create a vector
#' x <- sample(1:500)
#' 
#' # Calculate errors
#' calculate_errors(x)
#' 
#' # Calculate errors with confidence intervals
#' calculate_errors(x, type = "ci", level = 95)
#' 
#' # Calculate errors with ranges
#' calculate_errors(x, type = "range")
#' 
#' @export
calculate_errors <- function(x, type = "se", level = NA) {
  
  # Check inputs
  type <- stringr::str_to_lower(type)
  type <- stringr::str_trim(type)
  stopifnot(type %in% c("se", "standard_error", "ci", "confidence_interval", "range"))
  stopifnot(is.numeric(x) | lubridate::is.POSIXt(x))
  
  # Drop all nas
  x <- x[!is.na(x)]
  
  # Get summaries
  n <- length(x)
  sd <- sd(x)
  mean <- mean(x)
  
  # Calculate standard error
  se <- sd / sqrt(n)
  
  if (type %in% c("ci", "confidence_interval")) {
    
    # Get z score for confidence interval calculations
    if (is.na(level)) level <- 95
    
    # Get z score from look up table
    z <- df_z_scores %>% 
      filter(confidence == !!level) %>% 
      pull(z_score)
    
    # Check
    if (length(z) == 0) stop("Level not recognised...", call. = FALSE)
    
    # Multiply error by z
    error <- se * z
    
  } else if (type %in% c("se", "standard_error")) {
    
    # Reasign for next calculation
    error <- se
    
  } else if (type == "range") {
    
    error <- NA_real_
    
  }
  
  if (type == "range") {
    
    # Calculate min and max, these variables will be renamed once in the tibble
    lower <- min(x)
    upper <- max(x)
    
  } else {
    
    # Calculate bounds
    lower <- mean - error
    upper <- mean + error
    
  }
  
  # Bind together into a tibble
  df <- tibble(
    n, 
    sd, 
    mean, 
    se, 
    cv = sd / mean,
    error_type = type, 
    error_level = as.numeric(level), 
    error, 
    lower, 
    upper
  )
  
  if (type == "range") {
    
    # Rename lower and upper
    df <- df %>% 
      rename(min = lower,
             max = upper)
    
  }
  
  return(df)
  
}


# Helper table for z scores
df_z_scores <- dplyr::tribble(
  ~confidence, ~z_score,
  90, 1.645,
  95, 1.96,
  98, 2.326,
  99, 2.576,
  99.9, 3
)
