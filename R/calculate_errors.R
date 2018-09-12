#' Function to calculate errors for an aggregation. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Numeric vector to be summarised. 
#' 
#' @param type Type of error to calculate, either \code{"se"} or \code{"ci"}. 
#' 
#' @param level When \code{type} is \code{"ci"}, what confidence interval should
#' be calculated. \code{level} can be 90, 95, 98, or 99 (which represent 
#' percentage). 
#' 
#' @return Tribble.
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
    
  } else {
    
    # Reasign for next calculation
    error <- se
    
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


# Helper table for z scores
df_z_scores <- dplyr::tribble(
  ~confidence, ~z_score,
  90L, 1.645,
  95L, 1.96,
  98L, 2.326,
  99L, 2.576,
)
