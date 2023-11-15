#' Function to summaries a numeric vector by calculating and minimum, maximum, 
#' range, and other summary statistics.
#' 
#' @param x Numeric vector.
#' 
#' @param inf_to_na Should any \code{Inf} or \code{-Inf} values be transformed
#' to \code{NA}? \code{Inf} values occur if there are no non-missing elements
#' in the vector and can cause issues plotting. 
#' 
#' @author Stuart K. Grange.
#' 
#' @return A tibble with one row. 
#' 
#' @seealso \code{\link{calculate_ci}}, \code{\link{calculate_quantiles}}
#' 
#' @examples 
#' 
#' # Calculate a few summaries with this vector
#' x <- c(44617L, 7066L, 17594L, 2726L, 1178L, 18898L, 5033L, 37151L,  4514L, 4000L)
#' 
#' # Calculate the summaries
#' calculate_range(x)
#' 
#' @export
calculate_range <- function(x, inf_to_na = FALSE) {
  
  # Check inputs
  stopifnot(is.numeric(x) | lubridate::is.POSIXt(x))
  
  # Get n, including all missing elements if they exist
  n_all <- length(x)
  
  # Drop all nas
  x <- x[!is.na(x)]
  
  # Get summaries
  n <- length(x)
  sd <- sd(x)
  se <- sd / sqrt(n)
  mean <- mean(x)
  median <- median(x)
  # Warning suppression is for when all elements are missing and a return of Inf
  # results
  range <- suppressWarnings(range(x))
  
  # Push non-finite results to missing
  if (inf_to_na) {
    range <- if_else(is.infinite(range), NA_real_, range)
  }
  
  # Bind together into a tibble
  df <- tibble(
    n_all,
    n, 
    sd, 
    se,
    mean, 
    median,
    min = range[1], 
    max = range[2],
    range = max - min
  )
  
  return(df)
  
}
