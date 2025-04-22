#' Function to calculate confidence intervals (CIs), usually based on population
#' standard deviation. 
#' 
#' @description The CI calculations use the 
#' \code{\link[Hmisc:smean.sd]{smean.cl.normal}} and 
#' \code{\link[Hmisc:smean.sd]{smean.cl.boot}} functions from the \strong{Hmisc} 
#' package that were developed by Frank Harrell. These functions are also used 
#' internally by \strong{ggplot2}. 
#' 
#' @param x Numeric vector.
#' 
#' @param level Confidence interval to calculate. This is a decimal, 
#' \strong{i.e.}, 0.95 is the 95 \% CI. 
#' 
#' @param method Method to use for CI calculation. Can be one of two methods: 
#' \code{standard} or \code{bootstrap}. \code{standard} calculates the sample 
#' mean and lower and upper Gaussian confidence limits based on the 
#' t-distribution. \code{bootstrap} uses non-parametric bootstrapping for
#' calculating the lower and upper limits without assuming normality. 
#' 
#' @param n_samples When method is \code{bootstap}, how many bootstrap samples 
#' should be taken? 
#' 
#' @author Stuart K. Grange and Frank Harrell.
#' 
#' @return A tibble with one row. 
#' 
#' @seealso \code{\link{calculate_quantiles}}, \code{\link{calculate_range}}
#' 
#' @examples 
#' 
#' # Calculate confidence intervals
#' x <- c(44617L, 7066L, 17594L, 2726L, 1178L, 18898L, 5033L, 37151L,  4514L, 4000L)
#' 
#' calculate_ci(x)
#' calculate_ci(x, level = 0.9)
#' calculate_ci(x, method = "bootstrap")
#' 
#' @export
calculate_ci <- function(x, level = 0.95, method = "standard", n_samples = 1000) {
  
  # Check method argument, used for logic
  stopifnot(method %in% c("standard", "bootstrap"))
  
  # Get n, including potential missing elements
  n_all <- length(x)
  
  # Drop all nas
  x <- x[!is.na(x)]
  
  # Get summaries
  n <- length(x)
  sd <- sd(x)
  
  # Calculate standard error
  se <- sd / sqrt(n)
  
  # Calculate CIs using one of two methods
  if (method == "standard") {
    x_ci <- Hmisc::smean.cl.normal(
      x, mult = qt((1 + level) / 2, n - 1), conf.int = level, na.rm = TRUE
    )
  } else {
    x_ci <- Hmisc::smean.cl.boot(
      x, conf.int = level, B = n_samples, na.rm = TRUE, reps = FALSE
    )
  }
  
  # Drop names
  x_ci <- unname(x_ci)
  
  # Build tibble
  df <- tibble(
    n_all, 
    n, 
    sd,
    se,
    level, 
    mean = x_ci[1], 
    lower = x_ci[2], 
    upper = x_ci[3]
  )
  
  return(df)
  
}
