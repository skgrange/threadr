#' Function to calculate quantiles for a numeric vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x A numeric vector. 
#' 
#' @param probabilities A numeric vector with the length of two representing
#' probabilities of the quantiles. These values will always be less than 1. 
#' 
#' @param type Method to use to calculate the quantiles. 
#' 
#' @param keep_probabilities Should the probabilities vector be kept? If 
#' \code{TRUE}, the return will contain a nested column. 
#' 
#' @param na.rm Should missing values be removed for calculation? 
#' 
#' @seealso \code{\link{quantile}}
#' 
#' @return Tibble. 
#' 
#' @export
calculate_quantiles <- function(x, probabilities = c(0.05, 0.95), type = 7, 
                                keep_probabilities = FALSE, na.rm = FALSE) {
  
  # Check input
  stopifnot(length(probabilities) == 2L)
  
  # Calculate averages
  mean <- mean(x, na.rm = na.rm)
  median <- median(x, na.rm = na.rm)
  
  # Calculate counts
  n_all <- length(x)
  n <- length(na.omit(x))
  
  # Do the calculation
  df <- x %>% 
    quantile(probs = probabilities, names = FALSE, type = type, na.rm = na.rm) %>% 
    t() %>% 
    data.frame(check.names = FALSE, fix.empty.names = FALSE) %>% 
    purrr::set_names(c("lower", "upper")) %>% 
    mutate(probabilities = list(probabilities),
           mean = !!mean,
           median = !!median,
           n_all = !!n_all,
           n = !!n) %>%
    relocate(n_all, 
             n, 
             mean,
             median) %>% 
    as_tibble()
  
  # Drop probabilities column
  if (!keep_probabilities) {
    df <- select(df, -probabilities)
  } else {
    df <- relocate(df, probabilities)
  }
  
  return(df)
  
}
