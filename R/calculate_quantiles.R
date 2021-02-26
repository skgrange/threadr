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
#' @param na.rm Should missing values be removed for calculation? 
#' 
#' @seealso \code{\link{quantile}}
#' 
#' @return Tibble. 
#' 
#' @export
calculate_quantiles <- function(x, probabilities = c(0.05, 0.95), type = 7, 
                                na.rm = FALSE) {
  
  # Check input
  stopifnot(length(probabilities) == 2L)
  
  x %>% 
    quantile(probs = probabilities, names = FALSE, type = type, na.rm = na.rm) %>% 
    t() %>% 
    data.frame(check.names = FALSE, fix.empty.names = FALSE) %>% 
    purrr::set_names(c("lower", "upper")) %>% 
    mutate(probabilities = list(probabilities)) %>%
    relocate(probabilities) %>% 
    as_tibble()
  
}
