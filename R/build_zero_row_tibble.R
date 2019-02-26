#' Function to create a zero row tibble, usually for pre-allocation of values. 
#' 
#' @author Stuart K. Grange
#' 
#' @param names Vector of names for the tibble. 
#' 
#' @return Tibble with names and zero rows. 
#' 
#' @export
build_zero_row_tibble <- function(names) {
  
  # Create empty data frame with correct number of variables
  data.frame(
    matrix(0, ncol = length(names), nrow = 0)
  ) %>% 
    purrr::set_names(names) %>% 
    as_tibble()
  
}
