#' Function to create a zero row tibble, usually for pre-allocation of values. 
#' 
#' @author Stuart K. Grange
#' 
#' @param names Vector of names for the tibble. 
#' 
#' @param base Despite the function's name, should a \strong{base} 
#' \code{data.frame} be returned rather than a tibble? 
#' 
#' @return Tibble with names and zero rows. 
#' 
#' @export
build_zero_row_tibble <- function(names, base = FALSE) {
  
  # Create empty data frame with correct number of variables
  df <- data.frame(
    matrix(0, ncol = length(names), nrow = 0)
  ) %>% 
    purrr::set_names(names) 
  
  # To tibble
  if (!base) {
    df <- as_tibble(df)
  }
  
  return(df)
  
}
