#' Function to generate all combinations of the elements within a vector.
#' 
#' @author Stuart K. Grange
#' 
#' @param x A vector to generate all combinations of elements of. 
#' 
#' @return List, with the length of 2 ^ length(\code{x}) - 1.
#' 
#' @seealso \code{\link{combn}}
#' 
#' @examples
#' 
#' # Get all combinations of three letters
#' letters <- c("s", "t", "u")
#' generate_all_combinations(letters)
#'
#' @export 
generate_all_combinations <- function(x) {
  
  # Only vectors are supported
  stopifnot(is.vector(x))
  
  # The number of combinations which will be generated
  # 2 ^ n - 1
  
  # Will generate a unique list
  x %>% 
    seq_along() %>% 
    purrr::map(~combn(x, ., FUN = list)) %>% 
    purrr::flatten()
  
}
