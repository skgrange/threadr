#' Function to calculate correlations and return a tidy output. 
#' 
#' @param df Input data frame/tibble. Only numeric variables in \code{df} will
#' be used for the calculation. 
#' 
#' @param type Type of correlation statistic: \code{"pearson"} or 
#' \code{"spearman"}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
calculate_correlations <- function(df, type = "pearson") {
  
  # Check type
  stopifnot(type %in% c("pearson", "spearman"))
  
  # Create numeric matrix
  matrix_numeric <- df %>% 
    select(tidyselect::vars_select_helpers$where(is.numeric)) %>% 
    as.matrix()
  
  # Calculate correlations and tidy output
  # Message suppression is for when two or fewer observations are present 
  df <- suppressWarnings(
    matrix_numeric %>% 
      Hmisc::rcorr(type = type) %>% 
      tidy_rcorr() %>% 
      mutate(type = !!type) %>% 
      relocate(type,
               .before = r)
  )
  
  return(df)
  
}


tidy_rcorr <- function(list) {
  
  list %>% 
    purrr::set_names(stringr::str_to_lower(names(.))) %>% 
    purrr::map_dfr(tidy_rcorr_matrix, .id = "statistic") %>% 
    tidyr::pivot_wider(names_from = "statistic")
  
}


tidy_rcorr_matrix <- function(x) {
  
  x %>% 
    data.frame(check.names = FALSE) %>% 
    tibble::rownames_to_column("x") %>% 
    tidyr::pivot_longer(-x, names_to = "y")
  
}
