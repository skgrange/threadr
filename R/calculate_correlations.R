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
  
  df %>% 
    select(tidyselect::vars_select_helpers$where(is.numeric)) %>% 
    as.matrix() %>% 
    Hmisc::rcorr(type = type) %>% 
    tidy_rcorr()
  
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
