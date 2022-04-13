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
  
  # Skip if fewer than 4 observations
  if (nrow(df) < 4) {
    
    # Raise warning
    warning(
      "There are fewer than four observations, correlations cannot be calculated...", 
      call. = FALSE
    )
    
    return(tibble())
    
  }
  
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
      rowwise() %>% 
      mutate(sorted_variables = str_c_sorted_pairs(x, y)) %>% 
      ungroup() %>% 
      mutate(type = !!type) %>% 
      relocate(sorted_variables,
               type,
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


str_c_sorted_pairs <- function(x, y) {
  stringr::str_c(sort(c(x, y)), collapse = "-")
}
