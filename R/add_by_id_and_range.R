#' Function to add a variable by testing an identifier and a range. 
#' 
#' @param df Data frame to test. 
#' 
#' @param test Variable in \code{df} for the range test.
#' 
#' @param df_map Mapping data frame containing \code{by}, \code{min}, \code{max},
#' and \code{add} variables. 
#' 
#' @param by An identifying variable in both \code{df} and \code{df_map} which is
#' to be used for the matching test.
#' 
#' @param min,max Variables in \code{df_map} which will be used for the range
#' test, i.e., is \code{test} between \code{min} and \code{max}? 
#' 
#' @param add Variable in \code{df_map} to add to \code{df}. Generally, this 
#' will be an integer key. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
add_by_id_and_range <- function(df, test, df_map, by, min, max, add) {
  
  # Check inputs
  stopifnot(c(test, by) %in% names(df))
  stopifnot(c(by, add, min, max) %in% names(df_map))
  
  # Determine what NA type to use
  na_type <- df_map %>% 
    select(!!add) %>% 
    pull() %>% 
    class() %>% 
    get_na_type()
  
  # Pre-allocate variable
  df <- mutate(df, !!add := na_type)
  
  # Test and replace
  for (i in seq_len(nrow(df_map))) {
    
    # Repeatedly mutate in place
    df <- df %>% 
      mutate(
        !!add := if_else(
          !!sym(by) == !!df_map[i, by, drop = TRUE] & 
            !!sym(test) >= !!df_map[i, min, drop = TRUE] & 
            !!sym(test) <= !!df_map[i, max, drop = TRUE],
          !!df_map[i, add, drop = TRUE], 
          !!sym(add)
        )
      )
    
  }
  
  return(df)
  
}


get_na_type <- function(x) {
  
  if (x == "logical") {
    na_type <- as.logical(NA)
  } else if (x == "integer") {
    na_type <- as.integer(NA)
  } else if (x == "numeric") {
    na_type <- as.numeric(NA)
  } else if (x == "character") {
    na_type <- as.character(NA)
  } else if (x == "factor") {
    na_type <- as.factor(NA)
  }
  
  return(na_type)
  
}