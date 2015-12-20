#' Function to catch \code{dplyr}'s \code{tbl_df} data frame class and convert 
#' to a standard/base data frame. 
#' 
#' The different type of data frame in \code{dplyr} can cause failures for some 
#' functions. \code{base_df} degrades a \code{tbl_df} to a base data frame. 
#'
#' @author Stuart K. Grange
#'
#' @param tbl_df \code{tbl_df} to be converted into a standard data frame. 
#'
#' @export
base_df <- function (df) {
  
  # Collapse class vector
  class <- stringr::str_c(class(df), collapse = " ")
  
  # Convert if dplyr tbl
  if (grepl("grouped_df|tbl_df|tbl", class)) {
    df <- data.frame(df)
  }
  
  # Return
  df
   
}
