#' Function to apply \code{grepl} across many variables in a data frame. 
#' 
#' \code{grepl_all} is useful for searching an entire data frame for a pattern
#' and can be used for filtering. 
#' 
#' @param df Data frame. 
#' @param pattern Pattern to match with \code{\link{grepl}}. 
#' @param ignore.case Use case sensitive matching? Default is \code{FALSE}. 
#' 
#' @return Logical vector with the length of \code{ncol(df)}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
grepl_all <- function(df, pattern, ignore.case = FALSE) {
  
  # Apply grepl to all variables
  # Find the variables
  index <- sapply(df, function(x) 
    ifelse(is.character(x) | is.factor(x), TRUE, FALSE))
  
  # Use grepl
  logical_group <- lapply(df[index], function(x) 
    grepl(pattern, x, ignore.case = ignore.case))
  
  # Make two dimensional object
  logical_group <- do.call("rbind", logical_group)
  
  # To vector
  logical_vector <- apply(logical_group, 2, function(x) any(x))
  
  # Return
  logical_vector
  
}
