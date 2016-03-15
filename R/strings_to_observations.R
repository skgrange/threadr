#' Functions to transform a delimited string into observations (rows) in a data
#' frame. 
#' 
#' \code{strings_to_observations} works by first splitting a string variable by a 
#' pattern. The split object is then used to replicate all other variables in a
#' data frame. \code{strings_to_observations} is useful for making data tidy. 
#' 
#' @param df Data frame which contains a delimited string variable. 
#' 
#' @param variable Name of variable in \code{df} which is, or contains delimited 
#' strings. 
#' 
#' @param pattern Delimiter to split the string. Common values are \code{","} and
#' \code{"/"}. 
#' 
#' @param trim Should the new vector be trimmed so there is no leading or 
#' trailing whitespace? Default is \code{FALSE}. 
#' 
#' @param reset Should the \code{row.names} of the data frame be reset after 
#' replicating rows. Default is \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Make tidy data
#' data_tidy <- strings_to_observations(french_verbs, variable = "subject", 
#'                                      pattern = ",")
#' }
#'
#' @export
strings_to_observations <- function (df, variable, pattern, trim = FALSE, 
                                     reset = TRUE) {
  
  # Catch for dplyr's table
  df <- base_df(df)
  
  # Split by pattern into character list
  split_variable <-  strsplit(as.character(df[, variable]), pattern)
  
  # Store lengths of list
  split_variable_length <- sapply(split_variable, length)
  
  # Make variable a vector
  vector_variable <- unlist(split_variable)
  
  # Trim whitespace
  if (trim) vector_variable <- stringr::str_trim(vector_variable)
  
  # Replicate input rows
  df <- df[rep(seq(nrow(df)), split_variable_length), ]
  
  # Overwrite variable
  df[, variable] <- vector_variable
  
  # Drop replications in row.names
  if (reset) row.names(df) <- NULL
  
  # For when there is only one extra variable, it is a matrix
  # if (class(df) == "matrix") df <- data.frame(df)
  
  # Return
  df
  
}
