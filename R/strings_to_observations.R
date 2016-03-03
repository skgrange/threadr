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
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Make tidy data
#' data_tidy <- string_to_observations(french_verbs, variable = "subject", 
#'                                     pattern = ",")
#' }
#'
#' @export
strings_to_observations <- function (df, variable, pattern) {
  
  # Catch for dplyr's table
  df <- base_df(df)
  
  # Store string for renaming after transformation
  variable_index <- grep(variable, names(df))
  variable_string <- variable
  
  # Store extra variables
  df_extra <- df
  df_extra[, variable] <- NULL
  
  # Split by pattern into character list
  split_variable <-  strsplit(as.character(df[, variable]), pattern)
  
  # Store lengths of list
  split_variable_length <- sapply(split_variable, length)
  
  # Make variable vector
  variable <- unlist(split_variable)
  
  # Replicate stored data
  df_extra <- df_extra[rep(seq(nrow(df_extra)), split_variable_length), ]
  
  # Drop odd replications in row.names
  row.names(df_extra) <- NULL
  
  # Bind things together
  df <- cbind(variable, df_extra)
  
  # For when there is only one extra variable, it is a matrix
  if (class(df) == "matrix") df <- data.frame(df)
  
  # Rename variable to input
  names(df) <- stringr::str_replace(names(df), "variable", variable_string)
  
  # Return
  df
  
}
