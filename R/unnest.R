#' Functions to transform a delimited string of nested observations into it's
#' own rows in a data frame. 
#' 
#' \code{unnest} works by first splitting a string variable by a pattern and 
#' then the split object is then used to replicate all other variables in a data
#'  frame. \code{unnest} is useful for making data tidy. 
#' 
#' @param df Data frame which contains a nested delimited string variable. 
#' 
#' @param variable Name of variable in \code{df} which is, or contains delimited 
#' strings. 
#' 
#' @param pattern Delimiter to split the string. Common values are \code{","},
#' \code{"/"}, and \code{";"}. 
#' 
#' @param trim Should the new vector be trimmed so there is no leading or 
#' trailing whitespace? Default is \code{FALSE}. 
#' 
#' @param reset Should the \code{row.names} of the data frame be reset after 
#' replicating rows. Default is \code{TRUE}. 
#' 
#' @param convert Should \code{type.convert} be applied to the unnested variable? 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \link{tidyr}
#' 
#' @examples
#' \dontrun{
#' 
#' # Make tidy data
#' data_tidy <- unnest(french_verbs, variable = "subject", pattern = ",")
#' 
#' }
#'
unnest <- function (df, variable, pattern, trim = FALSE, reset = TRUE,
                    convert = FALSE) {
  
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
  
  # Type conversion
  if (trim) 
    vector_variable <- type.convert(as.character(vector_variable), as.is = TRUE)
  
  # Replicate input rows
  df <- df[rep(seq(nrow(df)), split_variable_length), ]
  
  # Overwrite variable
  df[, variable] <- vector_variable
  
  # Drop replications in row.names
  if (reset) row.names(df) <- NULL
  
  # Return
  df
  
}
