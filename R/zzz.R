#' Function to squash R check's global variable notes. 
#' 
#' @name zzz
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "variable", "value", "date_end", "sd", "median", "tail", "head", 
    "str_replace_all", "file_basename", "weekday", "saturday_logical", 
    "row_number", "date_ahead", "date_type", "descriptive", "year", "month", 
    "matches", "monday_logical", "confidence", "z_score", "name", "unique_name",
    "gas", "r", "Winsorize", "InDots", "boot", "boot.ci", "value_predict", 
    "x", "y", "sorted_variables", "hour", "lower", "upper"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
