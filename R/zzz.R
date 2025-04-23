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
    "x", "y", "sorted_variables", "hour", "lower", "upper", "outlier",
    "sequential_deviation", "value_reference", "rowid", "x_id", "y_id",
    "x_interval", "y_interval", "interval", "date_package", "observations",
    "n", "event", "date_in_time_zone", "date_end_in_time_zone", "path", 
    "type", "size", "modification_time", "week_monday_year", "week_monday_number"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}


# Set an environment variable
.datatable.aware <- TRUE
