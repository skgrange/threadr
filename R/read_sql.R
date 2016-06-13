#' Function to read SQL script and store statments as a vector. 
#' 
#' Mutli-line commenting is not correctly parsed yet.
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
read_sql <- function(file) {
  
  # Load file
  sql <- readLines(file, warn = FALSE)
  
  # Drop comments
  sql <- grep("--", sql, invert = TRUE, value = TRUE)
  
  # Clean
  sql <- stringr::str_c(sql, collapse = "")
  sql <- str_trim_many_spaces(sql)
  
  # Split based on ;
  sql <- unlist(stringr::str_split(sql, ";"))
  
  # Drop empty statements
  sql <- sql[!ifelse(sql == "", TRUE, FALSE)]
  
  # Return
  sql
  
}
