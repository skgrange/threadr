#' Function to load SQL script and send it to a database. 
#' 
#' @param con Database connection. 
#' @param file File name of SQL script. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_use_sql <- function(con, file) {
  
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
  
  # Use statements
  plyr::l_ply(sql, function(x) db_send(con, x))
  
  # No return
  
}
