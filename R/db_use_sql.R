#' Function to load SQL script and send it to a database. 
#' 
#' @param con Database connection. 
#' @param file File name of SQL script. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_use_sql <- function(con, file) {
  
  # Load script
  sql <- read_sql(file)
  
  # Use statements
  plyr::l_ply(sql, function(x) db_send(con, x))
  
  # No return
  
}
