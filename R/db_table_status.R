#' Function to get a table's status from a MySQL database. 
#' 
#' \code{db_table_status} queries a MySQL database for a table's status. 
#' See \url{https://dev.mysql.com/doc/refman/5.1/en/show-table-status.html} for 
#' the variables which are returned. 
#'
#' @param db MySQL database connection. 
#' @param table Database table
#' 
#' @author Stuart K. Grange
#' 
#' @export 
#' 
db_table_status <- function (db, table) {
  
  # Build statement
  statement <- stringr::str_c("SHOW TABLE STATUS WHERE `name` = '", table, "'")
  
  # Get status
  info.status <- dbGetQuery(db, statement)
   
  # Return
  info.status
  
}
