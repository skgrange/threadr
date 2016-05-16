#' Function to get first \emph{n} rows of a database table. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection.
#' @param table Database table name. 
#' @param n Number of rows to read. Default is 5. 
#' 
#' @export
db_head <- function(con, table, n = 5)
  db_get(con, stringr::str_c("SELECT * FROM ", table, " LIMIT ", n))
