#' Functions to vacuum, analyse, and optimise database tables. 
#' 
#' The PostgreSQL statement uses \code{VACUUM (VERBOSE)}, the MySQL statement
#' uses \code{OPTIMIZE}, and the SQLite statement uses \code{VACUUM} and the 
#' \code{table} argument is not used. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' @param table Database table. 
#' 
#' @examples 
#' \dontrun{
#' db_vacuum_analyse(con, "hysplit_data")
#' db_optimise(con, "malta_data")
#' }
#' 
#' @export
db_vacuum <- function (con, table) {
  
  # PostgreSQL databases
  if (grepl("postgresql", class(con), ignore.case = TRUE))
    quiet(db_send(con, stringr::str_c("VACUUM (VERBOSE) ", table)))
    
  # MySQL databases
  if (grepl("mysql", class(con), ignore.case = TRUE)) {
    
    # Catch the reserved verbs
    table <- stringr::str_c("`", table, "`")
    
    quiet(db_send(con, stringr::str_c("OPTIMIZE TABLE ", table)))
    
    # Clear results
    quiet(DBI::dbClearResult(DBI::dbListResults(con)[[1]]))
    
  }
  
  # SQLite
  if (grepl("sqlite", class(con), ignore.case = TRUE))
    quiet(dbSendQuery(con, "VACUUM"))
  
  # No return
  
}


# Use MySQL notation
#' @rdname db_vacuum
#' @export
db_optimise <- db_vacuum
