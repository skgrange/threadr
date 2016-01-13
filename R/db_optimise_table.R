#' Functions to optimise MySQL tables. 
#' 
#' \code{db_optimise_table} uses MySQL's \code{OPTIMIZE TABLE} function to 
#' analyse, vacuum, and optimise a table. \code{db_optimise_all_tables} is 
#' a vectorised version which will use the same \code{OPTIMIZE TABLE} function
#' and apply it to all tables in a database. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_optimise_table <- function (con, table, clear = TRUE) {
  
  # Catch the reserved verbs
  table <- stringr::str_c("`", table, "`")
  
  # Build statement
  statement <- stringr::str_c("OPTIMIZE TABLE ", table)
  
  # Use statement
  DBI::dbSendQuery(con, statement)
  
  # Clear results
  if (clear) {
    DBI::dbClearResult(dbListResults(con)[[1]])
  }
  
  # No return
  
}


#' @rdname db_optimise_table
#' @export
db_optimise_all_tables <- function (con, progress = "text") {
  
  # Get a vector of all tables in database
  tables <- DBI::dbListTables(con)
  
  # Optimise all tables
  plyr::l_ply(tables, db_optimise_table, con = con, .progress = progress)
  
  # No return
  
}
