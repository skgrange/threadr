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
db_optimise_table <- function (db, table, clear = TRUE) {
  
  # Catch the reserved verbs
  table <- stringr::str_c("`", table, "`")
  
  # Build statement
  statement <- stringr::str_c("OPTIMIZE TABLE ", table)
  
  # Use statement
  dbSendQuery(db, statement)
  
  # Clear results
  if (clear) {
    dbClearResult(dbListResults(db)[[1]])
  }
  
  # No return
  
}


#' @rdname db_optimise_table
#' @export
db_optimise_all_tables <- function (db, progress = "text") {
  
  # Get a vector of all tables in database
  tables <- dbListTables(db)
  
  # Optimise all tables
  plyr::l_ply(tables, db_optimise_table, db = db, .progress = progress)
  
  # No return
  
}
