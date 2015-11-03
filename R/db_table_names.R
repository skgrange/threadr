#' Function to get the names of database table and produce a data frame with 
#' zero rows. 
#' 
#' \code{db_table_names} is useful when preparing a data frame for a SQL insert.
#' If a database table is empty, this function can fail on some databases. To-do:
#' catch this. 
#' 
#' @param db Database connection.
#' @param table Database table.
#' 
#' @seealso \code{\link{dbWriteTable}}, \code{\link{rbind.fill}}
#'
#' @author Stuart K. Grange
#'
#' @export
db_table_names <- function (db, table) {
  
  # Get database table names with one observation
  # Returning data too because some database connections return nothing when
  # LIMIT = 0
  suppressWarnings(
    df <- dbGetQuery(db, stringr::str_c("SELECT * FROM ", table, " LIMIT 1"))
  )
  
  # Remove data
  df <- df[-1, ]
  
  # Return
  df
  
}
