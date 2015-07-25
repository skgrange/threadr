#' Function to get the names of database table and produce a data frame with 
#' zero rows. 
#' 
#' \code{get_database_names} is very useful when preparing a data frame for an 
#' SQL insert.
#' 
#' @param db Database connection.
#' @param table Database table.
#' 
#' @seealso \code{\link{dbWriteTable}}, \code{\link{rbind.fill}}
#'
#' @author Stuart K. Grange
#'
#' @export
#'
get_database_names <- function (db, table) {
  
  # Get database table names with no data
  suppressWarnings(
    df <- dbGetQuery(db, stringr::str_c("SELECT * FROM ", table, " LIMIT 0"))
  )
  
  # Return
  df
  
}