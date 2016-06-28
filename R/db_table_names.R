#' Function to get the names of database table and produce a data frame with 
#' zero rows. 
#' 
#' \code{db_table_names} is useful when preparing a data frame for a SQL insert.
#' If a database table is empty, this function can fail on some databases. To-do:
#' catch this. 
#' 
#' @param con Database connection.
#' 
#' @param table Database table.
#' 
#' @seealso \code{\link{dbWriteTable}}, \code{\link{rbind.fill}}
#'
#' @author Stuart K. Grange
#' 
#' @return Data frame with no observations, only headers. 
#'
#' @export
db_table_names <- function(con, table) {
  
  # Get database table names with one observation
  # Returning data too because some database connections return nothing when
  # LIMIT = 0
  suppressWarnings(
    df <- db_get(con, stringr::str_c("SELECT * FROM ", table, " LIMIT 1"))
  )
  
  # Postgres returns 0 0 data frame
  if (nrow(df) == 0 & ncol(df) == 0) {
    
    # Get names
    names <- db_list_variables(con, table)
    
    # Create a data frame
    suppressWarnings(
      df <- read.csv("", col.names = names)
    )
    
  }
  
  # Remove data if present
  df <- df[-1, ]
  
  # Return
  df
  
}
