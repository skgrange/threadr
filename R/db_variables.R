#' Function to return all variable names in all tables in a SQL database. 
#' 
#' If there are complexities around schemas, PostGIS tables, or temporary tables, 
#' the table name is returned with a single variable of \code{NA}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param db Database connection. 
#' 
#' @examples
#' \dontrun{
#' table_variables <- db_variables(db)
#' 
#' }
#'
#' @export
db_variables <- function (db) {
  
  # Get table names
  tables <- DBI::dbListTables(db)
  
  # Get all table's names
  df <- plyr::ldply(tables, get_names, db)
  
  # Arrange
  df <- df[order(df$table), ]
  
  # Return
  df
  
}


# Function to get table and variable names from database table
# 
# No export
# 
get_names <- function (table, db) {
  
  # Get vector of variables from database table
  variables <- tryCatch(DBI::dbListFields(db, table), 
                        error = function(e) NA)
  
  # Make data frame
  df <- data.frame(table = table, variable = variables)
  
  # Return
  df
  
}
