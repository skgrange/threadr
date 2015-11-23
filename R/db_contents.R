#' Function to quickly return tables, variables, and values in a database. 
#' 
#' \code{db_contents} will return a tidy data frame with three variables, 
#' \code{"table"}, \code{"variable"}, and \code{"value"}. \code{db_contents} is 
#' useful to explore a database's structure. 
#' 
#' If there are complexities around schemas, PostGIS tables, temporary tables, 
#' or permissions, the table name is returned with a single variable of \code{NA}.
#' 
#' @author Stuart K. Grange
#' 
#' @param db Database connection
#' 
#' @param limit Maximum number rows to return in each table. Default is 1. Use 
#' \code{NA} to read entire table; use with caution. 
#' 
#' @examples 
#' \dontrun{
#' # Only one value will be returned
#' data_single_value <- db_contents(db)
#' 
#' # Entire tables will be returned
#' data_entire_contents <- db_contents(db, limit = NA)
#' }
#'
#' @export
db_contents <- function (db, limit = 1) {
  
  # Get tables
  tables <- DBI::dbListTables(db)
  
  # Apply function
  df <- plyr::ldply(tables, table_reader, db, limit = limit, .progress = "text")
  
  # Reutn
  df
  
}


# The function which does the work
# 
# No export
table_reader <- function (table, db, limit = NA) {
  
  if (is.na(limit)) {
    
    # Read entire table
    suppressWarnings(
      df <- tryCatch(DBI::dbReadTable(db, table),
                     error = function(e) data.frame(table = integer()))
    )
    
  } else {
    
    # Only read n rows/observations
    suppressWarnings(
      df <- tryCatch(DBI::dbGetQuery(db, stringr::str_c("SELECT * FROM ", 
                                               table, 
                                               " LIMIT ", limit)),
                     error = function(e) data.frame(table = integer()))
    )
    
  }
  
  # A catch if the table cannot be accessed, e.g. PostGIS extensions
  if (nrow(df) == 0) {
    df <- data.frame(table = table, 
                     variable = NA, 
                     value = NA)
    
  } else {
    
    # Add table variable
    df$table <- table
    
    # This will keep the table variable in position 1
    suppressWarnings(
      df <- tidyr::gather(df, variable, value, -table)
    )
    
  }
  
  # Return
  df
  
}
