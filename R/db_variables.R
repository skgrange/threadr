#' Function to return all variable names in all tables in a SQL database. 
#' 
#' If there are complexities around schemas, PostGIS tables, or temporary tables, 
#' the table name is returned with a single variable of \code{NA}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @examples
#' \dontrun{
#' 
#' table_variables <- db_variables(con)
#' 
#' }
#'
#' @export
db_variables <- function(con) {
  
  # Get table names
  tables <- db_list_variables(con)
  
  # Get all table's names
  df <- plyr::ldply(tables, get_names, con)
  
  # Arrange
  df <- df[order(df$table), ]
  
  # Return
  df
  
}


# Function to get table and variable names from database table
# 
# No export
get_names <- function (table, con) {
  
  # Get vector of variables from database table
  variables <- tryCatch({
    
    db_list_variables(con, table) 
    
  }, error = function(e) {
    
    NA
    
  })
  
  # Make data frame
  df <- data.frame(table = table, 
                   variable = variables)
  
  # Return
  df
  
}


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
#' @param con Database connection. 
#' 
#' @param limit Maximum number rows to return in each table. Default is 1. Use 
#' \code{NA} to read entire table; use with caution. 
#' 
#' @examples 
#' \dontrun{
#' # Only one value will be returned
#' data_single_value <- db_contents(con)
#' 
#' # Entire tables will be returned
#' data_entire_contents <- db_contents(con, limit = NA)
#' }
#'
#' @export
db_contents <- function(con, limit = 1) {
  
  # Get tables
  tables <- db_list_tables(con)
  
  # Apply function
  df <- plyr::ldply(tables, table_reader, con, limit = limit, .progress = "text")
  
  # Return
  df
  
}


# The function which does the work
# 
# No export
table_reader <- function(table, con, limit = NA) {
  
  if (is.na(limit)) {
    
    # Read entire table
    suppressWarnings(
      df <- tryCatch(DBI::dbReadTable(con, table),
                     error = function(e) data.frame(table = integer()))
    )
    
  } else {
    
    # Only read n rows/observations
    suppressWarnings(
      df <- tryCatch(DBI::dbGetQuery(con, stringr::str_c("SELECT * FROM ", 
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
    
    # This will keep the table-variable in position 1
    suppressWarnings(
      df <- tidyr::gather(df, variable, value, -table)
    )
    
  }
  
  # Return
  df
  
}


#' Function to get first \emph{n} rows of a database table. 
#' 
#' @export
db_head <- function(con, table, n = 5)
  db_get(con, stringr::str_c("SELECT * FROM ", table, " LIMIT ", n))
