#' Function to list indices for a database tables. 
#' 
#' Only SQLite databases are currently supported. 
#' 
#' @param con Database connection. 
#' @param table Table name. If unused, all tables will be queried.  
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_indices <- function(con, table = NA) {
  
  # Get tables
  tables <- db_list_tables(con)
  
  # SQLite databases
  if (grepl("sqlite", class(con), ignore.case = TRUE))
    df <- plyr::ldply(tables, function(x) index_query_er(con, x))
  
  # Return
  df
  
}


# Only for SQLite at the moment
# No export
index_query_er <- function(con, table) {
  
  # Build statement
  sql <- stringr::str_c("PRAGMA INDEX_LIST('", table, "')")
  
  # Query
  df <- db_get(con, sql)
  
  # Return
  df
  
}
