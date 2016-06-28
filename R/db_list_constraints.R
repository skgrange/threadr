#' Function to list table constraints for a database. 
#' 
#' \code{db_list_constraints} only supports PostgreSQL databases at present. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_constraints <- function(con) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE))
    df <- db_get(con, "SELECT * FROM information_schema.constraint_column_usage")
    
  # Return
  df
  
}
