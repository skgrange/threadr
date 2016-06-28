#' Function to list activities/processes for a database. 
#' 
#' \code{db_list_activities} only supports PostgreSQL databases at present. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_activities <- function(con) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE)) {
    
    df <- suppressWarnings(
      db_get(con, "SELECT * FROM pg_stat_activity")
    )
    
  }
  
  # Return
  df
  
}
