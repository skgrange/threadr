#' Function to get row counts from database tables. 
#' 
#' @author Stuart K. Grange
#' 
#' @param db Database connection
#' @param table Table name in \code{db}. If \code{table} is unknown, all tables
#' in \code{db} will be queried. 
#' @param progress Progress bar. Should a progress bar be shown? Default is 
#' \code{FALSE}.
#' 
#' @export
db_count_rows <- function (db, table = NA, progress = FALSE) {
  
  # If no table is selected, do them all
  if (is.na(table[1])) {
    table <- DBI::dbListTables(db)
  } 
  
  progress <- switch_progress(progress)
  
  # Only some tables
  df <- plyr::ldply(table, counter, db, .progress = progress)
  
  # Return
  df
  
}


# Function to get the row counts
counter <- function (table, db) {
  
  # Create statement
  statement <- stringr::str_c("SELECT COUNT(*) AS row_count 
                               FROM ", table)
  
  # Use statment
  df <- tryCatch(DBI::dbGetQuery(db, statement),
                 error = function(e) data.frame(row_count = NA))
  
  # Add table and order variables
  df <- data.frame(table, row_count = df$row_count)
  
  # Return
  df
  
}


# Function to switch logical progress to a string for plyr
switch_progress <- function (logical, type = "text") {
  
  # Switch logical to type
  if (logical) {
    string <- type
    
  } else {
    string <- "none"
    
  }
  
  # Return
  string
  
}
