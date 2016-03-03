#' Function to get row counts from database tables. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection

#' @param table Table name in \code{con}. If \code{table} is unknown, all tables
#' in \code{con} will be queried. 

#' @param progress Progress bar. Should a progress bar be shown? Default is 
#' \code{FALSE}.
#' 
#' @export
db_count_rows <- function (con, table = NA, progress = FALSE) {
  
  # If no table is selected, do them all
  if (is.na(table[1])) {
    table <- DBI::dbListTables(con)
  } 
  
  progress <- switch_progress(progress)
  
  # Only some tables
  df <- plyr::ldply(table, row_counter, con, .progress = progress)
  
  # Return
  df
  
}


# Function to get the row counts
row_counter <- function (table, con) {
  
  # Create statement
  statement <- stringr::str_c("SELECT COUNT(*) AS row_count 
                               FROM ", table)
  
  # Use statement
  df <- tryCatch(DBI::dbGetQuery(con, statement),
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
