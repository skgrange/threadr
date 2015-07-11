#' Function to write an entire database table to disk. 
#' 
#' \code{write_database_table} breaks up the queries and exports to small pieces 
#' to allow for very large tables to be written to disc with systems with limited
#' amounts of physical memory. The output is a \code{.csv} file.
#' 
#' @param db A database connection. 
#' @param table \code{db}'s table which is to be written to disk. 
#' @param nrow Number of rows of the \code{db}'s table to query and then write
#' to disk in each cycle. Default is 1000000 rows. 
#' @param file File to export the table to. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
#' 
write_database_table <- function (db, table, nrow = 1000000, file = "") {
  
  # Get size of table
  message("Finding the length of the table...")
  count.statement <- paste("SELECT COUNT(*) FROM", table)
  length <- dbGetQuery(db, count.statement)
  length <- length[, 1]
  message(paste("This table is", length, "rows long..."))
  
  # Ceiling length to nrow multiple
  length.ceiling <- nrow * ceiling(length / nrow)
  
  # Generate sequence
  sequence <- seq(0, length.ceiling, by = nrow)
  
  # Build sql select statements
  statement.select <- paste("SELECT *",
                            "FROM", table, 
                            "LIMIT", nrow, 
                            "OFFSET", sequence)
  
  # Select and write using the many select statements
  message(paste("Selecting data and writing to disk in", 
                length(statement.select), "pieces..."))
                
  # Apply functions
  l_ply(statement.select, select_and_write, db, file = file, .progress = "text")
  
}


# Function for selecting then exporting file
select_and_write <- function (statement, db, file) {
  
  # Get data from database
  df <- dbGetQuery(db, statement)
  
  # Export data
  if (file.exists(file)) {
    
    write.table(df, file, row.names = FALSE, col.names = FALSE, append = TRUE, 
                sep = ",")
    
  } else {
    
    write.csv(df, file, row.names = FALSE)
    
  }
  
}
