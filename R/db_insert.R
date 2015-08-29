#' Function to insert a data frame into/as a database table.
#'
#' \code{db_insert} is a wrapper for \code{dbWriteTable} but uses different 
#' defaults and has a few enhancements which can be helpful. 
#' 
#' @author Stuart K. Grange
#' 
#' @param db A database connection.
#' @param table A table in or to be created in \code{db}.
#' @param df Data frame to be inserted into \code{db}.
#' @param append Should the table be appended? Default is TRUE. 
#' @param overwrite Should the table be overwrited? Default is FALSE. 
#' @param rows Should the inserted data include row names? Default is FALSE. 
#' @param fill Should \code{df} be forced to have the same columns and order as
#' \code{table}? 
#' @param increment_reset Should the auto-increment column in \code{table} be
#' reset before insert? Default is FALSE. 
#' 
#' @export
#'
db_insert <- function (db, table, df, append = TRUE, overwrite = FALSE,
                         rows = FALSE, fill = FALSE, 
                         increment_reset = FALSE) {
                         
  # No :: because many connections could be used
  
  # Reset auto increment
  if (increment_reset) {
    dbSendQuery(db, stringr::str_c("ALTER TABLE ", table, " AUTO_INCREMENT = 1"))
  }
  
  # Pad the columns
  if (fill) {
    df <- plyr::rbind.fill(get_database_names(db, table), df)
  }
  
  # Write data frame to database
  # Do not display cat output
  quiet(
    dbWriteTable(db, table, df, append = append, overwrite = overwrite, 
                 row.names = rows)
  )
  
}


# http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function (x) {
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 