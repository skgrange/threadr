#' Function to insert a data frame into/as a database table.
#'
#' \code{db_insert} is a wrapper for \code{DBI::dbWriteTable} but uses different 
#' defaults and has a few enhancements which can be helpful. 
#'
#' \code{db_insert} will not overwrite data or include a "row.names" variable by
#' default. 
#' 
#' @author Stuart K. Grange
#' 
#' @param db Database connection.
#'
#' @param table Table in, or to be created in \code{db}.
#'
#' @param df Data frame to be inserted into \code{db}.
#'
#' @param append Should the table be appended? Default is \code{TRUE}. 
#'
#' @param overwrite Should the table be overwritten? Default is \code{FALSE}. 
#'
#' @param rows Should the inserted data include row names? Default is 
#' \code{FALSE}. 
#'
#' @param fill Should \code{df} be forced to have the same columns and order as
#' \code{table}? 
#'
#' @param increment_reset Should the auto-increment column in \code{table} be
#' reset before insert? Default is \code{FALSE}. 
#' 
#' @export
db_insert <- function (db, table, df, append = TRUE, overwrite = FALSE,
                       rows = FALSE, fill = FALSE, 
                       increment_reset = FALSE) {
                         
  # Catch dplyr's data table
  # df <- base_df(df)
  
  # Reset auto increment
  if (increment_reset) {
    DBI::dbSendQuery(db, stringr::str_c("ALTER TABLE ", table, " AUTO_INCREMENT = 1"))
  }
  
  # Reorder and fill the columns
  if (fill) {
    df <- plyr::rbind.fill(get_database_names(db, table), df)
  }
  
  # Write data frame to database
  # Do not display cat output
  quiet(
    DBI::dbWriteTable(db, table, df, append = append, overwrite = overwrite, 
                      row.names = rows)
  )
  
}


# http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
# Function to catch \code{cat} messages and make them invisible. 
# 
#' @export
quiet <- function (x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
