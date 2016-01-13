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
#' @param con Database connection.
#'
#' @param table Table in, or to be created in \code{con}.
#'
#' @param df Data frame to be inserted into \code{con}.
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
db_insert <- function (con, table, df, append = TRUE, overwrite = FALSE,
                       rows = FALSE, fill = FALSE, 
                       increment_reset = FALSE) {
                         
  # Catch dplyr's data table
  df <- base_df(df)
  
  # Reset auto increment
  if (increment_reset) {
    DBI::dbSendQuery(con, stringr::str_c("ALTER TABLE ", table, " AUTO_INCREMENT = 1"))
  }
  
  # Reorder and fill the columns
  if (fill) {
    df <- plyr::rbind.fill(db_table_names(con, table), df)
  }
  
  # Write data frame to database
  # Do not display cat output
  quiet(
    DBI::dbWriteTable(con, table, df, append = append, overwrite = overwrite, 
                      row.names = rows)
  )
  
  # No return
  
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


#' Function to send a statement to a database. 
#' 
#' \code{db_send} is a wrapper for \code{DBI::dbSendQuery}. 
#' 
#' @param con Database connection. 
#' @param statement Statement to send to \code{con}. 
#' 
#' @export
db_send <- function (con, statement) DBI::dbSendQuery(con, statement)


#' Function to get/fetch data from a database with a statement. 
#' 
#' \code{db_get} is a wrapper for \code{DBI::dbGetQuery}. 
#' 
#' @param con Database connection. 
#' @param statement Statement to send to \code{con}. 
#' 
#' @export
db_get <- function (con, statement) DBI::dbGetQuery(con, statement)


#' Function to read an entire database table. 
#' 
#' \code{db_read_table} is a wrapper for \code{DBI::dbReadTable}. 
#' 
#' @param con Database connection. 
#' @param table Table to read
#' 
#' @export
db_read_table <- function (con, table) DBI::dbReadTable(con, table)
