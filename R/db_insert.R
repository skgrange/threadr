#' Function to insert a data frame into/as a database table.
#'
#' \code{db_insert} is a wrapper for \code{DBI::dbWriteTable}, but uses different 
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
#' @export
db_insert <- function (con, table, df, append = TRUE, overwrite = FALSE,
                       rows = FALSE, fill = FALSE) {
                         
  # Catch dplyr's data table
  df <- base_df(df)
  
  # Reorder and fill the columns
  if (fill) df <- plyr::rbind.fill(db_table_names(con, table), df)
  
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
#' @param table Table to read. 
#' 
#' @export
db_read_table <- function (con, table) DBI::dbReadTable(con, table)


#' Function to list all variables/columns/fields in a database table. 
#' 
#' \code{db_list_variables} is a wrapper for \code{DBI::dbListFields}. 
#' 
#' @param con Database connection. 
#' @param table Table to list variables/columns/fields. 
#' 
#' @export
db_list_variables <- function (con, table) DBI::dbListFields(con, table)





#' Function to get the names of database table and produce a data frame with 
#' zero rows. 
#' 
#' \code{db_table_names} is useful when preparing a data frame for a SQL insert.
#' If a database table is empty, this function can fail on some databases. To-do:
#' catch this. 
#' 
#' @param con Database connection.
#' 
#' @param table Database table.
#' 
#' @seealso \code{\link{dbWriteTable}}, \code{\link{rbind.fill}}
#'
#' @author Stuart K. Grange
#'
#' @export
db_table_names <- function (con, table) {
  
  # Get database table names with one observation
  # Returning data too because some database connections return nothing when
  # LIMIT = 0
  suppressWarnings(
    df <- dbGetQuery(con, stringr::str_c("SELECT * FROM ", table, " LIMIT 1"))
  )
  
  # Remove data
  df <- df[-1, ]
  
  # Return
  df
  
}

