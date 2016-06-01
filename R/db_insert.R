#' Function to insert a data frame into/as a database table.
#'
#' \code{db_insert} is a wrapper for \code{\link{DBI::dbWriteTable}}, but uses 
#' different defaults and has a few enhancements which can be helpful. 
#'
#' \code{db_insert} will not overwrite data or include a "row.names" variable by
#' default. 
#' 
#' @seealso \code{\link{DBI::dbWriteTable}}, \code{\link{db_list_variables}},
#' \code{\link{db_table_names}}
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
db_insert <- function(con, table, df, append = TRUE, overwrite = FALSE,
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
