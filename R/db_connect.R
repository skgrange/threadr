#' Function to create a database connection with a JSON configuration file. 
#' 
#' \code{db_connect} uses a \code{JSON} configuration file to create a database
#' connection. This configuration file will often exist outside a code package 
#' so database credentials are not accidentally transmitted or shared. 
#' 
#' If only one entry is in the \code{JSON} file, the \code{database} argument is
#' not needed.
#'
#' MySQL, PostgreSQL, and SQLite connections are currently supported. 
#' 
#' @param file \code{JSON} file or string containing database connection 
#' details. For SQLite databases, use the database's file path. 
#' 
#' @param database The name of the database within \code{file} to use to create
#' a database connection to. If only one entry is in \code{file}, this argument 
#' is not needed and will be ignored if used. 
#' 
#' @param config A logical to skip the \code{JSON} file configuration and just
#' attempt to connect to \code{file} directly. This is used for \code{SQLite}
#' databases which require no configuration. 
#' 
#' @param foreign_keys A logical for SQLite databases where foreign keys should 
#' be enforced. Default is \code{TRUE}. For other database types, this will be 
#' ignored. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Connect to an air quality database
#' db <- db_connect("connections.json", "air_quality")
#' 
#' 
#' # Use a json config file which looks similar to this:
#' string <- '
#' {
#'   "driver": "MySQL",
#'   "host": "172.31.4.159",
#'   "database_name": "database_seven",
#'   "user": "web",
#'   "password": "read_password"
#' }'
#' 
#' # Connect, no need for second argument when one connection is present in 
#' # configuration file
#' db_seven <- db_connect(string)
#' 
#' 
#' # A SQLite connection needs no configuration
#' con <- db_connect("../databases/air_quality_data.sqlite", config = FALSE)
#' 
#' }
#' 
#' @export
db_connect <- function(file, database, config = TRUE, foreign_keys = TRUE) {
  
  if (config) {
    
    # Load configuration file
    json <- jsonlite::fromJSON(file)
    
    # If json file has many database connection details, filter with argument
    if (class(json) == "data.frame")
      json <- json[json[, "database_name"] == database, ]
    
    # Create connection based on driver type
    if (grepl("mysql", json$driver, ignore.case = TRUE)) {
      
      con <- DBI::dbConnect(RMySQL::MySQL(), 
                            host = json$host, 
                            dbname = json$database_name,
                            user = json$user, 
                            password = json$password)
      
    }
    
    if (grepl("postg", json$driver, ignore.case = TRUE)) {
      
      con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                            host = json$host, 
                            dbname = json$database_name,
                            user = json$user, 
                            password = json$password)
      
    }
    
  } else {
    
    # sqlite databases, only need a path
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    
    # Add support for foreign keys
    if (foreign_keys) db_send(con, "PRAGMA foreign_keys = 1")
    
  }
  
  # Return
  con
  
}


#' @export
db_disconnect <- function(con) quiet(DBI::dbDisconnect(con))
