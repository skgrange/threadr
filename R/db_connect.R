#' Function to create a database connection with a JSON configuration file. 
#' 
#' \code{db_connect} uses a \code{JSON} configuration file to create a database
#' connection. This configuration file will generally exist outside a code 
#' package so database credentials are not accidentally transmitted or shared. 
#' 
#' If only one entry is in the \code{JSON} file, the \code{database} argument is
#' not needed.
#'
#' MySQL and PostgreSQL connections are currently supported. 
#' 
#' @param file \code{JSON} file or string containing database connection 
#' details. 
#' 
#' @param database The name of the database within \code{file} to use to create
#' a database connection to. If only one entry is in \code{file}, this argument 
#' is not needed and will be ignored if used. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Connect to an air quality database
#' db <- db_connect("connections.json", "air_quality")
#' 
#' 
#' # Use a json confg file which looks similar to this:
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
#' }
#' 
#' @export
db_connect <- function (file, database) {
  
  # Load configuration file
  json <- jsonlite::fromJSON(file)
  
  # If json file has many database connection details, filter with argument
  if (class(json) == "data.frame") {
    json <- json[json[, "database_name"] == database, ]
  } 
  
  # Create connection based on driver type
  if (json$driver == "MySQL") {
    
    con <- DBI::dbConnect(RMySQL::MySQL(), 
                          host = json$host, 
                          dbname = json$database_name,
                          user = json$user, 
                          password = json$password)
    
  }
  
  if (json$driver == "PostgreSQL") {
    
    con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                          host = json$host, 
                          dbname = json$database_name,
                          user = json$user, 
                          password = json$password)
    
  }
  
  # Return
  con
  
}
