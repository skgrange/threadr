#' Function to read HTML tables from an URL. 
#' 
#' @param url A URL which contains HTML tables. 
#' 
#' @param n Table-number to return. Optional, but this will return a single
#' data frame rather than a list of data frames which is usually what is 
#' desired. 
#' 
#' @param character Should the content of the url be downloaded as a character
#' string for processing? Default is \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing data frames or a data frame. 
#' 
#' @examples 
#' \dontrun{
#' # A url
#' url <- "https://en.wikipedia.org/wiki/List_of_London_Underground_stations"
#' 
#' # A list
#' list_tables <- read_html_tables(url)
#' length(list_tables)
#' 
#' # A data frame
#' df <- read_html_tables(url, n = 1)
#' 
#' }
#' 
#' @export 
read_html_tables <- function(url, n = NA, character = TRUE) {
  
  # Get text/characters, helps with https and some proxy servers
  if (character) url <- readLines(url, warn = FALSE)
    
  # Parse html
  document <- XML::htmlTreeParse(url, ignoreBlanks = FALSE, 
    useInternalNodes = TRUE, trim = FALSE)
  
  if (is.na(n)) {
  
    # All tables as a list
    table <- XML::readHTMLTable(document, ignoreBlanks = FALSE, trim = FALSE,
                                stringsAsFactors = FALSE)
    
    # If names are null, give names
    if (unique(names(table))[1] == "NULL")
      names(table) <- stringr::str_c("table_", 1:length(table))
    
    # If a single table, return as data frame
    if (length(table) == 1) table <- table[[1]]
    
  } else {
  
    # Get table nodes
    nodes <- XML::getNodeSet(document, "//table")
    
    # Parse a specific table
    table <- XML::readHTMLTable(nodes[[n]], stringsAsFactors = FALSE)
    
  }
  
  # Return a list or a data frame
  table
  
}
