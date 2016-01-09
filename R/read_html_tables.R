#' Function to read HTML tables from an URL. 
#' 
#' @param url A URL which contains HTML tables. 
#' 
#' @param n Table-number to return. Optional, but this will return a single
#' data frame rather than a list of data frames which is usually what is 
#' desired. 
#' 
#' @param character Is \code{url} a character string? This is useful when the 
#' HTML has been read and stored previously before being used by this function. 
#' 
#' @author Stuart K. Grange
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
read_html_tables <- function (url, n = NA, character = FALSE) {
  
  # Get text/characters, works with https and with many proxy servers
  if (character) {
    url <- readLines(url, warn = FALSE)
  }
  
  # Parse html
  document <- XML::htmlTreeParse(url, ignoreBlanks = FALSE, 
                                 useInternalNodes = TRUE, trim = FALSE)
  
  if (is.na(n)) {
    # All tables as a list
    table <- XML::readHTMLTable(document, ignoreBlanks = FALSE, trim = FALSE,
                                stringsAsFactors = FALSE)
    
  } else {
    # Get table nodes
    nodes <- XML::getNodeSet(document, "//table")
    
    # Parse a specific table
    table <- XML::readHTMLTable(nodes[[n]], stringsAsFactors = FALSE)
    
  }
  
  # Return a list or a data frame
  table
  
}
