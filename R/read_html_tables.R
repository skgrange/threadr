#' Function to read HTML tables from an URL. 
#' 
#' @param url A URL which contains HTML tables. 
#' 
#' @param n Table-number to return. Optional, but this will return a single
#' data frame rather than a list of data frames which is usually what is 
#' desired. 
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
read_html_tables <- function (url, n = NA) {
  
  # Get text, works with https and with many proxy servers
  text <- readLines(url, warn = FALSE)
  
  # Parse html
  document <- XML::htmlTreeParse(text, ignoreBlanks = FALSE, 
                                 useInternalNodes = TRUE, trim = FALSE)
  
  if (is.na(n)) {
    # All tables as a list
    table <- XML::readHTMLTable(document, ignoreBlanks = FALSE, trim = FALSE)
    
  } else {
    # Get table nodes
    nodes <- XML::getNodeSet(document, "//table")
    
    # Parse a specific table
    table <- XML::readHTMLTable(nodes[[n]])
    
  }
  
  # Return a list or a data frame
  table
  
}
