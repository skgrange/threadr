#' Function to read HTML tables from an URL. 
#' 
#' @param url A URL which contains HTML tables. 
#' 
#' @param col_names Do the HTML tables have column names/headers? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing tibbles. 
#' 
#' @examples
#' 
#' # A url
#' url <- "https://en.wikipedia.org/wiki/List_of_London_Underground_stations"
#' 
#' # A list
#' list_tables <- read_html_tables(url)
#' length(list_tables)
#' 
#' # Get a single data frame
#' data_stations <- list_tables[[1]]
#' 
#' @export 
read_html_tables <- function(url, col_names = TRUE) {
  
  # Check if url is a url or an html document
  if (stringr::str_detect(url[1], "^http|^https")) {
    
    # Read page
    text <- tryCatch({
      read_lines(url)
    }, error = function(e) {
      # Break and return here
      return(list())
    })
    
  } else {
    # Reassign
    text <- url
  }
  
  if (length(text) != 0) {
    
    # Parse html document
    xml <- XML::htmlTreeParse(
      text, 
      ignoreBlanks = FALSE, 
      useInternalNodes = TRUE, 
      trim = TRUE
    )
    
    # All tables as a list
    list_tables <- XML::readHTMLTable(
      xml, 
      header = col_names, 
      ignoreBlanks = FALSE, 
      trim = TRUE,
      stringsAsFactors = FALSE
    )
    
    if (length(list_tables) != 0) {
      
      # If names are null, give names
      if (unique(names(list_tables))[1] == "NULL") {
        names(list_tables) <- stringr::str_c("table_", 1:length(list_tables))
      }
      
      # Make data frames tibbles
      list_tables <- purrr::modify_if(
        list_tables, 
        is.data.frame, 
        as_tibble, 
        .name_repair = "minimal"
      )
      
      # If a single table, return as data frame
      if (length(list_tables) == 1) list_tables <- list_tables[[1]]
      
    }
    
  } else {
    list_tables <- list()
  }
  
  return(list_tables)
  
}
