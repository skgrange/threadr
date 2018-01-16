#' Function to write an R object to a JSON file.
#' 
#' \code{write_json} uses \code{jsonlite} as the data frame to JSON parser.
#' 
#' @param x R object to be written as a JSON file.
#' 
#' @param file JSON file name.
#' 
#' @param pretty Should the JSON output be formatted for readability? Default is
#' \code{TRUE}. 
#' 
#' @param na Should \code{NA} values be kept in the JSON file? Default is 
#' \code{FALSE}. If \code{na} is \code{TRUE} then \code{NA}s will transformed to
#' JSON's \code{null} value. 
#' 
#' @param auto_unbox Should vectors with the length of \code{1} be represented 
#' as atomic, single element arrays? Default is \code{FALSE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Export a data frame
#' write_json(data_car_identifiers, "car_identifiers.json")
#' 
#' }
#'
#' @export
write_json <- function(x, file, pretty = TRUE, na = FALSE, auto_unbox = FALSE) {
  
  # Factor vector
  if ("factor" %in% class(x)) x <- as.character(x)
  
  # Factors within data frame
  if (any(grepl("data.frame", class(x)))) {
    
    # Make factors strings
    index_factor <- sapply(x, is.factor)
    x[index_factor] <- lapply(x[index_factor], as.character)
    
  }
  
  # Make JSON object
  if (na) {
    
    # Keep NAs, but make them null
    json <- jsonlite::toJSON(
      x, 
      pretty = pretty, 
      na = "null", 
      null = "null",
      auto_unbox = auto_unbox
    )
    
  } else {
    
    # Drop NAs
    json <- jsonlite::toJSON(x, pretty = pretty, auto_unbox = auto_unbox)
      
  }
  
  # Write JSON to disk
  write(json, file)
  
}
