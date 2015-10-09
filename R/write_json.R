#' Function to write a data frame to a JSON file with usage analogous to 
#' \code{write.table}.
#' 
#' \code{write_json} uses \code{jsonlite} as the data frame to JSON parser.
#' 
#' @param df Data frame to be written as a JSON file.
#' @param file JSON file name.
#' @param pretty Should the JSON output be formatted for readability? Default is
#' \code{TRUE}. 
#' @param na Should \code{NA} values be kept in the JSON file? Default is 
#' \code{FALSE}. If \code{na} is \code{TRUE} then \code{NA}s will transformed to
#' JSON's \code{null} value. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' write_json(data_car_identifiers, "car_identifiers.json")
#' 
#' }
#'
#' @export
#' 
write_json <- function (df, file, pretty = TRUE, na = FALSE) {
  
  # Make factors strings so they are not subjected to the classic factor
  # conversion process
  index_factor <- sapply(df, is.factor)
  df[index_factor] <- lapply(df[index_factor], as.character)
  
  # Make JSON object
  if (na) {
    # Keep NAs, but make them null
    json <- jsonlite::toJSON(df, pretty = pretty, na = "null", null = "null")
    
  } else {
    # Drop NAs
    json <- jsonlite::toJSON(df, pretty = pretty)
      
  }
  
  # Write JSON to disk
  write(json, file)
  
}
