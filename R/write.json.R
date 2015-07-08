#' Function to write a data frame to a JSON file with usage analogous to 
#' \code{write.table} and its derivatives.
#' 
#' \code{write.json} Uses \code{jsonlite} as the data frame to JSON parser.
#' 
#' @param df Data frame to be written as a JSON file.
#' @param file JSON file name
#' @param pretty Should the JSON output be formatted for readability? Default is
#' TRUE. 
#' @param na.empty Should empty strings be converted to NA values to they are 
#' not included in the the JSON element? 
#'   
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' write.json(data.nelson, "nelson_data_as_json.json")
#' }
#'
#' @export
#' 
write.json <- function (df, file = "", pretty = TRUE, na.empty = TRUE) {
  
  # Replace empty strings with NA so the variable will not be written to file
  if (na.empty) {
    
    # Catch dates
    index.date <- sapply(df, is.POSIXt)
    df[index.date] <- lapply(df[index.date], as.character)

    # Replace empty strings
    df <- data.frame(lapply(df, function (x) ifelse(x == "", NA, x)))
    
  }
  
  # Make JSON object
  json <- jsonlite::toJSON(df, pretty = pretty)
  
  # Write JSON to disk
  write(json, file)
  
}
