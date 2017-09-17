#' Function to extract and format an XML attribute string into a key-value pair. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.  
#' 
#' @export
str_extract_xml_attributes <- function(text) {
  
  # Get and clean attribute values
  values <- stringr::str_extract_all(text, '"[^"]*"')[[1]]
  values <- stringr::str_replace_all(values, "\"", "")
  
  # Get and clean attribute names
  variables <- stringr::str_split(text, "=")[[1]]
  variables <- variables[-length(variables)]
  variables <- stringr::str_replace(variables, ".* ", "")
  
  # Build data frame
  df <- data.frame(
    variable = variables, 
    value = values,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}
