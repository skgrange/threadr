#' Function to write a data frame to an XML document. 
#' 
#' \code{write_xml} is not suitable for large data frames.
#' 
#' @import XML
write_xml <- function (df, file, name = "observation") {
  
  # Create an xml object
  xml <- data_frame_to_xml(df, name)
  
  # Make a text string
  text <- saveXML(xml)
  
  # Write text
  write(text, file)
  
  # No return
  
}


# 
data_frame_to_xml <- function (df, name = "observation") {
  
  # Create xml object
  xml <- xmlTree()
  
  suppressWarnings(
    xml$addTag("document", close = TRUE)
  )
  
  # 
  for (i in 1:nrow(df)) {
    
    xml$addTag("observation", close = FALSE)
    
    for (j in names(df)) xml$addTag(j, df[i, j])
        
    xml$closeTag()
    
  }
  
  xml$closeTag()
  
  # Return 
  xml
  
}

