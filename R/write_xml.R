#' Function to write data frame to XML document. 
#' 
#' @export
write_xml <- function(data, file) {
  
  # https://cran.r-project.org/web/packages/kulife/
  
  # Start empty XML document tree
  doc <- XML::newXMLDoc()          
  # Start by adding a document tag at the root of the XML file
  root <- XML::newXMLNode("document", doc=doc)
  
  # Make output invisible
  invisible(
    # Iterate over all rows
    lapply(1:nrow(data),                 
           function(rowi) {
             r <- XML::newXMLNode("row", parent = root)   # Create row tag
             for(var in names(data)) {   # Iterate over variables
               XML::newXMLNode(var, data[rowi, var], parent = r)
             }
           }))            
  invisible(XML::saveXML(doc, file = file))
}
