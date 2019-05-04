#' Function to parse and switch common file paths for different systems. 
#' 
#' @param path Path or file to be parsed and switched. 
#' 
#' @author Stuart K. Grange
#' 
#' @return \code{fs_path} vector. 
#' 
#' @export
parse_personal_path <- function(path) {
  
  # Work system
  if (Sys.info()["nodename"] == "DDM06364") {
    
    if (stringr::str_detect(path, "^~/Dropbox")) {
      path <- stringr::str_replace(path, "~/Dropbox", "C:/Users/grst/Dropbox")
    } else if (stringr::str_detect(path, "^/media/stuart/ELEMENTS_II")) {
      path <- stringr::str_replace(path, "/media/stuart/ELEMENTS_II", "T:")
    }
    
    # My systems
  } else if (Sys.info()["sysname"] == "Linux" && Sys.info()["user"] == "stuart") {
    
    if (stringr::str_detect(path, "^C:/Users/grst/Dropbox")) {
      path <- stringr::str_replace(path, "C:/Users/grst/Dropbox", "~/Dropbox")
    } else if (stringr::str_detect(path, "^T:")) {
      path <- stringr::str_replace(path, "T:", "/media/stuart/ELEMENTS_II")
    }
    
  }
  
  # To fs
  path <- fs::as_fs_path(path)
  
  return(path)
  
}
