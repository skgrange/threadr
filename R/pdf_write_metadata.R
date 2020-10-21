#' Function to write metadata to a pdf file with \code{exiftool}.
#' 
#' @param file Vector of \code{.pdf} file names. 
#' 
#' @param author Author to write to \code{file}. 
#' 
#' @param title Title to write to \code{file}. 
#' 
#' @param subject Subject to write to \code{file}. 
#' 
#' @param producer Producer to write to \code{file}. 
#' 
#' @param creator Creator to write to \code{file}. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible \code{file}. 
#' 
#' @examples
#' 
#' \dontrun{
#' pdf_write_metadata("~/Desktop/document.pdf")
#' }
#' 
#' @export
pdf_write_metadata <- function(file, author = NA, title = NA, subject = NA, 
                               producer = NA, creator = NA, verbose = FALSE) {
  
  # Check if exiftool is installed
  detect_exiftool()
  
  # Check file types
  if (!stringr::str_detect(unique(fs::path_ext(file)), "(?i)pdf")) {
    stop("Files must be pdf files.", call. = FALSE)
  }
  
  # Do
  purrr::walk(
    file, 
    ~pdf_write_metadata_worker(
      .,
      author = author,
      title = title,
      subject = subject,
      producer = producer,
      creator = creator,
      verbose = verbose
    )
  )
  
  return(invisible(file))
  
}


pdf_write_metadata_worker <- function(file, author, title, subject, producer, 
                                      creator, verbose) {
  
  # Message to user
  if (verbose) message(date_message(), "`", file, "`...")
  
  # Prepare file name
  file <- shQuote(fs::path_expand(file))
  
  # Get R version
  r_version <- r_version()
  r_version_quoted <- shQuote(r_version)
  
  # Catch the defaults
  author <- if_else(is.na(author), r_version_quoted, as.character(author))
  creator <- if_else(is.na(creator), r_version_quoted, as.character(creator))
  producer <- if_else(is.na(producer), r_version_quoted, as.character(producer))
  
  title <- if_else(
    is.na(subject), 
    stringr::str_c("'", r_version, " figure output'"),
    as.character(subject)
  )
  
  subject <- if_else(
    is.na(subject), 
    stringr::str_c("'", r_version, " figure output'"),
    as.character(subject)
  )
  
  # # Remove all metadata first
  # system(stringr::str_c("exiftool -overwrite_original -all= ", file))
  
  # Build command string
  command <- stringr::str_c(
    "exiftool -overwrite_original",
    " -Title=", title,
    " -Subject=", subject,
    " -Producer=", author,
    " -Creator=", r_version_quoted,
    " -Author=", r_version_quoted, 
    " -Keywords=", "'R, figure, pdf output'", 
    " ", file
  )
  
  # Do
  system(command, intern = TRUE)
  
  return(invisible(file))
  
}


r_version <- function() {
  
  list_version <- R.version
  
  # Get pieces
  version_string <- list_version[["version.string"]]
  nickname <- list_version[["nickname"]]
  
  # Clean
  version_string_clean <- version_string %>% 
    stringr::str_split_fixed(" \\(", 2) %>% 
    .[,1] %>% 
    stringr::str_remove(" version") 
  
  return(version_string_clean)
  
}
