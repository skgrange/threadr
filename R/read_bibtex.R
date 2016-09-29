#' Function to load a \code{BibTeX} file to a data frame. 
#' 
#' @param file File name of a \code{BibTeX} file. URLs work too. 
#' 
#' @param article A vector of \code{BibTeX} keys to filter \code{file} to. If 
#' not used, all entries will be returned. 
#' 
#' @param progress Progress bar to display. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @examples 
#' \dontrun{
#' 
#' # A file from the web
#' url <- "http://www.andy-roberts.net/res/writing/latex/sample.bib"
#' data_bib <- read_bibtex(url)
#' 
#' # To json
#' to_json(data_bib)
#' 
#' 
#' }
#' 
#' @import stringr
#' 
#' @export
read_bibtex <- function(file, skip = 0, article = NA, progress = "none") {
  
  # Load file
  text <- readLines(file)
  text <- str_trim(text)
  
  if (skip >= 1) text <- text[-1:-skip]
  
  # Drop comments
  text <- grep("^%", text, value = TRUE, invert = TRUE)
  
  # Create mapping table to get ranges of articles
  keys <- grep("@", text)
  
  article_key <- text[keys]
  article_key <- grep("@", article_key, value = TRUE)
  article_key <- str_split_fixed(article_key, "\\{", 2)[, 2]
  article_key <- str_replace(article_key, ",$", "")
  
  df_map <- data.frame(
    id = article_key,
    start = keys,
    end = dplyr::lead(keys - 1),
    stringsAsFactors = FALSE
  )
  
  # Drop, will have to change this
  df_map <- df_map[!grepl("jabref", df_map$id, ignore.case = TRUE), ]
  
  # 
  df_map$end <- ifelse(is.na(df_map$end), length(text), df_map$end)
  
  # Filter to article if argument is used
  if (!is.na(article[1])) df_map <- df_map[df_map$id %in% c(article), ]
  
  # Split text into list
  list_text <- plyr::alply(df_map, 1, function(x) split_bibtex_text(text, x))
  attributes(list_text) <- NULL
  
  # Rip out data
  df <- plyr::ldply(list_text, extract_bibtex_variables, .progress = progress)
  
  # Remove double quotes
  index <- sapply(df, is.character)
  df[, index] <- lapply(df[, index], function(x) str_replace_all(x, '^"|"$', ''))
  df[, index] <- lapply(df[, index], function(x) str_create_na(x))
  
  # Return
  df
  
}


split_bibtex_text <- function(text, df_map) text[df_map$start:df_map$end]


extract_bibtex_variables <- function(observation) {
  
  # Prepare
  observation <- str_trim(observation)
  observation <- observation[!observation %in% c("", "}", "},")]
  
  bibtex_key <- grep("@", observation, value = TRUE)
  bibtex_key <- str_split_fixed(bibtex_key, "\\{", 2)[, 2]
  bibtex_key <- str_replace(bibtex_key, ",$", "")
  
  article_type <- grep("@", observation, value = TRUE)
  article_type <- str_replace(article_type, "^@", "")
  article_type <- str_split_fixed(article_type, "\\{", 2)[, 1]
  article_type <- str_to_lower(article_type)
  
  variables <- grep("@", observation, value = TRUE, invert = TRUE)
  variables <- str_split_fixed(variables, "=", 2)
  
  key <- variables[, 1]
  key <- str_trim(key)
  key <- str_to_underscore(key)
  
  value <- variables[, 2]
  value <- str_replace_all(value, "\\{|\\}|,$", "")
  value <- str_trim(value)
  
  value <- c(bibtex_key, article_type, value)
  
  # Named character
  names(value) <- c("bibtex_key", "article_type", key)
  
  # Leading : in file variable, ifelse drops names
  # value <- ifelse(names(value) == "file", str_replace(value, "^:", ""), value)
  
  # Create data frame
  df <- data.frame(t(value), stringsAsFactors = FALSE)
  
  # Return
  df
  
}


#' Function to load a \code{BibTeX} \code{.aux} file and create a vector of unique
#' \code{BibTeX} keys. 
#' 
#' \code{read_bibtex_aux} reads \code{.aux} files created by \code{BibTeX} and 
#' \code{BibLaTeX}. 
#' 
#' @param file File name of a \code{BibTeX} \code{.aux} file. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @import stringr
#' 
#' @rdname read_bibtex
#' @export
read_bibtex_aux <- function(file) {
  
  # Load
  text <- readLines(file)
  
  # Filter
  text_cite <- grep("@cite", text, value = TRUE)
  
  if (length(text_cite) != 0) {
    
    # Biblatex 
    # Clean vector
    keys <- str_split_fixed(text_cite, "\\{", 2)[, 2]
    keys <- str_trim(keys)
    keys <- str_replace_all(keys, "\\}$|,$", "")
    keys <- str_trim(keys)
    
  } else {
    
    # Bibtex
    text_cite <- grep("bibcite", text, value = TRUE)
    
    keys <- str_split_fixed(text_cite, "\\{", 2)[, 2]
    keys <- str_split_fixed(keys, "\\}", 2)[, 1]
    keys <- str_trim(keys)
    
  }
  
  keys <- sort(unique(keys))
  
  # Return
  keys
  
}
