#' Function to load a BibTeX file to a data frame. 
#' 
#' @param file File name of a BibTeX file. 
#' 
#' @param progress Progress bar to display. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @import stringr
#' 
#' @export
read_bibtex <- function(file, progress = "none") {
  
  # Load file
  text <- readLines(file)
  # text <- text[!text == ""]
  
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
  
  # Drop
  df_map <- df_map[!grepl("jabref", df_map$id, ignore.case = TRUE), ]
  
  # Split text into list
  list_text <- plyr::alply(df_map, 1, function(x) split_bibtex_text(text, x))
  attributes(list_text) <- NULL
  
  # Rip out data
  df <- plyr::ldply(list_text, extract_bibtex_variables, .progress = progress)
  
  # Return
  df
  
}


split_bibtex_text <- function(text, df_map) text[df_map$start:df_map$end]



extract_bibtex_variables <- function(observation) {
  
  # Prepare
  observation <- observation[!observation %in% c("", "}")]
  
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
  names(value) <- c("bibtex_key", "article_type", key)
  
  # Create data frame
  df <- data.frame(t(value), stringsAsFactors = FALSE)
  
  # Return
  df
  
}
