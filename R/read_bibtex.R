#' Function to load a \code{BibTeX} file to a data frame. 
#' 
#' @param file File name of a \code{BibTeX} file. URLs work too. 
#' 
#' @param skip How many lines to skip before starting reading? This is useful
#' if \code{file} contains a non-commented preamble. 
#' 
#' @param article A vector of \code{BibTeX} keys to filter \code{file} to. If 
#' not used, all entries will be returned. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' # A file from the web
#' url <- "http://www.andy-roberts.net/res/writing/latex/sample.bib"
#' data_bib <- read_bibtex(url)
#' 
#' @export
read_bibtex <- function(file, skip = 0, article = NA) {
  
  # Load file
  text <- read_lines(file)
  
  # Drop top
  if (skip >= 1) text <- text[-1:-skip]
  
  # Drop white space
  text <- str_trim(text)
  
  # Drop all comments, latex and jab ref
  text <- grep("^%|^@comment", text, value = TRUE, invert = TRUE)
  
  # Drop empty lines
  text <- text[!text == ""]
  
  # Get article keys
  index_keys <- grep("@", text)
  article_key <- text[index_keys]
  article_key <- clean_bibtex_article_key(article_key)
  
  # Create mapping table to get ranges of articles
  df_map <- tibble(
    article_key = article_key,
    start = index_keys,
    end = dplyr::lead(index_keys - 1)
  )
  
  # Final observation
  df_map$end <- ifelse(is.na(df_map$end), length(text), df_map$end)
  
  # Filter to article if argument is used
  if (!is.na(article[1])) df_map <- df_map[df_map$article_key %in% c(article), ]
  
  # Split text into list
  list_text <- purrr::map2(
    df_map$start, 
    df_map$end, 
    ~split_bibtex_text(.x, .y, text)
  )
  
  # Extract data for each article
  df <- purrr::map_dfr(list_text, extract_bibtex_variables)
  
  # Remove double quotes
  index <- purrr::map_lgl(df, is.character)
  df[, index] <- lapply(df[, index], function(x) str_remove_all(x, '^"|"$'))
  df[, index] <- lapply(df[, index], function(x) str_create_na(x))
  
  return(df)
  
}


clean_bibtex_article_key <- function(x) {
  
  x <- str_split_fixed(x, "\\{", 2)[, 2]
  x <- str_remove(x, ",$")
  return(x)
  
}


clean_bibtex_article_type <- function(x) {
  
  x <- str_remove(x, "^@")
  x <- str_split_fixed(x, "\\{", 2)[, 1]
  x <- str_to_lower(x)
  return(x)
  
}


split_bibtex_text <- function(start, end, text) text[start:end]


extract_bibtex_variables <- function(observation) {
  
  # Prepare
  observation <- observation[!observation %in% c("", "}", "},")]
  
  # Get article key
  bibtex_type_and_key <- grep("@", observation, value = TRUE)
  
  # Clean key
  bibtex_key <- clean_bibtex_article_key(bibtex_type_and_key)
  
  # Clean article type
  article_type <- clean_bibtex_article_type(bibtex_type_and_key)
  
  # Get the data
  variables <- grep("@", observation, value = TRUE, invert = TRUE)
  
  # Split by =
  variables <- str_split_fixed(variables, "=", 2)
  
  # Isolate the key and values
  key <- variables[, 1]
  key <- str_trim(key)
  key <- str_to_underscore(key)
  
  value <- variables[, 2]
  value <- str_replace_all(value, "\\{|\\}|,$", "")
  value <- str_trim(value)
  
  # Add the extras
  value <- c(bibtex_key, article_type, value)
  
  # Build named character
  names(value) <- c("bibtex_key", "article_type", key)
  
  # Create data frame
  df <- data.frame(
    t(value), 
    stringsAsFactors = FALSE
  )
  
  # To tibble
  df <- as_tibble(df)
  
  return(df)
  
}


#' Function to load a \code{BibTeX} \code{.aux} file and create a vector of unique
#' \code{BibTeX} keys. 
#' 
#' \code{read_bibtex_aux} reads \code{.aux} files created by \code{BibTeX} and 
#' \code{BibLaTeX}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
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
  
  return(keys)
  
}
