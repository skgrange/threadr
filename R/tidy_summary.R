#' Function to create tidy data descriptives from a data frame. 
#' 
#' @param df Data frame for descriptives to be calculated for.
#' 
#' @param round Rounding precision of descriptives. Default is \code{3}. 
#' 
#' @param json Should the return be a pretty JSON array? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame or pretty printed JSON. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Calcuate summaries
#' data_summary <- tidy_summary(data_air)
#' 
#' }
#' 
#' @export
tidy_summary <- function (df, round = 3, json = FALSE) {
  
  # Summarise
  table <- summary(df)
  
  # To data frame
  df <- data.frame(table, stringsAsFactors = FALSE)
  
  # Drop and filter
  df[, 1] <- NULL
  df <- df[!is.na(df[, 2]), ]
  
  # Useful names
  names(df) <- c("variable", "output")
  
  # Separate variables
  output_split <- stringr::str_split_fixed(df$output, pattern = ":", n = 2)
  output_split <- apply(output_split, 2, stringr::str_trim)
  
  df$descriptive <- output_split[, 1]
  df$value <- output_split[, 2]
  
  # Drop variable
  df$output <- NULL

  # Calculate extras
  suppressWarnings(
    sd <- lapply(df, sd, na.rm = TRUE)
  )
  
  df_sd <- data.frame(
    variable = names(sd),
    descriptive = "sd",
    value = as.character(unlist(unname(lapply(sd, "[[", 1)))),
    stringsAsFactors = FALSE
  )
  
  # and variance
  suppressWarnings(
    variance <- lapply(df, var, na.rm = TRUE)
  )
  
  df_variance <- data.frame(
    variable = names(variance),
    descriptive = "variance",
    value = as.character(unlist(unname(lapply(variance, "[[", 1)))),
    stringsAsFactors = FALSE
  )
  
  # Bind summaries together
  df <- dplyr::bind_rows(df, df_sd, df_variance)
  
  # String processing
  df$variable <- stringr::str_trim(df$variable)
  
  df$descriptive <- stringr::str_to_lower(df$descriptive)
  df$descriptive <- stringr::str_replace(df$descriptive, " ", "_")
  df$descriptive <- stringr::str_replace_all(df$descriptive, "\\.$|'", "")
  
  # Name changes
  df$descriptive <- stringr::str_replace(df$descriptive, "_qu", "_quartile")
  df$descriptive <- stringr::str_replace(df$descriptive, "1st", "first")
  df$descriptive <- stringr::str_replace(df$descriptive, "3rd", "third")
  
  # Reshape
  df <- tidyr::spread(df, descriptive, value, convert = TRUE)
  
  # Arrange
  df <- arrange_left(df, 
    c("variable", "class", "length", "mean", "median", "min", "max", "mode", 
      "sd", "variance", "first_quartile", "third_quartile", "nas"))
  
  # To json
  if (json) df <- to_json(df)
  
  # Return
  df

}
