#' Function for plotting interactive time-series plots in a similar way to
#' \code{\link{timePlot}}. 
#' 
#' @param df Data frame with a variable/column named \code{"date"}.
#' 
#' @param variable Variable in \code{df} to plot. Default is \code{"value"}. 
#' \code{variable} can be a vector but faceting is not yet supported. 
#' 
#' @param colour Colour of plotted geometry. Default is \code{"dodgerblue"}. 
#' 
#' @param range Should a range selector be plotted? Default is \code{TRUE}.
#' 
#' @param step Should the plot take a stepped look? Default is \code{FALSE}.
#' 
#' @param points Should the plot include points where observations are recorded? 
#' Default is \code{FALSE}.
#' 
#' @param fill Should the plot fill between the x-axis and the geometry? Default
#' is \code{FALSE}.
#' 
#' @param ylab String for y-axis label. 
#' 
#' @param legend_width Width of legend in pixels. Default is \code{400}. 
#' 
#' @param mouse_label String for legend label. If not used, label will take the
#' name of \code{variable}. 
#' 
#' @param tz Timezone to display plot with. If not used, \code{tz} will use the
#' time-zone of the \code{df}'s \code{"date"} variable. 
#' 
#' @param window An optional range window. Uses a date vector with a length of 
#' two. 
#'
#' @seealso \code{\link{timePlot}}
#' 
#' @author Stuart K. Grange
#' 
#' @export
time_dygraph <- function (df, variable = "value", colour = "dodgerblue", 
                          range = TRUE, step = FALSE, points = FALSE, fill = FALSE,
                          color = colour, ylab = NA, legend_width = 400,
                          mouse_label = NA, tz = NA, window = NULL) {
  
  # Check
  if (nrow(df) == 0) stop("No data to plot.", call. = FALSE)
  
  # Catch dplyr's table data frame
  df <- base_df(df)
  
  if (is.na(mouse_label) & length(mouse_label) == 1) mouse_label <- variable
  if (is.na(tz)) tz <- time_zone(df[, "date"])
  
  # Create time-series objects
  if (length(variable) == 1) {
    
    # Single variable
    time_series <- xts::xts(df[, variable], df[, "date"], order.by = df[, "date"], 
                            tzone = tz)
    
  } else {
    
    # Select variables
    df <- df[, c("date", variable)]
    
    # Promote
    list_ts <- data_frame_to_timeseries(df)
    
    # Bind
    time_series <- do.call(cbind, list_ts)
    
    # Fix names, names are mashed
    names(time_series) <- variable
    
    # Also alter colours if needed
    if (length(colour) != length(variable))
      colour <- ggplot2_colours(length(variable))
    
    # Use names
    mouse_label <- NULL
    
  }
  
  # Plot
  plot <- dygraphs::dygraph(time_series) %>%  
    dygraphs::dyOptions(colors = colour, stepPlot = step, fillGraph = fill, 
                        useDataTimezone = TRUE) %>% 
    dygraphs::dySeries(label = mouse_label, drawPoints = points) %>% 
    dygraphs::dyLegend(width = legend_width)
  
  # Add range selector
  if (range) {
    
    plot <- plot %>% 
      dygraphs::dyRangeSelector(dateWindow = window)
    
  }
  
  if (!is.na(ylab)) {
    
    plot <- plot %>% 
      dygraphs::dyAxis("y", label = ylab)
    
  }
  
  # Return
  plot
  
}
