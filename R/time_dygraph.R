#' Function for plotting interactive time-series plots in a similar way to
#' \code{openair::timePlot}. 
#' 
#' @param df Data frame with a variable/column named "date". 
#' 
#' @param variable Variable in \code{df} to plot. 
#' 
#' @param color Color of plotted geometry. 
#' 
#' @param range Should a range selector be plotted? 
#' 
#' @param step Should the plot take a stepped look? 
#' 
#' @param points Should the plot include points where observations are recorded? 
#' 
#' @param fill Should the plot fill between the x-axis and the geometry? 
#' 
#' @author Stuart K. Grange
#' 
#' @import dygraphs
#' 
#' @export
time_dygraph <- function (df, variable = "no2", colour = "dodgerblue", 
                          range = TRUE, step = FALSE, points = FALSE, fill = FALSE,
                          color = colour) {
  
  # Catch dplyr's table data frame
  df <- base_df(df)
  
  # missing(colour)
  
  # Create timeseries object
  time_series <- xts::xts(df[, variable], df[, "date"], order.by = df[, "date"], 
                          tzone = "UTC")
  
  # Plot
  plot <- dygraph(time_series) %>%  
    dyOptions(colors = color, stepPlot = step, fillGraph = fill, 
              useDataTimezone = TRUE) %>% 
    dySeries(label = variable, drawPoints = points) 
  
  # Add range selector
  if (range) {
    plot <- plot %>% 
      dyRangeSelector()
    
  }
  
  # Return
  plot
  
}