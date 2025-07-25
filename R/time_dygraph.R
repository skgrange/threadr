#' Function for plotting interactive time-series plots in a similar way to
#' \code{timePlot}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame with a variable/column named \code{"date"}.
#' 
#' @param variable Variable in \code{df} to plot. Default is \code{"value"}. 
#' \code{variable} can be a vector but faceting is not yet supported. 
#' 
#' @param colour,color Colour of plotted geometry. Default is 
#' \code{"dodgerblue"}. 
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
#' @param tz Timezone to display plot with. If not used, \code{tz} will use the
#' time-zone of the \code{df}'s \code{"date"} variable. 
#' 
#' @param window An optional range window. Uses a date vector with a length of 
#' two. 
#' 
#' @param group An optional grouping string for when multiple plots are to be 
#' connected when rendered in an HTML document. 
#' 
#' @param y_reference Location of a y-axis reference line. 
#' 
#' @param main The plot's title. 
#' 
#' @param highlight On mouse-over, should the time series be highlighted? 
#' 
#' @return Invisible, an interactive \strong{dygraph}.  
#'
#' @seealso \code{\link[openair]{timePlot}}, \code{\link[dygraphs]{dygraph}}, 
#' \code{\link{plot_time_series}}
#' 
#' @export
time_dygraph <- function(df, variable = "value", colour = "dodgerblue", 
                         range = TRUE, step = FALSE, points = FALSE, 
                         fill = FALSE, color = colour, ylab = NA, 
                         legend_width = 400, tz = NA, window = NULL, 
                         group = NULL, y_reference = NA, main = NULL,
                         highlight = FALSE) {
  
  # Check Input
  if (nrow(df) == 0) {
    cli::cli_abort("There are no observations to plot.")
  }
  
  if (!"date" %in% names(df)) {
    cli::cli_abort("`date` must be present in data frame.")
  }
  
  if (!lubridate::is.POSIXct(df$date)) {
    cli::cli_abort("`date` must be a parsed date (POSIXct).") 
  }
  
  # Check variable names
  input_names <- names(df)
  
  if (!all(variable %in% input_names)) {
    cli::cli_abort("All variables are not within input data.")
  }
  
  # Get time zone from date in input data frame
  if (is.na(tz)) tz <- time_zone(df$date)
  
  # Create time-series objects
  if (length(variable) == 1) {
    
    # For a single variable
    # Message suppression is for registered S3 method overwritten message
    suppressMessages(
      time_series <- xts::xts(
        df[, variable, drop = TRUE], 
        df$date, 
        order.by = df$date, 
        tzone = tz
      )
    )
    
  } else {
    
    # Select variables
    df <- df[, c("date", variable), drop = FALSE]
    
    # Promote data frame to time series object
    list_ts <- data_frame_to_timeseries(df, tz = tz)
    
    # Bind
    time_series <- do.call(cbind, list_ts)
    
    # Also alter colours if needed
    if (length(colour) != length(variable)) {
      colour <- ggplot2_colours(length(variable))
    }
    
  }
  
  # Fix names, names can be mashed
  names(time_series) <- variable
  
  # TODO: add vertical reference lines
  # https://rstudio.github.io/dygraphs/gallery-event-lines.html
  
  # Plot
  plot <- dygraphs::dygraph(time_series, main = main, group = group) %>%  
    dygraphs::dyOptions(
      colors = colour, 
      stepPlot = step, 
      fillGraph = fill, 
      useDataTimezone = TRUE
    ) %>% 
    dygraphs::dySeries(drawPoints = points) %>% 
    dygraphs::dyLegend(width = legend_width)
  
  # Add range selector
  if (range) {
    plot <- dygraphs::dyRangeSelector(plot, dateWindow = window)
  }
  
  # Label for y-axis
  if (!is.na(ylab)) {
    plot <- dygraphs::dyAxis(plot, "y", label = ylab)
  }
  
  # Reference line
  if (!is.na(y_reference)[1]) {
    plot <- dygraphs::dyLimit(plot, y_reference)
  }
  
  # Highlight the time series during mouse-over
  if (highlight) {
    plot <- dygraphs::dyHighlight(
      plot, highlightSeriesBackgroundAlpha = 0.5, hideOnMouseOut = TRUE
    )
  }
  
  return(plot)
  
}
