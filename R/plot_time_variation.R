#' Function to plot hourly, weekday, and monthly averages. 
#' 
#' @param df Input data frame with at least two variables, two of which are 
#' named \code{value} and \code{date}. 
#' 
#' @param by A grouping variable to separately plot different variables.
#' 
#' @param n_min How many observations are needed to create a valid confidence 
#' interval? By default, at least two observations are needed to avoid very 
#' large bounds that results in bad y-axes scales. 
#' 
#' @param colours A vector of colours to use for the plotting. 
#' 
#' @param ylim Limits for y-axes.
#' 
#' @author Stuart K. Grange
#' 
#' @return A \strong{ggplot2} list object containing four plots. 
#' 
#' @export
plot_time_variation <- function(df, by = NA, n_min = 2, colours = NA, 
                                ylim = c(NA, NA)) {
  
  # Check inputs
  stopifnot("value" %in% names(df) && is.numeric(df$value))
  stopifnot("date" %in% names(df) && lubridate::is.POSIXct(df$date))
  
  # If no by is included
  if (is.na(by[1])) {
    by <- "variable"
    df <- mutate(df, variable = "value")
  }
  
  # Prepare input
  df <- df %>% 
    mutate(weekday = lubridate::wday(date, label = TRUE, week_start = 1),
           hour = lubridate::hour(date),
           month = lubridate::month(date, label = TRUE)) %>% 
    dplyr::group_by_at(by)
  
  # Calculate aggregations
  # Hourly weekdays
  df_weekday_hours <- df %>% 
    group_by(weekday,
             hour,
             .add = TRUE) %>% 
    dplyr::group_modify(~calculate_ci(.$value)) %>% 
    ungroup() %>% 
    mutate(across(c("lower", "upper"), ~if_else(n <= !!n_min, NA_real_, .)))
  
  # Hour
  df_hour <- df %>% 
    group_by(hour,
             .add = TRUE) %>% 
    dplyr::group_modify(~calculate_ci(.$value)) %>% 
    ungroup() %>% 
    mutate(across(c("lower", "upper"), ~if_else(n <= !!n_min, NA_real_, .)))
  
  # Weekday
  df_weekday <- df %>% 
    group_by(weekday,
             .add = TRUE) %>% 
    dplyr::group_modify(~calculate_ci(.$value)) %>% 
    ungroup() %>% 
    mutate(across(c("lower", "upper"), ~if_else(n <= !!n_min, NA_real_, .)))
  
  # Monthly
  df_month <- df %>% 
    group_by(month,
             .add = TRUE) %>% 
    dplyr::group_modify(~calculate_ci(.$value)) %>% 
    ungroup() %>% 
    mutate(across(c("lower", "upper"), ~if_else(n <= !!n_min, NA_real_, .)))
  
  # For plotting
  by_symbol <- sym(by)
  
  # Build the plots
  plot_weekday_hours <- df_weekday_hours %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        hour,
        mean, 
        ymin = lower,
        ymax = upper, 
        fill = !!by_symbol, 
        colour = !!by_symbol,
        group = !!by_symbol)
    ) + 
    ggplot2::geom_ribbon(alpha = 0.3, colour = NA) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    ggplot2::facet_wrap("weekday", ncol = 7) + 
    theme_less_minimal(legend_position = "bottom")
  
  plot_hours <- df_hour %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        hour,
        mean, 
        ymin = lower,
        ymax = upper, 
        fill = !!by_symbol, 
        colour = !!by_symbol,
        group = !!by_symbol)
    ) + 
    ggplot2::geom_ribbon(alpha = 0.3, colour = NA) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    theme_less_minimal(legend_position = "none")
  
  plot_weekday <- df_weekday %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        weekday,
        mean, 
        ymin = lower,
        ymax = upper, 
        fill = !!by_symbol, 
        colour = !!by_symbol,
        group = !!by_symbol)
    ) + 
    ggplot2::geom_ribbon(alpha = 0.3, colour = NA) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    theme_less_minimal(legend_position = "none")
  
  plot_month <- df_month %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        month,
        mean, 
        ymin = lower,
        ymax = upper, 
        fill = !!by_symbol, 
        colour = !!by_symbol,
        group = !!by_symbol)
    ) + 
    ggplot2::geom_ribbon(alpha = 0.3, colour = NA) + 
    ggplot2::geom_line() + 
    ggplot2::scale_x_discrete(drop = FALSE) + 
    theme_less_minimal(legend_position = "none")
  
  # Add all plots to a list
  list_plots <- list(
    weekday_hours = plot_weekday_hours,
    hours = plot_hours,
    weekday = plot_weekday,
    month = plot_month
  )
  
  # Add colours to plots
  if (!is.na(colours[1])) {
    list_plots <- purrr::map(list_plots, add_colours_to_plot, colours = colours)
  }
  
  # Add ylims to plots
  if (!all(is.na(ylim))) {
    list_plots <- purrr::map(list_plots, add_ylim_to_plot, ylim = ylim)
  }
  
  # Combine plots
  plot <- cowplot::plot_grid(
    list_plots$weekday_hours, 
    cowplot::plot_grid(list_plots$hours, list_plots$weekday, list_plots$month, nrow = 1), 
    ncol = 1
  )
  
  return(plot)
  
}


add_colours_to_plot <- function(plot, colours) {
  plot + 
    ggplot2::scale_colour_manual(values = colours) + 
    ggplot2::scale_fill_manual(values = colours)
}


add_ylim_to_plot <- function(plot, ylim) {
  plot + ggplot2::ylim(ylim)
}
