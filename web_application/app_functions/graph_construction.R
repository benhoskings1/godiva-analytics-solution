
generate_activity_graph <- function(activity, x_var, y_var, settings, line_colour=NULL, plot_colour=NULL, bg_colour=NULL) {

  stream_data = activity$data_streams
  stream_df <- data.frame(matrix(unlist(stream_data), ncol=length(stream_data)),stringsAsFactors=FALSE)
  names(stream_df) <- names(stream_data)
  stream_df <- stream_df |>
    dplyr::mutate(
      distance = as.numeric(distance),
      distance_m = distance,
      distance_yd = distance_m * 1.0936132983,
      distance_km = distance / 1000,
      distance_mi = distance_km * 0.6213711922
    )

  distance_labels = list(
    "km"="Distance / km", "mi"="Distance / mi",
    "m"="Distance / m", "yd"="Distance / yd",
    "distance"="Distance")

  pace_labels = list(
    "min_mi"="Pace / min per mile", "min_km"="Pace / min per km",
    "min_yd"="Pace / min per 100yd", "min_m"="Pace / min per 100m"
  )

  speed_labels = list(
    "mi_hour"="Speed / mph", "km_min"="Speed / kmph",
    "yd_min"="Pace / 100yd per min", "m_min"="Speed / 100m per min"
  )

  x_settings <- switch(x_var,
                       "time" = {
                         graph_settings <- list(
                           "x_data"=stream_data$time[[1]], "x_label"= "Time"
                         )
                       },
                       "distance" = {
                         distance_unit <- get_distance_unit(activity$sport_type, settings[activity$sport_type])
                         distance_vals <- get_activity_metric(stream_df, distance_unit)

                         graph_settings <- list(
                           "x_data"=distance_vals, "x_label"= paste("Distance /", distance_unit)
                         )
                       }
  )

  y_settings <- switch(y_var,
                       "heartrate" = {
                         graph_settings <- list("data"=stream_data$heartrate[[1]], "label"= "Heart Rate / bpm")
                       },
                       "altitude" = {
                         graph_settings <- list("data"=stream_data$altitude[[1]], "label"= "Elevation / m")
                       },
                       "temp" = {
                         graph_settings <- list("data"=stream_data$temp[[1]], "label"= "Temperature / degrees")
                       },
                       "distance" = {
                         distance_unit <- get_distance_unit(activity$sport_type, settings[activity$sport_type])
                         distance_vals <- get_activity_metric(stream_df, distance_unit)
                         graph_settings <- list("data"=distance_vals, "label"= paste("Distance /", distance_unit))
                       },
                       "pace" = {
                         pace_unit <- get_pace_unit(activity$sport_type, settings[activity$sport_type])
                         pace_vals <- get_activity_metric(stream_df, pace_unit)
                         pace_vals[pace_vals > 30*60] = NA
                         graph_settings <- list("data"=pace_vals, "label"= pace_labels[pace_unit])
                       },
                       "speed" = {
                         pace_unit <- get_speed_unit(activity$sport_type, settings[activity$sport_type])
                         pace_vals <- get_activity_metric(stream_df, pace_unit)
                         pace_vals[pace_vals > 30] = NA
                         graph_settings <- list("data"=pace_vals, "label"= speed_labels[pace_unit])
                       }
  )

  plot_data = data.frame(x = x_settings$x_data,
                         y = rtrend::movmean(y_settings$data, 10))

  # create plot
  p <- ggplot(plot_data, aes(x=x, y=y, group=1))


  # Add geometry
  p <- p + geom_line(linewidth=1.5, aes(color=line_colour))

  # Add x and y labels
  p <- p + xlab(x_settings$x_label) + ylab(y_settings$label)

  # Add graph style
  p <- p + scale_color_manual(values=c(line_colour)) +
    theme(
      panel.background = element_rect(fill = 'white', color = 'white'),
      panel.grid.major.x = element_line(color = 'lightgrey'),
      panel.grid.major.y = element_line(color = 'lightgrey'),
      legend.position="none")

  if (x_var == "time") {
    p <- p + scale_x_time(labels = function(l) {
      dplyr::case_when(
        l < 3600 ~ {strftime(l, '%M:%S')},
        .default = {strftime(l, '%H:%M:%S')}
      )
    })
  }

  if (y_var == "pace") {
    p <- p + scale_y_time(labels = function(l) {
      print(l)
      dplyr::case_when(
        l < 3600 ~ {strftime(l, '%M:%S')},
        .default = {strftime(l, '%H:%M:%S')}
      )
    })
  }

  if (!is.null(bg_colour)) {
    p + theme(plot.background = element_rect(fill=bg_colour, color=bg_colour))
  } else {p}
}
