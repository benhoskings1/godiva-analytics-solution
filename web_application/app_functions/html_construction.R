# HTML ELEMENTS
construct_week_overview <- function(week_activities) {
  tags$table(class = "overview_table",
    tags$tr(
     tags$td(
       class = "overview_data",
       tags$div(
         class = "activity_container",
         style = "gap: 10px",
         tags$div(style = "grid-area: type", "Activities"),
         tags$div(style = "grid-area: dist", base::nrow(week_activities)),
         tags$div(style = "grid-area: time", {
           total_time <- week_activities |>
             dplyr::pull(elapsed_time) |>
             base::sum()
           format(lubridate::as_datetime(lubridate::seconds(total_time)), "%H:%M:%S")
         }),
         tags$div(style = "grid-area: icon; font-size: 50px", get_sport_icon("Workout"))
       )
     ),
     tags$td(
        class = "overview_data",
        tags$div(
          class = "activity_container",
          style = "gap: 10px",
          tags$div(style = "grid-area: type", "Runs"),
          tags$div(style = "grid-area: dist",
           base::nrow(
              week_activities |>
                dplyr::filter(sport_type == "Run")
           )),
          tags$div(style = "grid-area: time", {
            total_time <- week_activities |>
              dplyr::filter(sport_type == "Run") |>
              dplyr::pull(elapsed_time) |>
              base::sum()
            format(lubridate::as_datetime(lubridate::seconds(total_time)), "%H:%M:%S")
          }),
          tags$div(style = "grid-area: icon; font-size: 50px", get_sport_icon("Run"))
        )
     )
    )
  )
}


construct_graph_option_table <- function(activity_df) {
  tags$table(style="width: 100%",
    tags$tr(style="padding: 10px", {
      sport_types <- unique(activity_df$sport_type)
      purrr::map(sport_types, function(sport_type) {
        tags$td(style="text-align: left; vertical-align: text-top; padding: 10px",
          tags$h3(sport_type, style="text-align: center"),

          shiny::checkboxGroupInput(
            inputId = glue("{stringr::str_to_lower(sport_type)}_graph_options"),
            choices= activity_graph_options[[sport_type]],
            label = NULL # Label is handled above
          )
        )
      })
    })
  )
}


#' Generate the HTML element for an activity
#'
#' @param activity A Tibble
#' @param settings A named list denoting the metrics to use
#'
#' @return HTML element
#' @export
#'
#' @examples
generate_calendar_cell <- function(activities, settings) {
  activities <- activities |>
    dplyr::group_by(sport_type) |>
    dplyr::summarise(
      distance = sum(distance),
      elapsed_time = sum(elapsed_time),
      distance_km = sum(distance_km),
      distance_miles = sum(distance_miles)
      )

  if (nrow(activities) == 1) {
    activity <- activities |> dplyr::filter(distance == max(distance))

    cell <- tags$div(
      class = "activity_container",
      tags$div(
        class = "grid_item",
        style = "grid-area: icon; font-size: 32px",
        get_sport_icon(activity$sport_type)
      ),
      tags$div(style = "grid-area: type;", activity$sport_type),
      tags$div(style = "grid-area: dist;", {
        if (activity$sport_type %in% names(settings)) {
          if (settings[activity$sport_type] == "metric") {
            sprintf("%.2f km", activity$distance_km)
          } else {
            sprintf("%.2f mi", activity$distance_miles)
          }
        } else {
          sprintf("%.2f", activity$distance)
        }
      }),
      tags$div(style = "grid-area: time;",
        format_time(activity |> dplyr::pull(elapsed_time))
      ),
    )
  } else {
    cell = tags$div(
      tags$table(style="width: 100%; table-layout: fixed; border: none",
        tags$tr(style="border: none",
          purrr::map(1:nrow(activities), function(activity_idx) {
            activity <- activities |>
              dplyr::slice(activity_idx)

            if (activity_idx < 3) {
              tags$td(style = "width: 100%; height: 100%",
                tags$div(class = "activity_data_field",
                  tags$div(style = "grid-area: type",
                    activity |> dplyr::pull(sport_type)),
                  tags$div(style = "grid-area: dist",
                    if (activity$sport_type %in% names(settings)) {
                      if (settings[activity$sport_type] == "metric") {
                        sprintf("%.2f km", activity$distance_km)
                      } else {
                        sprintf("%.2f mi", activity$distance_miles)
                      }
                    } else {
                      sprintf("%.2f", activity$distance)
                    }),
                  tags$div(style = "grid-area: time;",
                    format_time(activity |> dplyr::pull(elapsed_time))
                  )
                )
              )
            } else if (activity_idx == 3 & (nrow(activities) == 3)) {
              tags$td(style = "width: 100%; height: 100%;",
                tags$div(
                  class = "activity_data_field",
                  tags$div(style = "grid-area: type",
                    activity |> dplyr::pull(sport_type)),
                  tags$div(style = "grid-area: dist",
                    if (activity$sport_type %in% names(settings)) {
                      if (settings[activity$sport_type] == "metric") {
                        sprintf("%.2f km", activity$distance_km)
                      } else {
                        sprintf("%.2f mi", activity$distance_miles)
                      }
                    } else {
                      sprintf("%.2f", activity$distance)
                    }),
                  tags$div(style = "grid-area: time;",
                    # convert seconds to correct string
                    format_time(activity |> dplyr::pull(elapsed_time))
                  )
                )
              )
            } else if (activity_idx == 3) {
              tags$td(style = "width: 100%; height: 100%",
                tags$div(sprintf("+%d", nrow(activities) - 2))
              )
            }
          })
        )
      )

    )

  }
  return(cell)
}

generate_activity_detailed <- function(activity, settings) {
  tags$div(
    class = "activity_grid_detailed",
    style = "text-align: left;",
    tags$div(class = "detailed_activity_title", style = "grid-area: title", activity$name),
    {
      if (!is.na(activity$description)) {
        tags$div(style = "grid-area: description", activity$description)
      }
    },
    # tags$div(style="grid-area: description", activity$description),
    tags$div(
      style = "grid-area: stats",
      tags$div(
        class = "stats_holder",
        tags$div(
          class = "vertical-center",
          style = "grid-area: distance;",
          tags$div({
            if (activity$sport_type %in% names(settings)) {
              if (settings[activity$sport_type] == "metric") {
                sprintf("%.2f km", activity$distance_km)
              } else {
                sprintf("%.2f mi", activity$distance_miles)
              }
            } else {
              sprintf("%.2f", activity$distance)
            }
          }),
          tags$div(class = "small_text_1", "Distance")
        ),
        tags$div(
          style = "grid-area: time",
          tags$div(format(
            lubridate::as_datetime(lubridate::seconds(activity$moving_time)), "%H:%M:%S"
          )),
          tags$div(class = "small_text_1", "Moving Time")
        ),
        tags$div(
          style = "grid-area: pace",
          tags$div(calculate_pace(activity, settings)),
          tags$div(class = "small_text_1", "Pace")
        )
      )
    )
  )
}


generate_calendar <- function(date_range, all_activities, settings) {
    range_activities <- all_activities |>
      dplyr::filter(dplyr::between(start_date, date_range[1], date_range[2] + lubridate::days(1)))
    # time_period <- (lubridate::as_date(date_range[2]) - lubridate::as_date(date_range[1]))
    first_monday <-
      lubridate::as_date(date_range[1]) - lubridate::wday(lubridate::as_date(date_range[1]), week_start =
                                                            1) + 1
    next_sunday <-
      base::Sys.Date() + (7 - lubridate::wday(today, week_start = 1))

    tags$table(class = "calendar",
      tags$tr(purrr::map(names(week_days), function(name)
        tags$td(class = "cal_head", name))), {
          rows <- base::ceiling((next_sunday - first_monday) / 7)
          purrr::map(c(1:rows), function(row_idx) {
            tags$tr(class = "calendar_row",
              purrr::map(1:7, function(col_idx) {
                cell_date <- (next_sunday - (rows - row_idx) * 7 - (7 - col_idx))
                activities <- range_activities |>
                  dplyr::filter(lubridate::as_date(start_date) == cell_date)
                bg_colour <-
                  if (!dplyr::between(cell_date, date_range[1], date_range[2])) {
                    "#a9a9a9"
                  } else if (nrow(activities) > 0) {
                    "rgba(0,255,0,0.3);"
                  } else {
                    "rgba(228,228,228,0.3);"
                  }
                tags$td(
                  class = "calendar_cell",
                  style = sprintf("background-color: %s", bg_colour),
                  tags$div(class = "cal_date_num", {
                    if (lubridate::day(cell_date) == 1) {
                      format(cell_date, "%d-%b")
                    } else {
                      format(cell_date, "%d")
                    }
                  }),
                  if (nrow(activities) > 0) {

                    generate_calendar_cell(activities, settings)
                  }
                )
              }))
          })
        }
    )
}

get_num_string <- function(num) {
  num_string <- sprintf("0%d", base::round(num))
  return(substr(num_string, nchar(num_string) - 1, nchar(num_string)))
}

generate_calendar_2 <-function(start_end, day, activity_list, all_activities,
                               graph_options, settings) {
    day_idx <- base::as.numeric(day)
    first_day <- lubridate::as_date(start_end[1], week_start = 1) +
      (day_idx - lubridate::wday(lubridate::as_date(start_end[1]), week_start =
                                   1)) %% 7
    instances <- 0:base::floor((lubridate::as_date(start_end[2], week_start = 1) - first_day) / 7)


    tags$table(style = "background-color: white; width: 100%; padding: 10px;",
      purrr::map(instances, function(idx) {
        cell_date <- first_day + (max(instances) - idx) * 7
        activities <- all_activities |>
          dplyr::filter(lubridate::as_date(start_date) == cell_date)
        tags$tr(# Day Header
          tags$tr(
            tags$td(
              class = "day_title_large",
              style = "padding: 5px",
              format(cell_date, "%A %d %b")
            )
          ),
          # Day Activities
          if (base::nrow(activities) > 0) {
            purrr::map(1:base::nrow(activities), function(act_idx) {
              activity_data <-
                dplyr::slice(activities, act_idx)
              activity_id <- activity_data$activity_id
              activity_data_full <-
                activity_list[activity_id][[1]]
              tags$tr({
                tags$td(
                  style = "width: 100%; height: 100%; padding: 5px; background-color:rgba(228,228,228,0.3);",
                  generate_activity_detailed(activities[act_idx,], settings),
                  tags$div({
                    graph_names <- switch(activity_data_full$sport_type,
                      "Run"={
                        if (length(graph_options$run) > 0) {
                          names = list()
                          for (i in 1:length(graph_options$run)) {
                            names[[i]] = c("time", graph_options$run[i])
                          }
                          names
                        } else {NULL}
                      },
                      "Ride" = {
                        if (length(graph_options$ride) > 0) {
                          names = list()
                          for (i in 1:length(graph_options$ride)) {
                            print(graph_options$run[i])
                            names[[i]] = c("time", graph_options$run[i])
                          }
                          names
                        } else {NULL}
                      },
                      NULL
                    )

                    # print(graph_names)

                    if (length(graph_names) > 0) {
                      purrr::map(1:length(graph_names), function(graph_idx) {
                        var_names = graph_names[[graph_idx]]
                        print(var_names)
                        shiny::renderPlot(
                          generate_activity_graph(
                            activity_data_full,
                            var_names[1], var_names[2],
                            settings,
                            bg_colour = light_blue,
                            line_colour = deep_blue
                          )
                        )
                      })
                    }
                  }
                  )
                )
              })
            })
          } else {
            tags$tr(
              tags$td(style = "background-color:rgba(228,228,228,0.3);", "no activities to show")
            )
          })
      })
    )
  }
