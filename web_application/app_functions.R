
get_sport_icon <- function(sport_type) {
  icon <- switch (sport_type,
    "Run" = icon("running", lib = "font-awesome"),
    "Ride" = icon("bicycle", lib = "font-awesome"),
    "Stopwatch" = icon("stopwatch", lib = "font-awesome"),
    "Swim" = icon("person-swimming", lib = "font-awesome"),
    icon("dumbbell", lib="font-awesome")
  )
}

f_load_credentials <- function() {
  con_SQLite <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname = v_database_path)

  df_credentials <- RSQLite::dbReadTable(conn = con_SQLite, name = "credentials")

  RSQLite::dbDisconnect(conn = con_SQLite)

  return(df_credentials)
}

f_save_credentials <- function(data) {
  con_SQLite <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=v_database_path)
  RSQLite::dbWriteTable(conn = con_SQLite, name = "credentials", value=df_credentials, overwrite=T)

  RSQLite::dbDisconnect(conn=con_SQLite)
}


# App functions --------

f_get_athlete_week_data <- function(all_data, athlete_id, date_range) {
  week_data <- all_data |>
    dplyr::filter(
      Athlete_ID == athlete_id,
      dplyr::between(start_date,
                     lubridate::as_date(date_range[1]),
                     lubridate::as_date(date_range[2]))) |>
    dplyr::mutate(
      title = name,
      days_since = Sys.Date() - lubridate::as_date(start_date)
    )
}

f_create_team_overview <- function(first_name, last_name, week, month) {
  if (is.null(last_name)) {
    name_string <- sprintf("%s", stringr::str_sub(first_name, 1, 1))
  } else {
    name_string <- sprintf("%s%s", stringr::str_sub(first_name, 1, 1),
                           stringr::str_sub(last_name, 1, 1))
  }

  tags$div(
    class="team_container",
    tags$div(class="circle", name_string),
    tags$div(class="week_dist_container", sprintf("Week: %2.2f", week)),
    tags$div(class="month_dist_container", sprintf("Month: %2.2f", month))
  )
}

convert_seconds_to_string <- function(value) {
  return(sprintf("%d:%s", base::floor(value / 60), get_num_string(value %% 60)))
}

calculate_pace <- function(activity, settings) {
  pace <- switch(activity$sport_type,
    "Run" = {
      if (settings[activity$sport_type] == "metric") {
        # mins per km
        val <- activity$moving_time / (activity$distance_km)
        sprintf("%s /km", convert_seconds_to_string(val))
      } else {
        # mins per mile
        val <- activity$moving_time / (activity$distance_miles)
        sprintf("%s /mi", convert_seconds_to_string(val))
      }
    },
    "Swim" = {
      if (settings[activity$sport_type] == "metric") {
        # mins per 100m
        val <- (100 * activity$moving_time) / (activity$distance_m)
        sprintf("%s /100m", convert_seconds_to_string(val))
      } else {
        val <- (100 * activity$moving_time) / (activity$distance_yd)
        sprintf("%s /100yd", convert_seconds_to_string(val))
      }
    }
  )
  return(pace)
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
generate_activity <- function(activity, settings) {
  cell <- tags$div(class="activity_container",
    tags$div(class = "grid_item",
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
      format(
        lubridate::as_datetime(lubridate::seconds(activity$elapsed_time)),"%H:%M:%S"
      )
    ),
  )
  return(cell)
}

generate_activity_detailed <- function(activity, settings) {
  tags$div(class="activity_grid_detailed", style="text-align: left;",
    tags$div(class="detailed_activity_title", style="grid-area: title", activity$title), {
    if (!is.na(activity$description)) {
      tags$div(style="grid-area: description", activity$description)
    }},
    # tags$div(style="grid-area: description", activity$description),
    tags$div(style="grid-area: stats",
      tags$div(class="stats_holder",
        tags$div(class="vertical-center", style="grid-area: distance;",
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
          tags$div(class="small_text_1", "Distance")
        ),
        tags$div(style="grid-area: time",
          tags$div(format(
            lubridate::as_datetime(lubridate::seconds(activity$moving_time)),"%H:%M:%S"
          )),
          tags$div(class="small_text_1", "Moving Time")
        ),
        tags$div(style="grid-area: pace",
          tags$div(calculate_pace(activity, settings)),
          tags$div(class="small_text_1", "Pace")
        )
      )
    )
  )
}


generate_calendar <- function(start_end, all_activities, settings) {
  date_range <- (lubridate::as_date(start_end[2]) - lubridate::as_date(start_end[1]))
  first_monday <- lubridate::as_date(start_end[1]) - lubridate::wday(lubridate::as_date(start_end[1]), week_start=1)+1
  next_sunday <- base::Sys.Date()+(7-lubridate::wday(today, week_start=1))

  tags$table(class="calendar",
     tags$tr(purrr::map(names(week_days), function(name)
       tags$td(class = "cal_head", name)
       )
     ), {
     rows <- base::ceiling((next_sunday - first_monday) / 7)
     purrr::map(c(1:rows), function(row_idx) {
       tags$tr(class = "calendar_row",
       purrr::map(1:7, function(col_idx) {
         cell_date <- (next_sunday - (rows - row_idx) * 7 - (7 - col_idx))
         activities <- all_activities |>
           dplyr::filter(lubridate::as_date(start_date) == cell_date)

         bg_colour <-
           if (!dplyr::between(cell_date, start_end[1], start_end[2])) {
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
             main <-activities |> dplyr::filter(elapsed_time == max(elapsed_time))
             generate_activity(main, settings)
           }
         )
       }))
     })
   })
}

get_num_string <- function(num) {
  num_string <- sprintf("0%d", base::round(num))
  return(substr(num_string, nchar(num_string)-1, nchar(num_string)))
}

generate_calendar_2 <- function(start_end, day, all_activities, settings) {

  day_idx <- base::as.numeric(day)
  first_day <- lubridate::as_date(start_end[1], week_start=1) +
    (day_idx - lubridate::wday(lubridate::as_date(start_end[1]), week_start=1)) %% 7

  instances <- 0:base::floor((lubridate::as_date(start_end[2], week_start=1) - first_day) / 7)

  tags$table(style = "background-color: white; width: 100%; padding: 10px;",
    purrr::map(instances, function(idx) {
      cell_date <- first_day + (max(instances) - idx)*7

      activities <- all_activities |>
        dplyr::filter(lubridate::as_date(start_date) == cell_date)

      tags$tr(
        # Day Header
        tags$tr(
          tags$td(class="day_title_large", style="padding: 5px",
            format(cell_date, "%A %d %b")
          )
        ),
        # Day Activities
        if (base::nrow(activities) > 0) {
          purrr::map(1:base::nrow(activities), function(act_idx) {
            tags$tr({
              tags$td(style = "width: 100%; height: 100%; padding: 5px; background-color:rgba(228,228,228,0.3);",
                generate_activity_detailed(activities[act_idx, ], settings)
              )
            })
          })
        } else {
          tags$tr(tags$td(style="background-color:rgba(228,228,228,0.3);", "no activities to show"))
        }
      )
    })
  )
}
