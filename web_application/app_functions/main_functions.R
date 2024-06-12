
#' Map database result to named list
#'
#' @param db_result
#'
#' @return large list of all activities
#' @export
#'
#' @examples
map_db_to_list <- function(db_result) {
  all_consults <- list()
  for (idx in 1:nrow(db_result)) {
    df_row <- db_result |>
      dplyr::slice(idx)

    all_consults[[paste(df_row$activity_id)]] <- list(
      "activity_id"=paste(df_row$activity_id),
      "user_id"=df_row$athlete_id, "start_date"=df_row$start_date,
      "sport_type"=df_row$sport_type, "name"=df_row$name,
      "description"=df_row$description,
      "overview" =list(
        "distance"=as.numeric(df_row$distance), "elapsed_time"=as.numeric(df_row$elapsed_time),
        "moving_time" = as.numeric(df_row$moving_time)
      ),
      "data_streams"=df_row$data_streams
    )
  }

  return(all_consults)
}

#' Return distance unit for a given sport
#'
#' @param sport_type Strava sport type (Run, Ride, Swim, etc...)
#' @param system Measurement system (imperial or metric)
#'
#' @return Unit as a string
#' @export
#'
#' @examples get_distance_unit("Run", "metric")
get_distance_unit <- function(sport_type, system) {
  unit <- dplyr::case_when(
    sport_type %in% c("Run", "Ride") ~ {
      if (system == "metric") {"km"} else {"mi"}
    },
    sport_type %in% c("Swim") ~ {
      if (system == "metric") {"m"} else {"yd"}
    },
    .default = NULL
  )
}

get_pace_unit <- function(sport_type, system) {
  unit <- dplyr::case_when(
    sport_type %in% c("Run", "Ride") ~ {
      if (system == "metric") {"min_km"} else {"min_mi"}
    },
    sport_type %in% c("Swim") ~ {
      if (system == "metric") {"min_m"} else {"min_yd"}
    },
    .default = NULL
  )
}

format_time <- function(secs) {
  # print(secs)
  if (lubridate::seconds(secs) < 3600) {
    time_string <- format(lubridate::as_datetime(lubridate::seconds(secs)), "%M:%S")
  } else {
    time_string <- format(lubridate::as_datetime(lubridate::seconds(secs)), "%H:%M:%S")
  }

  # print(time_string)
  time_string
}

get_speed_unit <- function(sport_type, system) {
  unit <- dplyr::case_when(
    sport_type %in% c("Run", "Ride") ~ {
      if (system == "metric") {"km_hour"} else {"mi_hour"}
    },
    sport_type %in% c("Swim") ~ {
      if (system == "metric") {"m_min"} else {"yd_min"}
    },
    .default = NULL
  )
}

get_activity_metric <- function(data_streams, unit_string) {
  values <- switch(unit_string,
    # distance values
    "mi" = {data_streams$distance_mi}, "km" = {data_streams$distance_km},
    "yd" = {data_streams$distance_yd}, "m" = {data_streams$distance_m},

    # Pace values
    "min_mi" ={diff(c(0, data_streams$time)) / diff(c(0, data_streams$distance_mi))},
    "min_km" ={diff(c(0, data_streams$time)) / diff(c(0, data_streams$distance_km))},
    "min_yd" ={diff(c(0, data_streams$time)) / diff(c(0, 100*data_streams$distance_yd))},
    "min_m" = {diff(c(0, data_streams$time)) / diff(c(0, 100*data_streams$distance_m))},

    # Speed values
    "mi_hour" ={diff(c(0, data_streams$distance_mi)) / diff(c(0, data_streams$time / 3600))},
    "km_hour" ={diff(c(0, data_streams$distance_km)) / diff(c(0, data_streams$time / 3600))},
    "yd_min" ={diff(c(0, 100*data_streams$distance_yd)) / diff(c(0, data_streams$time / 60))},
    "m_min" = {diff(c(0, 100*data_streams$distance_m)) / diff(c(0, data_streams$time / 60))},
    data_streams$time)
}

generate_activity_overview <- function(activity_list, date_range) {
  activity_df <- data.frame(matrix(ncol=8, nrow=0))
  colnames(activity_df) <- c(
    "activity_id", "name", "description", "start_date", "sport_type", "distance", "elapsed_time", "moving_time"
  )
  for (activity in activity_list) {
    activity_df[nrow(activity_df) + 1,] = c(
      activity$activity_id, activity$name,
      activity$description, paste(activity$start_date),
      activity$sport_type, activity$overview$distance,
      activity$overview$elapsed_time, activity$overview$moving_time
    )
  }

  activity_df <- activity_df |>
    dplyr::mutate(
      start_date = as_datetime(start_date),
      distance = as.numeric(distance),
      distance_m = distance,
      distance_yd = distance_m * 1.0936132983,
      distance_km = distance / 1000,
      distance_miles = distance_km * 0.6213711922,
      elapsed_time = as.numeric(elapsed_time),
      moving_time = as.numeric(moving_time)
    )


  return(activity_df)
}

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
