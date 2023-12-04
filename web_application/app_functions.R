
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
    ) |>
    dplyr::select(
      title, description, distance, elapsed_time, start_date, days_since,
      sport_type, total_elevation_gain
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

generate_calendar <- function(start_end, all_activities) {
  date_range <- (lubridate::as_date(start_end[2]) - lubridate::as_date(start_end[1]))
  first_monday <- lubridate::as_date(start_end[1]) - lubridate::wday(lubridate::as_date(start_end[1]), week_start=1)+1
  next_sunday <- today+(7-lubridate::wday(today, week_start=1))

  tags$table(class="calendar",
    tags$tr(purrr::map(names(week_days), function(name)
      tags$td(class = "cal_head", name)
      )
    ), {
    rows <- base::ceiling((today - first_monday) / 7)
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
            print(get_sport_icon(main$sport_type))
            tags$div(
              class = "activity_container",
              tags$div(
                class = "grid_item",
                style = "grid-area: icon; font-size: 32px",
                get_sport_icon(main$sport_type)
              ),
              tags$div(style = "grid-area: type;", main$sport_type),
              tags$div(style = "grid-area: dist;", sprintf("%.2f km", main$distance /
                                                             1000)),
              tags$div(style = "grid-area: time;",
                 format(
                   lubridate::as_datetime(lubridate::seconds(main$elapsed_time)),
                   "%H:%M:%S"
                 )
               ),
            )
          })
        })
      )
    })
  })
}

# shiny::appendTab(
#   inputId = "NP_Info",
#
#   tab = shiny::tabPanel(
#     title = "User Administration",
#     icon = shiny::icon(name = "users"),
#
#     shiny::wellPanel(
#       shiny::h2("User Administration")
#     ),
#
#     shinycssloaders::withSpinner(
#       proxy.height = "200px",
#       ui_element = rhandsontable::rHandsontableOutput(
#         outputId = "rh_table_user_logins"
#       )
#     ),
#
#     shiny::hr(style="border=color: #9E9E9E"),
#
#     shiny::actionButton(
#       inputId = "ab_user_admin_save_login",
#       label = "Save Login Data",
#       icon = shiny::icon("save")
#     )
#   )
# )

# shiny::appendTab(
#   inputId = "NP_Info",
#
#   tab = shiny::tabPanel(
#     title = "User Traffic",
#     icon = shiny::icon("mouse-pointer"),
#
#     shiny::wellPanel(
#       shiny::h2("User Traffic"),
#
#       shiny::fluidRow(
#         shiny::column(
#           width = 6,
#           align = "center",
#
#           shiny::dateRangeInput(
#             inputId = "dri_info_user_traffic",
#             label = "Time Range for Connections",
#             start = gl_min_date,
#             end = base::Sys.Date(),
#             min = gl_min_date,
#             max = base::Sys.Date(),
#             separator = "until",
#             weekstart = 1,
#             format = "dd/mm/yyyy"
#           )
#         ),
#         shiny::column(
#           width = 6,
#           align = "center",
#           shiny::selectInput(
#             inputId = "si_web_application_users",
#             label = "Web Application Users",
#             choices = gl_l_users,
#             selected = gl_l_users[1],
#             multiple = T
#           )
#         )
#       ),
#
#       shiny::fluidRow(
#         align = "center",
#         shiny::actionButton(
#           inputId = "ab_evaluate_usee_traffic",
#           label = "Evaluate User Traffic",
#           icon = shiny::icon("play")
#         )
#       )
#     ), # end well panel
#
#     shiny::h3("Connections per User"),
#
#     shinycssloaders::withSpinner(
#       proxy.height = "200px",
#       ui_element = plotly::plotlyOutput(
#         outputId = "plo_plot_connections_per_user"
#       )
#     ),
#
#     shiny::hr(style="border-color: #9E9E9E"),
#
#     shiny::h3("Connection Log"),
#
#     shinycssloaders::withSpinner(
#       proxy.height = "200px",
#       ui_element = rhandsontable::rHandsontableOutput(
#         outputId = "rh_table_log_connections"
#       )
#     )
#   )
# )
