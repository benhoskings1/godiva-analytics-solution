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
f_pull_data <- function(data, day, cat) {
  activities <- data |>
    dplyr::filter(days_since == day)

  value <- if(nrow(activities) != 0) {
    activities |>
      dplyr::filter(elapsed_time == max(elapsed_time)) |>
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::pull(cat)
  } else {
    activities |>
      dplyr::pull(cat)
  }

  if(length(value) != 0) {
    switch(
      cat,
      "distance" = sprintf("%.2f km", value/1000),
      "elapsed_time" = format(
        lubridate::as_datetime(lubridate::seconds(value)),
        "%H:%M:%S"),
      value)
  } else " "
}

f_get_athlete_week_data <- function(all_data, athlete_id) {
  week_data <- all_data |>
    dplyr::filter(
      Athlete_ID == athlete_id,
      start_date > Sys.Date() - lubridate::days(6)) |>
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

f_create_activity_table <- function(week_data) {

  day_off <- "#a5c90f"; day_on <-"#D3D8DB";

  days <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
  icons <- c(as.character(icon("dumbbell", lib = "font-awesome")),
  as.character(icon("stopwatch", lib = "font-awesome")),
  as.character(icon("running", lib = "font-awesome")),
  as.character(icon("stopwatch", lib = "font-awesome")),
  as.character(icon("bed", lib = "font-awesome")),
  as.character(icon("stopwatch", lib = "font-awesome")),
  as.character(icon("running", lib = "font-awesome")))

  icon_list <- c(
    " " = "",
    "Workout" = as.character(icon("dumbbell", lib = "font-awesome")),
    "Training" = as.character(icon("stopwatch", lib = "font-awesome")),
    "Run" = as.character(icon("running", lib = "font-awesome")),
    "Stopwatch" = as.character(icon("stopwatch", lib = "font-awesome")),
    "Rest" = as.character(icon("bed", lib = "font-awesome")),
    "Ride" = as.character(icon("bicycle", lib = "font-awesome")),
    "MountainBikeRide" = as.character(icon("bicycle", lib = "font-awesome")),
    "Swim" = as.character(icon("person-swimming", lib = "font-awesome")),
    "Hike" = as.character(icon("person-hiking", lib = "font-awesome")),
    "Walk" = as.character(icon("person-walking", lib = "font-awesome")))

  day_start <- lubridate::wday(Sys.time(), week_start = getOption("lubridate.week.start", 1))

  active_days <- logical(7)
  active_days[unique(7 - week_data$days_since)] <- T

  htmlTable <- sprintf("
      <style>
      table {
        border-spacing: 30px;
        border-style: solid;
        width: 100%%;
      }

      th, td {
        text-align: center;
        border-style: solid;
      }

      tr {
        border-style: solid;
      }

      .type_container {
        grid-area: type;
        color: #3F3F3F; font-size: 12px;
        display: flex; justify-content: center; align-items: center;}

      .dist_container {
        grid-area: dist;
        color: #3F3F3F;
        font-size: 12px;
        display: flex; justify-content: center; align-items: center;}

      .time_container {
        grid-area: time;
        color: #3F3F3F;
        font-size: 12px;
        display: flex; justify-content: center; align-items: center;}

      .img_container {
        grid-area: image;
        font-size: 48px;
        display: flex; justify-content: center; align-items: center;}

      .activity_container {
        display: grid;
        grid-template-areas:
          'image image type'
          'image image dist'
          'image image time';
        gap: 10px;
        padding: 5px 0px;
      }
      </style>

      <body>
        <table>
          <tr>
            <th style='border-style: solid;'>%s</th>
            <th style='border-style: solid;'>%s</th>
            <th style='border-style: solid;'>%s</th>
            <th style='border-style: solid;'>%s</th>
            <th style='border-style: solid;'>%s</th>
            <th style='border-style: solid;'>%s</th>
            <th style='border-style: solid;'>%s</th>
          </tr>
          <tr>
            <td style='background-color:%s; border-style: solid;'>
              <div class='activity_container'>
                <div class='type_container'>%s</div>
                <div class='dist_container'>%s</div>
                <div class='time_container'>%s</div>
                <div class='img_container'>%s</div>
              </div>
            </td>
            <td style='background-color:%s; border-style: solid;'>
              <div class='activity_container'>
                <div class='type_container'>%s</div>
                <div class='dist_container'>%s</div>
                <div class='time_container'>%s</div>
                <div class='img_container'>%s</div>
              </div>
            </td>
            <td style='background-color:%s; border-style: solid;'>
              <div class='activity_container'>
                <div class='type_container'>%s</div>
                <div class='dist_container'>%s</div>
                <div class='time_container'>%s</div>
                <div class='img_container'>%s</div>
              </div>
            </td>
            <td style='background-color:%s; border-style: solid;'>
              <div class='activity_container'>
                <div class='type_container'>%s</div>
                <div class='dist_container'>%s</div>
                <div class='time_container'>%s</div>
                <div class='img_container'>%s</div>
              </div>
            </td>
            <td style='background-color:%s; border-style: solid;'>
              <div class='activity_container'>
                <div class='type_container'>%s</div>
                <div class='dist_container'>%s</div>
                <div class='time_container'>%s</div>
                <div class='img_container'>%s</div>
              </div>
            </td>
            <td style='background-color:%s; border-style: solid;'>
              <div class='activity_container'>
                <div class='type_container'>%s</div>
                <div class='dist_container'>%s</div>
                <div class='time_container'>%s</div>
                <div class='img_container'>%s</div>
              </div>
            </td>
            <td style='background-color:%s; border-style: solid;'>
              <div class='activity_container'>
                <div class='type_container'>%s</div>
                <div class='dist_container'>%s</div>
                <div class='time_container'>%s</div>
                <div class='img_container'>%s</div>
              </div>
            </td>
          </tr>
        </table>
      </body>
      ",
      days[((day_start) %% 7) + 1], days[((day_start+1) %% 7) + 1],
      days[((day_start+2) %% 7) + 1], days[((day_start+3) %% 7) + 1],
      days[((day_start+4) %% 7) + 1], days[((day_start+5) %% 7) + 1],
      days[((day_start+6) %% 7) + 1],

      if(active_days[1]) day_on else day_off,
      f_pull_data(week_data, 6, "sport_type"),  f_pull_data(week_data, 6, "distance"),
      f_pull_data(week_data, 6, "elapsed_time"), icon_list[f_pull_data(week_data, 6, "sport_type")],
      if(active_days[2]) day_on else day_off,
      f_pull_data(week_data, 5, "sport_type"), f_pull_data(week_data, 5, "distance"),
      f_pull_data(week_data, 5, "elapsed_time"), icon_list[f_pull_data(week_data, 5, "sport_type")],
      if(active_days[3]) day_on else day_off,
      f_pull_data(week_data, 4, "sport_type"), f_pull_data(week_data, 4, "distance"),
      f_pull_data(week_data, 4, "elapsed_time"), icon_list[f_pull_data(week_data, 4, "sport_type")],
      if(active_days[4]) day_on else day_off,
      f_pull_data(week_data, 3, "sport_type"), f_pull_data(week_data, 3, "distance"),
      f_pull_data(week_data, 3, "elapsed_time"), icon_list[f_pull_data(week_data, 3, "sport_type")],
      if(active_days[5]) day_on else day_off,
      f_pull_data(week_data, 2, "sport_type"), f_pull_data(week_data, 2, "distance"),
      f_pull_data(week_data, 2, "elapsed_time"), icon_list[f_pull_data(week_data, 2, "sport_type")],
      if(active_days[6]) day_on else day_off,
      f_pull_data(week_data, 1, "sport_type"), f_pull_data(week_data, 1, "distance"),
      f_pull_data(week_data, 1, "elapsed_time"), icon_list[f_pull_data(week_data, 1, "sport_type")],
      if(active_days[7]) day_on else day_off,
      f_pull_data(week_data, 0, "sport_type"), f_pull_data(week_data, 0, "distance"),
      f_pull_data(week_data, 0, "elapsed_time"), icon_list[f_pull_data(week_data, 0, "sport_type")])

  return(htmlTable)
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
