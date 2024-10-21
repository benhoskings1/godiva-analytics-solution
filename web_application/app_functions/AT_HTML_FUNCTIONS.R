## Personal Tab -------
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
