base::source(v_global_session_path)

server <- function(input, output, session) {
  waiter::waiter_hide()

  app_settings <- shiny::reactiveValues(
    units = c("Run"="imperial", "Swim"="metric"),
    vals = ""
  )

  # New Tab -----
  ## Define reactive values ------
  rv_personal_tab <- shiny::reactiveValues(
    week_data = f_get_athlete_week_data(activities_detailed, "38807221", c(today, today - lubridate::days(6)))
    # week_sessions =
  )

  athlete_data <- shiny::reactiveValues(
    week_activities = f_get_athlete_week_data(activities_detailed, "38807221", c(today, today - lubridate::days(6))),
    display_activities = f_get_athlete_week_data(activities_detailed, "38807221", c(today, today - lubridate::days(6))),
    name_string = "", age_string = ""
  )

  ## Define reactive outputs ------
  output$activity_summary <- shiny::renderUI(
    tags$table(class="overview_table",
      tags$tr(
        tags$td(class="overview_data",
          tags$div(class="activity_container", style="gap: 10px",
            tags$div(style="grid-area: type", "Activities"),
            tags$div(style="grid-area: dist", base::nrow(athlete_data$week_activities)
            ),
            tags$div(style="grid-area: time", {
              total_time <- athlete_data$week_activities |>
                dplyr::pull(elapsed_time) |>
                base::sum()
              format(lubridate::as_datetime(
                lubridate::seconds(total_time)), "%H:%M:%S"
              )
            }),
            tags$div(style="grid-area: icon; font-size: 50px", get_sport_icon("Workout")
            )
          )
        ),
        tags$td(class="overview_data",
          tags$div(class="activity_container", style="gap: 10px",
            tags$div(style="grid-area: type", "Runs"),
            tags$div(style="grid-area: dist", base::nrow(
              athlete_data$week_activities |>
                dplyr::filter(sport_type == "Run")
              )
            ),
            tags$div(style="grid-area: time", {
              total_time <- athlete_data$week_activities |>
                dplyr::filter(sport_type == "Run") |>
                dplyr::pull(elapsed_time) |>
                base::sum()
              format(lubridate::as_datetime(
                lubridate::seconds(total_time)), "%H:%M:%S"
              )
            }),
            tags$div(style="grid-area: icon; font-size: 50px", get_sport_icon("Run")
            )
          )
        )
      )
    )
  )

  output$im_profile_pic <- shiny::renderUI({
    if(base::file.exists(base::paste("www/profile_pic_", input$athlete_filter, ".png", sep=""))) {
      tags$img(class="profile_pic",
        src = base::paste("profile_pic_", input$athlete_filter, ".png", sep="")
      )
    } else {
      tags$img(class="profile_pic",
        src = "profile_pic_empty.png"
      )
    }}
  )

  output$athlete_activity_calendar <- shiny::renderUI({
    generate_calendar(
      input$activity_range_filter,
      athlete_data$display_activities,
      app_settings$units
    )
  })

  output$day_calendar <-shiny::renderUI({
    selected_day <- names(week_days)[base::as.numeric(input$activity_day_filter)]

    if (!is.na(selected_day)) {
      tags$div(
        tags$br(style="height: 50px"),
        generate_calendar_2(
          input$activity_range_filter,
          input$activity_day_filter,
          athlete_data$display_activities,
          app_settings$units
        )
      )
    }
  })

  output$activity_table <- renderText(HTML(f_create_activity_table(athlete_data$display_activities)))
  output$name_string <- shiny::renderText(athlete_data$name_string)
  output$age_string <- shiny::renderText(athlete_data$age_string)

  ## Define observe events -----
  shiny::observeEvent(
    eventExpr = input$athlete_filter,
    handlerExpr = {
      athlete_data$week_activities <- f_get_athlete_week_data(
        activities_detailed,
        input$athlete_filter,
        c(today - lubridate::days(6), today)
      )

      athlete_data$display_activities <- f_get_athlete_week_data(
        activities_detailed,
        input$athlete_filter,
        input$activity_range_filter
      )

      first_name <- member_data |>
        dplyr::filter(ID == input$athlete_filter) |>
        dplyr::pull(First_Name)
      last_name <- member_data |>
        dplyr::filter(ID == input$athlete_filter) |>
        dplyr::pull(Last_Name)

      athlete_data$name_string <- sprintf("<b>%s %s</b>", first_name, last_name)
      athlete_data$age_string <- "<b>Age: </b>"
    }
  )

  shiny::observeEvent(
    eventExpr = input$activity_range_filter,
    handlerExpr = {
      athlete_data$display_activities <- f_get_athlete_week_data(
        activities_detailed,
        input$athlete_filter,
        input$activity_range_filter
      )
    }
  )
  # Activity Recording Tab ----
  set_names <- reactive(paste0("set", seq_len(input$ni_set_count)))

  output$sets <- renderUI({
    shiny::fluidRow(
      purrr::map(set_names(), ~ {

        rep_names <- paste0(.x, "_rep", seq_len(input$ni_rep_count))

        shiny::fluidRow(
          shiny::h3(paste("Set", sub('.*set', '', .x))),
          purrr::map(rep_names, ~ {
            font_size_options = c("1"=18, "2"=18)
            col_width <- max(1, min(c(2, floor(12/input$ni_rep_count))))
            font_size <- font_size_options[paste0(col_width)]

            shiny::column(
              width=col_width,
              tags$style(
                sprintf(
                  "#%s {font-size:%dpx;padding: 10px 0px; text-align: center;
                  display: flex; justify-content: center; align-items: center;}",
                  .x, font_size
                )
              ),
            shiny::numericInput(
              .x,
              value = NA,
              label = ""
              )
            )
          })
        )
      })
    )
  })

  output$activity_rating <- shiny::renderText("unset")

  shiny::observe(
    purrr::map(0:10, function(num) {
      shiny::observeEvent(
        input[[sprintf("ab_scale_%d", num)]], {
          shiny::updateActionButton(
            sprintf("ab_scale_%d", num),
            style="background-color: #FF0000"
          )
          output$activity_rating <- shiny::renderText(num)
        }
      )

    })
  )



  shiny::observeEvent(
    input$ab_submit_session,
    handlerExpr = {
      input_tags <- map(set_names(), ~ paste0(.x, "_rep", seq_len(input$ni_rep_count)))
      values <- map_chr(unlist(input_tags), ~ paste(input[[.x]], collapse = ","))

      session_record <- base::data.frame(
        Athlete_ID = user_data$strava_id,
        Date = input$di_activity_date,
        Session_Type = input$si_session_type,
        Session_Sets = input$ni_set_count,
        Session_Reps = input$ni_rep_count,
        Session_Timings = paste(values, collapse = ",")
      )

      session_record_all <<- rbind(session_record_all, session_record)

    }
  )

  shiny::onStop(
    session = session,
    fun <- function() {
      base::rm(list = base::ls())

      readr::write_tsv(session_record_all, base::paste(v_prepared_data_path, "session_data.tsv", sep=""))
    }
  )
}
# end server function -------
