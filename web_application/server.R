base::source(v_global_session_path)

server <- function(input, output, session) {
  waiter::waiter_hide()

  # authentication window -----------
  auth <- shiny::callModule(
    module = auth_server,
    id = "auth",
    check_credentials = shinymanager::check_credentials(df_credentials))

  shiny::observe({
   user_data <<- shiny::reactiveValuesToList(auth)$user_info
   print(user_data)
   if (!is.null(user_data)) {
     if (!is.na(user_data$permissions)) {
       athlete_data = athlete_db$find(sprintf('{"group": "%s"}', user_data$permissions))
       athlete_data <- athlete_data |>
         dplyr::mutate(full_name = paste(first_name, last_name, sep = " "))
       athlete_id_map <- athlete_data$strava_id
       names(athlete_id_map) <- athlete_data$full_name
         updateSelectInput(
         inputId = "athlete_filter",
         label = "Select Athelete",
         choices = athlete_id_map
       )
     }
   }
  })

  app_settings <- shiny::reactiveValues(
    units = list("Run"="imperial", "Swim"="metric", "Ride"="imperial"),
    vals = ""
  )

  reactive_vals <- shiny::reactiveValues(
    date_range = c(today -lubridate::weeks(10), today),
    athlete_activities = NULL,
    activity_overviews = NULL,
    selected_consult = NULL,
    name_string = "", age_string = "",

    graph_option_table = NULL,
    graph_options = list(),
    graph_option_table = NULL
  )

  graph_option_table <- reactive({
    tags$table(style="width: 100%",
      tags$tr(style="padding: 10px", {
        sport_types <- unique(reactive_vals$activity_overviews$sport_type)
        purrr::map(sport_types, function(sport_type) {
          tags$td(style="text-align: left; vertical-align: text-top; padding: 10px",
            tags$h3(sport_type, style="text-align: center"),

            shiny::checkboxGroupInput(
              inputId = glue("{stringr::str_to_lower(sport_type)}_graph_options"),
              choices=activity_graph_options[[sport_type]],
              selected = {
                # prev_selected <- input[[paste(stringr::str_to_lower(sport_type), "_graph_options")]]

                prev_selected <- reactive_vals$graph_options[[stringr::str_to_lower(sport_type)]]

                if (!is.null(prev_selected)) {
                  print("setting")
                  reactive_vals$graph_options[[stringr::str_to_lower(sport_type)]]
                } else {NULL}
              },
              label = NULL # Label is handled above
            )
          )
        })
      })
    )
  })

  ## Define observe events -----
  shiny::observeEvent(
    eventExpr = input$athlete_filter,
    handlerExpr = {

      db_query = consults_db$find(sprintf('{"athlete_id":%s}', input$athlete_filter))
      reactive_vals$athlete_activities = map_db_to_list(db_query)

      reactive_vals$activity_overviews <- generate_activity_overview(reactive_vals$athlete_activities)

      first_name <- athlete_data |>
        dplyr::filter(strava_id == input$athlete_filter) |>
        dplyr::pull(first_name)
      last_name <- athlete_data |>
        dplyr::filter(strava_id == input$athlete_filter) |>
        dplyr::pull(last_name)

      reactive_vals$name_string <- sprintf("<b>%s %s</b>", first_name, last_name)
      reactive_vals$age_string <- "<b>Age: </b>"

      reactive_vals$graph_options_table <- construct_graph_option_table(reactive_vals$activity_overviews)
    }
  )

  shiny::observeEvent(
    eventExpr = input$activity_range_filter,
    handlerExpr = {
      # athlete_data$display_activities <- f_get_athlete_week_data(
      #   activities_detailed,
      #   input$athlete_filter,
      #   input$activity_range_filter
      # )
    }
  )

  shiny::observeEvent(
    eventExpr = input$run_graph_options,
    handlerExpr = {
      reactive_vals$graph_options$run <- input$run_graph_options
    }
  )

  ### Display Settings tab --------------
  shiny::observeEvent(
    eventExpr = input$run_unit_input,
    handlerExpr = {app_settings$units$Run <- input$run_unit_input}
  )

  shiny::observeEvent(
    eventExpr = input$ride_unit_input,
    handlerExpr = {app_settings$units$Ride <- input$ride_unit_input}
  )

  shiny::observeEvent(
    eventExpr = input$swim_unit_input,
    handlerExpr = {
      if (input$swim_unit_input == "Miles") {
        app_settings$units$Swim <- "imperial"
      } else {
        app_settings$units$Swim <- "metric"
      }
    }
  )

  ## Define reactive outputs ------
  output$activity_summary <- shiny::renderUI({
    week_activities <- reactive_vals$activity_overviews |>
      dplyr::filter(dplyr::between(start_date, today- lubridate::days(7), today))

    # print(week_activities)

    construct_week_overview(week_activities)
    }
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

  output$team_table <- shiny::renderUI({
    construct_team_table(athlete_data, consults_db$find())
  })

  output$athlete_activity_calendar <- shiny::renderUI({
    generate_calendar(
      input$activity_range_filter,
      reactive_vals$activity_overviews,
      app_settings$units
    )
  })

  output$day_calendar <-shiny::renderUI({
    selected_day <- names(week_days)[base::as.numeric(input$activity_day_filter)]

    if (!is.na(selected_day)) {
      print(input$run_graph_options)

      tags$div(
        tags$br(style="height: 50px"),
        tags$div(
          graph_option_table()
        ),
        tags$br(style="height: 50px"),
        generate_calendar_2(
          input$activity_range_filter,
          input$activity_day_filter,
          reactive_vals$athlete_activities,
          reactive_vals$activity_overviews,
          reactive_vals$graph_options,
          app_settings$units
        )
      )
    }
  })

  output$activity_table <- renderText(HTML(f_create_activity_table(reactive_vals$athlete_activities)))
  output$name_string <- shiny::renderText(reactive_vals$name_string)
  output$age_string <- shiny::renderText(reactive_vals$age_string)


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
          # shiny::updateActionButton(session,
          #   sprintf("ab_scale_%d", num),
          #   style="background-color: #FF0000"
          # )
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
