base::source(v_global_session_path)

server <- function(input, output, session) {
  # base::Sys.sleep(1)

  waiter::waiter_hide()

  # header screenshot --------
  shiny::observeEvent(
    eventExpr = {
      input$ab_screenshot
    },
    handlerExpr = {
      shinyscreenshot::screenshot()
    }
  )

  # authentication window -----------
  auth <- shiny::callModule(
    module = auth_server,
    id = "auth",
    check_credentials = shinymanager::check_credentials(df_credentials)
  )

  shiny::observe({
    user_data <<- shiny::reactiveValuesToList(auth)$user_info
    # print(shiny::reactiveValuesToList(auth))
  }

  )

  # usage tracking -------
  shiny::observe(
    {
      df_user_data <- shiny::reactiveValuesToList(auth)

      gl_df_tracking <<- f_track_connection(
        df_auth = df_user_data,
        df_tracking = gl_df_tracking,
        v_web_application = "Coventry_Running"
      )
    }
  )

  # header info button -------
  shiny::observeEvent(
    eventExpr = {
      input$ab_header_info
    },
    handlerExpr = {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Information",
        text = "I need information",
        type = "info"
      )
    }
  )

  # header log out button ------
  shiny::observeEvent(
    eventExpr = input$ab_logout,
    handlerExpr = session$reload()
  )

  # Admin tabs -------
  rv_tracking <- shiny::reactiveValues(
    df_tracking_per_user = plotly::plotly_empty(type = "scatter", mode = "markers") |>
      plotly::config(displayModeBar = F) |>
      plotly::layout(title = list(text = "Please select filters and evaluate!", yref="paper", y = 0.5)),

    df_tracking_log = base::data.frame(
      stringsAsFactors = F,
      "Information" = c("Please select filters and evaluate!")
    )
  )

  ## Admin Configuration ------
  shiny::observe(
    x = {
      v_user <- shiny::reactiveValuesToList(auth)$user_info

      if(!base::is.null(v_user)) {
        v_is_admin <- user_data$admin

        if(v_is_admin) {
          ## Append admin tab ------
          shiny::insertTab(
            inputId = "TabBox01",
            target = "Athlete_Overview",
            position = c("before"),
            shiny::tabPanel(
              title = tags$div(class="tab_title", "Team Overview"),
              shiny::wellPanel(
                id = "WP_Team_Overview",
                style = "background: white",

                tags$div(
                  purrr::map(member_data$ID, ~ {
                    f_create_team_overview(
                      member_data |>
                        dplyr::filter(ID == .x) |>
                        dplyr::pull(First_name),
                      member_data |>
                        dplyr::filter(ID == .x) |>
                        dplyr::pull(Last_Name),
                      20, 50
                    )
                  })
                )


              )
            )
          )


          shiny::appendTab(
            inputId = "TabBox01",
            shiny::tabPanel(
              title = tags$div(class="tab_title", "Admin Tab"),
              shiny::wellPanel(
                id = "WP_Admin",
                style = "background: white",
                shiny::navlistPanel(
                  id = "NP_Admin",
                  well = F,
                  widths = c(2, 10),

                  shiny::tabPanel(
                    title = "Session Input",

                    shiny::h2("Input session details here!"),
                    shiny::fluidRow(
                      shiny::column(
                        width = 6,
                        shiny::dateInput(
                          inputId = "di_activity_date",
                          label = "Date of Session",
                          value = Sys.Date(),
                          format = "D d MM"
                        ),
                        shiny::selectInput(
                          inputId = "si_session_type",
                          label = "Session Type",
                          choices = c("Reps"="R", "Pyramid"="P"),
                          selected = c("Reps"="R")
                        ),
                        shiny::numericInput(
                          inputId = "ni_rep_dist",
                          label = "Rep Distance",
                          value = 1000,
                          min=100
                        ),
                        shiny::fluidRow(
                          shiny::column(
                            width = 6,
                            shiny::numericInput(
                              inputId = "ni_set_count",
                              label = "Set Count",
                              value = 1,
                              min=1
                            )
                          ),
                          shiny::column(
                            width = 6,
                            shiny::numericInput(
                              inputId = "ni_set_rest",
                              label = "Set Rest (seconds)",
                              value = 0,
                              min=1
                            )
                          )
                        ),
                        shiny::fluidRow(
                          shiny::column(
                            width = 6,
                            shiny::numericInput(
                              inputId = "ni_rep_count",
                              label = "Rep Count",
                              value = 6,
                              min = 1
                            )
                          ),
                          shiny::column(
                            width = 6,
                            shiny::numericInput(
                              inputId = "ni_rep_rest",
                              label = "Rep Rest (seconds)",
                              value = 60,
                              min=1
                            )
                          )
                        )
                      ),
                      shiny::column(
                        width=6,
                        checkboxGroupInput(
                          "checkGroup",
                          label = "Athlete Select",
                          choices = {
                            names(member_data$App_Username) <- member_data$ID
                          }
                        )
                      )
                    ),

                    shiny::br(),
                    shiny::h4("Select who attended!")
                  ),

                  shiny::tabPanel(
                    title = "Second Tab",
                    icon = shiny::icon("database"),

                    shiny::h2("What data is used in this app?")
                  )
                )
              )
            )
          )

          ## Add button to set as default ----
          output$session_buttons <- shiny::renderUI(
            shiny::fluidRow(
              shiny::column(
                width=6,
                shiny::actionButton(
                  inputId = "ab_submit_session",
                  label = "Submit Session"
                )
              ),
              shiny::column(
                width=6,
                shiny::actionButton(
                  inputId = "ab_session_default",
                  label = "Set As Default"
                )
              )
            )
          )

        } else {
          output$session_buttons <- shiny::renderUI(
            shiny::fluidRow(
              shiny::column(
                width=12,
                shiny::actionButton(
                  inputId = "ab_submit_session",
                  label = "Submit Session"
                )
              )
            )
          )

          updateSelectInput(
            session, "si_person_filter",
            label = "Select Athelete",
            choices = c(user_data$strava_id)
          )
        }
      }
    }
  )

  shiny::observeEvent(
    eventExpr = input$ab_evaluate_user_traffic,
    handlerExpr = {
      rv_tracking$df_tracking_per_user <- f_create_data_connections_per_user(
        l_timerange = input$dri_info_user_traffic,
        l_users = input$si_web_application_users
      )

      rv_tracking$df_tracking_log <- f_create_tracking_log(
        l_timerange = input$dri_info_user_traffic,
        l_users = input$si_web_application_users
      )
    }
  )

  ## User Traffic Plots ------
  output$plo_plot_connections_per_user <- plotly::renderPlotly(
    expr = rv_tracking$df_tracking_per_user
  )

  output$rh_table_log_connections <- rhandsontable::renderRHandsontable(
    expr = rhandsontable::rhandsontable(rv_tracking$df_tracking_log)
  )

  ## Table with credentials -----
  output$rh_table_user_logins <- rhandsontable::renderRHandsontable(
    expr = rhandsontable::rhandsontable(data = f_load_credentials())
  )

  # Save Credentials -----
  shiny::observeEvent(
    eventExpr = input$ab_user_admin_save_login,
    handlerExpr = f_save_credentials(
      rhandsontable::hot_to_r(input$rh_table_user_logins)
    )
  )

  # New Tab -----
  ## Define reactive values ------
  rv_personal_tab <- shiny::reactiveValues(
    week_data = f_get_athlete_week_data(activities_detailed, "38807221")
    # week_sessions =
  )
  ## Define reactive outputs ------

  output$activity_summary <- shiny::renderUI(
    tags$table(
      tags$td(
        style="border: 2px solid; text-align: center;",
        tags$div(
          class="activity_container",
          tags$div(class="type_container",
            "Activities"
          ),
          tags$div(class="dist_container",
            base::nrow(rv_personal_tab$week_data)
          ),
          tags$div(class="time_container", {
            total_time <- rv_personal_tab$week_data |>
              dplyr::pull(elapsed_time) |>
              base::sum()

              format(
                lubridate::as_datetime(
                  lubridate::seconds(total_time)),
               "%H:%M:%S")
            }
          ),
          tags$div(class="img_container",
            icon("dumbbell", lib = "font-awesome")
          )
        )
      ),
      tags$td(
        style="border: 2px solid; text-align: center;",
        tags$div(
          class="activity_container",
          tags$div(class="type_container",
                   "Runs"
          ),
          tags$div(class="dist_container",
                   base::nrow(rv_personal_tab$week_data |>
                                dplyr::filter(sport_type == "Run"))
          ),
          tags$div(class="time_container", {
            total_time <- rv_personal_tab$week_data |>
              dplyr::filter(sport_type == "Run") |>
              dplyr::pull(elapsed_time) |>
              base::sum()

            format(
              lubridate::as_datetime(
                lubridate::seconds(total_time)),
              "%H:%M:%S")
          }
          ),
          tags$div(class="img_container",
                   icon("running", lib = "font-awesome")
          )
        )
      )
    )

  )

  ## Define observe events -----
  shiny::observeEvent(
    eventExpr = input$si_person_filter,
    handlerExpr = {
      rv_personal_tab$week_data <- f_get_athlete_week_data(
        activities_detailed,
        input$si_person_filter
      )

      output$im_profile_pic <- shiny::renderUI({

        if(base::file.exists(base::paste("www/profile_pic_", input$si_person_filter, ".png", sep=""))) {

          tags$img(
            src = base::paste("profile_pic_", input$si_person_filter, ".png", sep=""),
            style = "max-width: 200px; border-radius: 50%;"
          )

        } else {
          tags$img(
            src = "profile_pic_empty.png",
            style = "max-height: 200px;"
          )
        }}
      )

      output$activity_table <- renderText(HTML(f_create_activity_table(rv_personal_tab$week_data)))

      first_name <- member_data |>
        dplyr::filter(ID == input$si_person_filter) |>
        dplyr::pull(Name)
      last_name <- member_data |>
        dplyr::filter(ID == input$si_person_filter) |>
        dplyr::pull(Last_Name)

      output$name_string <- shiny::renderText(
        sprintf("<b>%s %s</b>", first_name, last_name)
      )
      output$age_string <- shiny::renderText("<b>Age: </b>")

      updateSelectInput(
        session, "si_day_filter",
        label = "Select Day",
        choices = {
          value_options <- unique(rv_personal_tab$week_data$days_since)
          day_options <- unique(lubridate::as_date(rv_personal_tab$week_data$start_date))
          day_names <- format(day_options, format="%a %d %b")
          names(value_options) <- day_names
          sort(value_options)
        }
      )
    }

  )

  shiny::observeEvent(
    eventExpr = input$si_day_filter,
    handlerExpr = {
      output$day_table <- DT::renderDataTable(
        rv_personal_tab$week_data |>
          dplyr::filter(days_since == input$si_day_filter),
        # options = list(scrollX = TRUE),
        rownames = FALSE)
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
      gl_df_tracking <<- f_track_disconnection(gl_df_tracking)

      f_save_tracking(gl_df_tracking)

      base::rm(list = base::ls())

      readr::write_tsv(session_record_all, base::paste(v_prepared_data_path, "session_data.tsv", sep=""))
    }
  )
}
# end server function -------
