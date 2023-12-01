base::source(v_global_session_path)

server <- function(input, output, session) {
  waiter::waiter_hide()

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
        dplyr::pull(First_Name)
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
