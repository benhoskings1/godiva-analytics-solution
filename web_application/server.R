base::source(v_global_session_path)

server <- function(input, output, session) {
  waiter::waiter_hide()

  # New Tab -----
  ## Define reactive values ------
  rv_personal_tab <- shiny::reactiveValues(
    week_data = f_get_athlete_week_data(activities_detailed, "38807221", c(today, today - lubridate::days(6)))
    # week_sessions =
  )

  ## Define reactive outputs ------
  output$activity_summary <- shiny::renderUI(
    tags$table(class="overview_table",
      tags$tr(
        tags$td(class="overview_data",
          tags$div(class="activity_container", style="gap: 10px",
            tags$div(style="grid-area: type", "Activities"),
            tags$div(style="grid-area: dist", base::nrow(rv_personal_tab$week_data)
            ),
            tags$div(style="grid-area: time", {
              total_time <- rv_personal_tab$week_data |>
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
              rv_personal_tab$week_data |>
                dplyr::filter(sport_type == "Run")
              )
            ),
            tags$div(style="grid-area: time", {
              total_time <- rv_personal_tab$week_data |>
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
    start_end <- input$activity_range_filter
    date_range <- (lubridate::as_date(start_end[2]) - lubridate::as_date(start_end[1]))

    first_monday <- lubridate::as_date(start_end[1]) - lubridate::wday(lubridate::as_date(start_end[1]), week_start=1)+1

    next_sunday <- base::Sys.Date()+(7-lubridate::wday(today, week_start=1))

    tags$table(class="calendar",
      tags$tr(
        purrr::map(names(week_days), function(name) tags$td(class="cal_head", name))
      ), {
      rows <- base::ceiling((today - first_monday) / 7)
      purrr::map(c(1:rows), function(row_idx) {
        tags$tr(class="calendar_row",
          purrr::map(1:7, function(col_idx) {
            cell_date <- (next_sunday - (rows-row_idx)*7 - (7-col_idx))
            activities <- rv_personal_tab$week_data |>
              dplyr::filter(lubridate::as_date(start_date) == cell_date)

            bg_colour <- if (!dplyr::between(cell_date, start_end[1], start_end[2])) {
              "#a9a9a9"
            } else if (nrow(activities) > 0) {
              "rgba(0,255,0,0.3);"
            } else {"rgba(228,228,228,0.3);"}

            tags$td(class="calendar_cell",
              style=sprintf("background-color: %s", bg_colour),
              tags$div(class="cal_date_num", {
                if (lubridate::day(cell_date) == 1) {
                  format(cell_date, "%d-%b")
                } else {format(cell_date, "%d")}
                }

              ),
              if(nrow(activities) > 0) {
                main <- activities |> dplyr::filter(elapsed_time == max(elapsed_time))

                print(get_sport_icon(main$sport_type))
                tags$div(class="activity_container",
                  tags$div(class="grid_item", style="grid-area: icon; font-size: 32px",
                    get_sport_icon(main$sport_type)
                  ),
                  tags$div(style="grid-area: type;", main$sport_type),
                  tags$div(style="grid-area: dist;", sprintf("%.2f km", main$distance/1000)),
                  tags$div(style="grid-area: time;",
                    format(
                      lubridate::as_datetime(lubridate::seconds(main$elapsed_time)), "%H:%M:%S"
                    )
                  ),
                )
              }
            )
          })
        )
      })
    })
  })

  ## Define observe events -----
  shiny::observeEvent(
    eventExpr = {
      input$athlete_filter
      input$activity_range_filter
      },
    handlerExpr = {
      rv_personal_tab$week_data <- f_get_athlete_week_data(
        activities_detailed,
        input$athlete_filter,
        input$activity_range_filter
      )

      output$activity_table <- renderText(HTML(f_create_activity_table(rv_personal_tab$week_data)))

      first_name <- member_data |>
        dplyr::filter(ID == input$athlete_filter) |>
        dplyr::pull(First_Name)
      last_name <- member_data |>
        dplyr::filter(ID == input$athlete_filter) |>
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
