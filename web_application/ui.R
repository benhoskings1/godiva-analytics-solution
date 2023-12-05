base::options(spinner.color = v_main_colour)
base::options(spinner.type = 4)

ui <- shiny::fluidPage(
  title = "",
  theme = shinythemes::shinytheme("flatly"),

  shinyWidgets::setBackgroundColor("#F5F9FC"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app_style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "layouts.css"),
    tags$script(src="https://strava-embeds.com/embed.js")
  ),

  waiter::useWaiter(),
  waiter::waiter_show_on_load(
    html = html_loading_screen,
    color = v_main_colour
  ),

  shinydisconnect::disconnectMessage(
    text = "The app stopped working??"
  ),

  shiny::fluidRow(
    shinydashboard::tabBox(
      title = "",
      width = 12,
      id = "TabBox01",

      shiny::tabPanel(
        title = tags$div(class="tab_title", "Team Overview"),
        shiny::wellPanel(
          id = "WP_Team_Overview",
          style = "background: #FFFFFF",

          tags$div(class="data_container",
            tags$div(class="grid_title", "Team Overview")
          ),
          tags$br(style="height: 50px"),

          ## Team Table ------
          tags$div(class="data_container",
            tags$div(class="grid_title", "Team Summary"),
            tags$table(class="team_table", {
              rows <- base::ceiling(base::nrow(member_data) / 4)
              purrr::map(c(1:rows), function(row_idx) {
                tags$tr(class="team_row",
                  purrr::map(1:4, function(col_idx) {
                    if (((row_idx-1)*4 + col_idx) <= base::nrow((member_data))) {
                      member_id <- member_data$ID[(row_idx-1)*4 + col_idx]
                      tags$td(class="team_data",
                        f_create_team_overview(
                          member_data |>
                            dplyr::filter(ID == member_id) |>
                            dplyr::pull(First_Name),
                          member_data |>
                            dplyr::filter(ID == member_id) |>
                            dplyr::pull(Last_Name),
                          activities_detailed |>
                            dplyr::filter(Athlete_ID == member_id) |>
                            dplyr::filter(start_date > Sys.Date() - lubridate::days(6)) |>
                            dplyr::filter(sport_type == "Run") |>
                            dplyr::pull(distance) |>
                            sum() / 1000,
                          activities_detailed |>
                            dplyr::filter(Athlete_ID == member_id) |>
                            dplyr::filter(start_date > Sys.Date() - lubridate::days(30)) |>
                            dplyr::filter(sport_type == "Run") |>
                            dplyr::pull(distance) |>
                            sum() / 1000
                        )
                      )
                    }
                  })
                )
              })}
            )
          )
          # create a 4xY grid of athlete info
        )
      ),

      shiny::tabPanel(
        title = tags$div(class="tab_title", "Athlete Overview"),
        value = "Athlete_Overview",
        shiny::wellPanel(
          style = "background: white;",
          tags$div(class="profile_layout",
            tags$div(class="data_container",
              tags$div(style="grid-area: sidebar",
                shiny::selectInput(
                  inputId = "athlete_filter",
                  label = "Select Athelete",
                  choices = unique(activities_detailed$Athlete_ID)
                ),
                shiny::dateRangeInput(
                  inputId = "activity_range_filter",
                  label = "Date range",
                  format = "dd-M",
                  start = today - lubridate::days(20),
                  end   = today, weekstart = 1
                ),
                shiny::selectInput(
                  inputId = "activity_day_filter",
                  label = "Day Filter",
                  choices = c("None"=NA, week_days),
                  selected = 1
                ),
                shiny::selectInput(
                  inputId = "activity_type_filter",
                  label = "Activity Type",
                  choices = c("None"=NA, "Run")
                )
              )
            ),
            tags$div(class="data_container",
              tags$div(class="grid_item", style="height: 100%;",
                tags$div(class="profile_overview", style="height: 100%",
                  tags$div(class="grid_item", style="grid-area: profile_pic",
                    shiny::uiOutput("im_profile_pic")
                  ),
                  tags$div(style="grid-area: activity_overview",
                    tags$div("Weekly Summary"),
                    shiny::uiOutput("activity_summary")
                  ),
                  tags$div(class="grid_title", style="grid-area: name; font-size: 24px;",
                    shiny::htmlOutput("name_string")
                  ),
                  tags$div(class="grid_item", style="grid-area: age;", "Age: 21"),
                  tags$div(class="grid_item", style="grid-area: event;", "Event: 800m"),
                )
              )

            )
          ),
          tags$br(style="height: 50px"),
          tags$div(class="data_container",
            shiny::uiOutput("athlete_activity_calendar"),
            tags$br(style="height: 50px"),
            shiny::uiOutput("day_calendar")
          ),
          tags$br(),
          tags$div(class="data_container",
            # HTML('<div class="strava-embed-placeholder" data-embed-type="activity" data-embed-id="10331455741"></div><script src="https://strava-embeds.com/embed.js"></script>')
          )
        )
      ),

      # Activity Input Tab -------
      shiny::tabPanel(
        title = tags$div(class="tab_title", "Record Activity"),
        shiny::wellPanel(
          id = "WP_Activity",
          style = "background: white;",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 5,
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
              ),
              shiny::uiOutput("session_buttons")
            ),
            shiny::mainPanel(
              width = 7,
              style = "padding: 0px 40px;",
              shiny::uiOutput("sets"),
              shiny::fluidRow(
                textOutput("palette")
              )
            )
          ),
          shiny::textAreaInput(
            inputId = "ti_activity_overview",
            label = "How was your session?"
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              purrr::map(0:10, function(num) {
                shiny::actionButton(
                  inputId = sprintf("ab_scale_%d", num),
                  label = num
                )
              })
            )
          ),
          shiny::textOutput("activity_rating")
        )
      )
    )
  )
)


# shiny::appendTab(
#   inputId = "TabBox01",
#   shiny::tabPanel(
#     title = tags$div(class="tab_title", "Admin Tab"),
#     shiny::wellPanel(
#       id = "WP_Admin",
#       style = "background: white",
#       shiny::navlistPanel(
#         id = "NP_Admin",
#         well = F,
#         widths = c(2, 10),
#
#         shiny::tabPanel(
#           title = "Session Input",
#
#           shiny::h2("Input session details here!"),
#           shiny::fluidRow(
#             shiny::column(
#               width = 6,
#               shiny::dateInput(
#                 inputId = "di_activity_date",
#                 label = "Date of Session",
#                 value = Sys.Date(),
#                 format = "D d MM"
#               ),
#               shiny::selectInput(
#                 inputId = "si_session_type",
#                 label = "Session Type",
#                 choices = c("Reps"="R", "Pyramid"="P"),
#                 selected = c("Reps"="R")
#               ),
#               shiny::numericInput(
#                 inputId = "ni_rep_dist",
#                 label = "Rep Distance",
#                 value = 1000,
#                 min=100
#               ),
#               shiny::fluidRow(
#                 shiny::column(
#                   width = 6,
#                   shiny::numericInput(
#                     inputId = "ni_set_count",
#                     label = "Set Count",
#                     value = 1,
#                     min=1
#                   )
#                 ),
#                 shiny::column(
#                   width = 6,
#                   shiny::numericInput(
#                     inputId = "ni_set_rest",
#                     label = "Set Rest (seconds)",
#                     value = 0,
#                     min=1
#                   )
#                 )
#               ),
#               shiny::fluidRow(
#                 shiny::column(
#                   width = 6,
#                   shiny::numericInput(
#                     inputId = "ni_rep_count",
#                     label = "Rep Count",
#                     value = 6,
#                     min = 1
#                   )
#                 ),
#                 shiny::column(
#                   width = 6,
#                   shiny::numericInput(
#                     inputId = "ni_rep_rest",
#                     label = "Rep Rest (seconds)",
#                     value = 60,
#                     min=1
#                   )
#                 )
#               )
#             ),
#             shiny::column(
#               width=6,
#               checkboxGroupInput(
#                 "checkGroup",
#                 label = "Athlete Select",
#                 choices = {
#                   names(member_data$Username) <- member_data$ID
#                 }
#               )
#             )
#           ),
#
#           shiny::br(),
#           shiny::h4("Select who attended!")
#         ),
#
#         shiny::tabPanel(
#           title = "Second Tab",
#           icon = shiny::icon("database"),
#
#           shiny::h2("What data is used in this app?")
#         )
#       )
#     )
#   )
# )
#
# ## Add button to set as default ----
# output$session_buttons <- shiny::renderUI(
#   shiny::fluidRow(
#     shiny::column(
#       width=6,
#       shiny::actionButton(
#         inputId = "ab_submit_session",
#         label = "Submit Session"
#       )
#     ),
#     shiny::column(
#       width=6,
#       shiny::actionButton(
#         inputId = "ab_session_default",
#         label = "Set As Default"
#       )
#     )
#   )
# )
#
# } else {
#   output$session_buttons <- shiny::renderUI(
#     shiny::fluidRow(
#       shiny::column(
#         width=12,
#         shiny::actionButton(
#           inputId = "ab_submit_session",
#           label = "Submit Session"
#         )
#       )
#     )
#   )
#
#   updateSelectInput(
#     session, "si_person_filter",
#     label = "Select Athelete",
#     choices = c(user_data$strava_id)
#   )
# }
# }
# }
# )
