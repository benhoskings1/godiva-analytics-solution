base::options(spinner.color = v_main_colour)
base::options(spinner.type = 4)

ui <- shiny::fluidPage(
  waiter::useWaiter(),

  waiter::waiter_show_on_load(
    html = html_loading_screen,
    color = v_main_colour
  ),

  shinydisconnect::disconnectMessage(
    text = "The app stopped working??"
  ),

  # shinyBS:::shinyBSDep,
  shinyWidgets::useSweetAlert(),
  htmlwidgets::getDependency("sparkline"),
  shinytoastr::useToastr(),
  shinyFeedback::useShinyFeedback(),
  shinyWidgets::setBackgroundColor("#F5F9FC"),
  setShadow(id = "TabBox01"),

  title = "",
  theme = shinythemes::shinytheme("flatly"),


  tags$style("
    @import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);

    table, th, td {
      border: 2px solid; text-align: center;
    }

    well {
      border-width:10px;
    }

    .tab_title {
      color: #3F3F3F;
    }

    .type_container {
      grid-area: type;
      color: #3F3F3F; font-size: 12px; padding: 10px;
      display: flex; justify-content: center; align-items: center;}

    .dist_container {
      grid-area: dist;
      color: #3F3F3F;
      font-size: 12px; padding: 10px;
      display: flex; justify-content: center; align-items: center;}

    .time_container {
      grid-area: time;
      color: #3F3F3F;
      font-size: 12px; padding: 10px;
      display: flex; justify-content: center; align-items: center;}

    .img_container {
      grid-area: image;
      font-size: 48px; padding: 10px;
      display: flex; justify-content: center; align-items: center;}

    .activity_container {
      display: grid;
      grid-template-areas:
        'image image type'
        'image image dist'
        'image image time';
    }

    .profile_pic_container {
      grid-area: profile_pic;
    }

    .activity_overview_container {
      grid-area: activity_overview;
    }

    .profile_container {
      display: grid;
      grid-template-areas:
        'profile_pic activity_overview activity_overview'
        'profile_pic activity_overview activity_overview';
    }
    "),

  # Authentication

  shinymanager::auth_ui(

    id = "auth",
    tags_top = tags$div(
      tags$h4("Coventry Godiva Athelete Portal", style="align:center"),
      tags$img(
        src = "Logo.png",
        width = 100
      )
    ),
    tags_botton = tags$div(
      tags$p(
        "For any questions, please contact ",
        tags$a(
          href = "mailto:benjaminhoskings@gmail.com",
          target = "_top", "administator"
        )
      )
    ),
    choose_language = F
  ),

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width=6,
        shiny::br(),
        shiny::p(shiny::strong("Athelete Tracking", style="font-size:50px;")),
        shiny::span("Roberts Endurance Performace Squad", style="font-size:25px; color:grey80")
      ),

      shiny::column(
        width=2,
        align = "right",
        shiny::br(),
      ),

      shiny::column(
        width = 2,
        align = "left",
        shiny::br()
        # shiny::img(
        #   src="data/Logo.png",
        #   height = 60,
        #   width = 160,
        #   style = "padding-bottom:1px margin-bottom:1px"
        # )
      ),

      shiny::column(
        width = 2,
        align = "middle",

        shiny::fluidRow(
          shiny::br(style = "line-height:50px"),
          shiny::actionButton(
            inputId = "ab_header_info",
            label = "",
            icon = shiny::icon("envelope")
          ),
          shiny::actionButton(
            inputId = "ab_email",
            label = "",
            icon = shiny::icon("envelope")
          ),
          shiny::actionButton(
            inputId = "ab_screenshot",
            label = "",
            icon = shiny::icon("camera")
          ),
          shiny::actionButton(
            inputId = "ab_logout",
            label = "",
            icon = shiny::icon("sign-out-alt")
          )
        ),
        shiny::fluidRow(
          shiny::strong("Last Data Update: ", v_last_data_update)
        )
      )
    ),
    shiny::br()
  ),



  shiny::fluidRow(
    shinydashboard::tabBox(
      id = "TabBox01",
      selected = NULL,
      title = "",
      width = 12,

      # Tab No. 1 ----------
      shiny::tabPanel(
        title = tags$div(class="tab_title", "Info"),

        shiny::wellPanel(
          id = "WP_Info",
          style = "background: white",

          shiny::navlistPanel(
            id = "NP_Info",
            well = F,
            widths = c(2, 10),

            shiny::tabPanel(
              title = "Content",
              icon = shiny::icon("eye"),

              shiny::h2("Welcome!"),
              shiny::br(),
              shiny::p("What is the use of this application"),
              shiny::br(),
              shiny::p("Thanks for visiting"),
              shiny::br(),
              shiny::p("Best Regards from the project team"),
              shiny::br(),
              shiny::p(
                "Ideas and feedback are welcome!",
                shiny::br(),
                "Please contact: benjaminhoskings@gmail.com"
              )
            ),

            shiny::tabPanel(
              title = "Used Data",
              icon = shiny::icon("database"),

              shiny::h2("What data is used in this app?")
            )
          )
        )
      ),

      # Personal Tab -------
      shiny::tabPanel(
        title = "Personal Tab",
        shiny::wellPanel(
          id = "WP_Personal",
          style = "background: white",

          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 2,
              shiny::selectInput(
                inputId = "si_person_filter",
                label = "Select Athelete",
                choices = unique(activities_detailed$Athlete_ID)
              ),
              shiny::selectInput(
                inputId = "si_day_filter",
                label = "Select Day",
                choices = c("Monday")
              )
            ),

            shiny::mainPanel(
              width = 10,
              shiny::fluidRow(
                width = 12,
                ## Add profile section -------
                tags$div(
                  style="padding:15px;",
                  class="profile_container",
                  tags$div(
                    class="profile_pic_container",
                    shiny::uiOutput("im_profile_pic"),
                    tags$div(
                      shiny::htmlOutput("name_string"),
                      style="padding: 10px; display: flex;"
                    )

                  ),
                  tags$div(
                    class="activity_overview_container",
                    shiny::uiOutput("activity_summary")
                  )
                )
              ),
              shiny::br(),
              ## Show week highlights -----
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  tags$div(
                    shiny::htmlOutput("activity_table")
                  )
                )
              ),
              shiny::br(),

              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  DT::dataTableOutput("week_sessions")
                )
              ),
              shiny::br(),

              ## Show all strava activities for set day ----
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 12,
                  DT::dataTableOutput("day_table")
                )
              )
            )
          )
        )
      ),

      # Activity Input Tab -------
      shiny::tabPanel(
        title = "Record Activity",

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
          )
        )
      ),
      shiny::uiOutput("admin_tab")
    )
  )
)
