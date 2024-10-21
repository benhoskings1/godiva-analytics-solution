create_athlete_tab <- function(admin_panel=FALSE, athlete_select=NULL) {
  tags$div(class="profile_layout",
    tags$div(class="data_container",
      tags$div(style="grid-area: sidebar",
        if (admin_panel){
          shiny::selectInput(
            inputId = "athlete_filter",
            label = "Select Athelete",
            choices = {
              if (is.null(athlete_select)) {
                c("Not Available"=1)
              } else {
                athlete_select
              }
            }
          )
        },
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
          choices = c("All"=NA, week_days)
        ),
        shiny::selectInput(
          inputId = "activity_type_filter",
          label = "Activity Type",
          choices = c("None"=NA, "Run")
        )
      ),
      tags$div(class="data_container",
        tags$div(class="grid_item", style="height: 100%;",
          tags$div(class="profile_overview", style="height: 100%",
            tags$div(class="grid_item", style="grid-area: profile_pic",
              "PROFILE PIC HERE"
            ),
            tags$div(style="grid-area: activity_overview",
              tags$div("Weekly Summary"),
              shiny::uiOutput("AT_athlete_week_overview")
            ),
            tags$div(class="grid_title", style="grid-area: name; font-size: 24px;",
              "NAME GOES HERE"
            ),
            tags$div(class="grid_item", style="grid-area: age;", "Age: 21"),
            tags$div(class="grid_item", style="grid-area: event;", "Event: 800m")
          )
        )
      )
    )
  )
}

