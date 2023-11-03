# Load Required Libraries -------
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(RSQLite)
library(shinyjs)
library(shinymanager)
library(shinytoastr)
library(shinyBS)
library(toastui)
library(shinydashboardPlus)
library(htmlwidgets)
library(waiter)
library(shinydisconnect)
library(shinyscreenshot)
library(bslib)
library(readr)
library(purrr)
library(shinyTime)

Sys.setenv(VROOM_CONNECTION_SIZE=5000072)

# Define app colours -------
v_main_colour <- "#0B3861"

# Define app Loading Screen -------
html_loading_screen <- shiny::tagList(
  waiter::spin_folding_cube(),
  shiny::br(),
  shiny::h4("Loading Web Application...")
)

base::source("app_functions.R")

activity_data <- readr::read_tsv("data/prepared_data/Activities.tsv", show_col_types = FALSE)
member_data <- readr::read_tsv("data/prepared_data/Member_Data.tsv", show_col_types = FALSE)

# Define useful paths ---

v_functions_path <- "app_functions.R"
v_global_session_path <- "global_session.R"
v_prepared_data_path <- "data/prepared_data/"
v_database_path <- base::paste(v_prepared_data_path, "Coventry_Godiva.db", sep="")

# Load app functions ------
base::source(v_functions_path)

# Load credentials -------
con_SQLite <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=v_database_path)
df_credentials <- RSQLite::dbReadTable(conn= con_SQLite, name = "credentials")


# activity_data <- readr::read_tsv(base::paste(v_prepared_data_path,"Activities.tsv", sep=""), show_col_types = FALSE)
activity_data <- readr::read_delim("https://raw.githubusercontent.com/benhoskings1/REPS/main/Activities.tsv", delim = "\t", show_col_types = FALSE)
member_data <- readr::read_tsv(base::paste(v_prepared_data_path,"Member_Data.tsv", sep=""), show_col_types = FALSE)
activities_detailed <- readr::read_tsv(base::paste(v_prepared_data_path,"Activities_Detailed.tsv", sep=""), show_col_types = FALSE)

if (base::file.exists(base::paste(v_prepared_data_path, "session_data.tsv", sep=""))) {
  session_record_all <- readr::read_tsv(
    base::paste(v_prepared_data_path, "session_data.tsv", sep=""),
    show_col_types = F
  )
} else {
  session_record_all <- base::data.frame(
    Athlete_ID = numeric(),
    Date=numeric(),
    Session_Type=character(),
    Session_Sets=numeric(),
    Session_Reps=numeric(),
    Session_Timings=character())

}

# Tracking variables -----

gl_df_tracking <- base::data.frame(
  stringsAsFactors = F,
  "Web_Application" = c(""),
  "User" = c(""),
  "Session" = c(""),
  "Connection" = c(""),
  "Disconnection" = c("")
)

if("Tracking" %in% RSQLite::dbListTables(conn = con_SQLite)) {
  gl_min_date <- RSQLite::dbReadTable(conn = con_SQLite, name = "Tracking") |>
    dplyr::filter(!(User %in% c("admin"))) |>
    dplyr::mutate(min_date = base::as.Date(base::strptime(Connection, format="%Y-%m-%d %H:%M:%S"))) |>
    dplyr::pull(min_date)

  if(base::length(gl_min_date) == 0) {
    gl_min_date <- base::Sys.time()
    gl_users <- c("No users")

  } else {
    gl_min_date <- gl_min_date |>
      dplyr::first()

    gl_l_users <- RSQLite::dbReadTable(conn = con_SQLite, name = "Tracking") |>
      dplyr::filter(!(User %in% c("admin"))) |>
      dplyr::pull(User) |>
      base::unique() |>
      base::sort()
  }
} else {
  gl_min_date <- base::Sys.Date()
  gl_l_users <- c("No users")
}

# disconnect database
RSQLite::dbDisconnect(conn = con_SQLite)
v_last_data_update = base::Sys.Date()

# User interface -------
withBusyIndicatorCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}"

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    shiny::singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(
        class = "btn-err",
        div(
          icon("exclamtion-circle"),
          tags$b("Error: "),
          span(class = "btn-err-msg")
        )
      )
    )
  )
}

withBusyIndicatorServer <- function(buttonId, expr) {
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err-indicator", buttonId)

  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)

  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })

  tryCatch({
    value <- expr

    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(
      selector = doneEl, anim = T, animType = "fade", time = 0.5))
    value
  }, error <- function(err) errorFunc(err, buttonId))
}

errorFunc <- function(err, buttonId) {
  errorEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$mesage)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = T, animType = "fade")
}

setShadow <- function(id = NULL, class = NULL) {
  cssShadow <- paste0(
    " box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
    transition: 0.3s;
    border-radius: 5px"
  )

  cssHover <- "box-shadow: 0 16px 32px 0 rgba(0, 0, 0, 0, 0.2);"

  cssShadow <- if(!is.null(id)) {
    if(!is.null(class)) {
      paste0("#", id, ":hover .", class, ":hover {", cssHover, "}")
    } else {
      paste0("#", id, ":hover", " {", cssHover, "}")
    }
  } else {
    if(!is.null(class)) {
      paste0(".", class, ":hover", " {", cssHover, "}")
    } else {
      NULL
    }
  }

  css <- paste(cssShadow, cssHover)

  htmltools::tags$head(
    htmltools::tags$style(css)
  )
}


