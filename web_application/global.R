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

# disconnect database
RSQLite::dbDisconnect(conn = con_SQLite)
v_last_data_update = base::Sys.Date()

week_days <- c("Monday"=1, "Tuesday"=2, "Wednesday"=3, "Thurdsay"=4,
               "Friday"=5, "Saturday"=6, "Sunday"=7)

today <- base::Sys.Date()
