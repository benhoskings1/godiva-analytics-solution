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
library(mongolite)
library(lubridate)
library(ggplot2)
library(glue)

Sys.setenv(VROOM_CONNECTION_SIZE=5000072)

# Define app colours -------
v_main_colour <- "#0B3861"
deep_blue <-  "#274251"
ice_blue <- "#89A7AD"
light_blue <- "#E6F1F7"

# Define app Loading Screen -------
html_loading_screen <- shiny::tagList(
  waiter::spin_folding_cube(),
  shiny::br(),
  shiny::h4("Loading Web Application...")
)

base::source("app_functions/main_functions.R")
base::source("app_functions/html_construction.R")
base::source("app_functions/graph_construction.R")


# Define useful paths ---
v_global_session_path <- "global_session.R"
v_prepared_data_path <- "data/prepared_data/"
v_database_path <- base::paste(v_prepared_data_path, "Coventry_Godiva.db", sep="")

# load data from Mongo DB -------
username = 'benjaminhoskings'
password = 'i6UMmr7TpOcIOUcc'

connection_string = sprintf('mongodb+srv://%s:%s@cluster0.w6iufqn.mongodb.net/?retryWrites=True&w=majority&appName=Cluster0', username, password)

consults_db = mongo(collection="user_data", db="godiva_data", url=connection_string)
athlete_db = mongo(collection="athlete_data", db="godiva_data", url=connection_string)
athlete_data = athlete_db$find()
athlete_data <- athlete_data |>
  dplyr::mutate(full_name = paste(first_name, last_name, sep = " "))
athlete_id_map <- athlete_data$strava_id
names(athlete_id_map) <- athlete_data$full_name

user_select <- unique(consults_db$find('{}')$athlete_id)
names(user_select) <- paste("User ", user_select)


## Graph options ------
activity_graph_options = list(
  "Run"=list("Heart Rate" = "heartrate", "Pace" = "pace"),
  "Ride"=list("Heart Rate" = "heartrate", "Speed"="Speed",
              "Elevation" = "altitude"),
  "Swim"=list("Pace" = "pace")
)


# activity_data <- readr::read_tsv(base::paste(v_prepared_data_path,"Activities.tsv", sep=""), show_col_types = FALSE)
e = simpleError("No internet connection")

# activities_detailed <- activities_detailed |>
#   dplyr::mutate(distance_m = distance,
#                 distance_yd = distance_m * 1.0936132983,
#                 distance_km = distance / 1000,
#                 distance_miles = distance_km * 0.6213711922)

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

v_last_data_update = base::Sys.Date()

week_days <- c("Monday"=1, "Tuesday"=2, "Wednesday"=3, "Thurdsay"=4,
               "Friday"=5, "Saturday"=6, "Sunday"=7)

today <- base::Sys.Date()
base_date <- lubridate::origin
lubridate::hour(base_date) <- -1

df_credentials <- base::data.frame(
  user = c("admin", "neil_hoskings", "clarky"),
  password = c("pass", "neil_hoskings", "REPS"),
  strava_id = c(NA, "963223", NA),
  start = c("2020-01-01", "2020-01-01", "2020-01-01"),
  expire = c(NA, NA, NA),
  admin = c(T, F, T),
  permissions=c(NA, NA, "REPS"),
  comment = "Secure authentification for app",
  stringsAsFactors = F
)
