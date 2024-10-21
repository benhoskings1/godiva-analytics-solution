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
app_colours <- list(
  "v_main_colour"="#0B3861",
  "deep_blue"="#274251",
  "ice_blue"="#89A7AD",
  "light_blue"="#E6F1F7",
  "grey_blue"="#778899"
)


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
password = 'nW1O55mofKw8Dy7V'

connection_string = sprintf('mongodb+srv://%s:%s@cluster0.w6iufqn.mongodb.net/?retryWrites=True&w=majority&appName=Cluster0', username, password)

db_tables = c(
  "members" = mongo(collection="members", db="godiva_data", url=connection_string),
  "athletes" = mongo(collection="athletes", db="godiva_data", url=connection_string),
  "logins" = mongo(collection="member_login_details", db="godiva_data", url=connection_string),
  "coaches" = mongo(collection="coaches", db="godiva_data", url=connection_string),
  "activities" = mongo(collection="activities", db="godiva_data", url=connection_string)
)

athlete_member_ids = db_tables$athletes$find(fields = '{"memberId": true}', query='{}')$`memberId`
coach_ids = db_tables$coaches$find(fields = '{"memberId": true}', query='{}')$`memberId`

user_logins = db_tables$logins$find('{}')


