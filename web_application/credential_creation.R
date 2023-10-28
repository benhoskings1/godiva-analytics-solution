library(RSQLite)

df_credentials <- base::data.frame(
  user = c("admin", "neil_hoskings"),
  password = c("pass", "neil_hoskings"),
  strava_id = c(NA, "963223"),
  start = c("2020-01-01", "2020-01-01"),
  expire = c(NA, NA),
  admin = c(T, F),
  comment = "Secure authentification for app",
  stringsAsFactors = F
)

base::setwd(dir="/Users/benhoskings/Documents/RStudio/Coventry_Running/web_application")

v_web_application_name <- "Coventry_Running"

v_prepared_data_path <- "data/prepared_data/"
v_database_path <- base::paste(v_prepared_data_path, v_web_application_name, ".db", sep="")
con_SQLite <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=v_database_path)
RSQLite::dbWriteTable(conn = con_SQLite, name = "credentials", value=df_credentials, overwrite=T)

RSQLite::dbDisconnect(conn=con_SQLite)
