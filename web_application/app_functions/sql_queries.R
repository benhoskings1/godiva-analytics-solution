query_user_details <- function(member_id) {
  user_data = db_tables$members$find(
    query = sprintf('{"_id": {"$oid": "%s"}}', member_id),
    fields = '{"_id": true, "first_name": true, "last_name": true, "accessId": true}')
}

query_athlete_activities <- function(athlete_id, start_date=NULL, end_date=NULL) {
  db_tables$activities$find(
    query = sprintf('
      {
        "athleteId": {"$oid": "%s"},
        "start_date_local": {
            "$gte": "%s",
            "$lt": "%s"
        }
      }',
      athlete_id, start_date, end_date)
  )
}

query_athlete_id <- function(member_id) {
  result<- db_tables$athletes$find(
    query = sprintf('{"memberId": {"$oid": "%s"}}', member_id),
    fields = '{"_id": true}'
  )

  if(nrow(result) > 0) {
    result|> dplyr::pull(
      `_id`
    )
  } else { NULL}
}

