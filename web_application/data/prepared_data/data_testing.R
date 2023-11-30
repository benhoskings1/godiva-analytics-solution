library(readr)

member_data <- readr::read_tsv(base::paste(v_prepared_data_path,"Activities_Detailed.tsv", sep=""), show_col_types = FALSE)


activities_detailed |>
  dplyr::group_by(Athlete_ID) |>
  dplyr::summarise(n = dplyr::n())
