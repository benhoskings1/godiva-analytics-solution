library(readr)

member_data <- readr::read_tsv(base::paste(v_prepared_data_path,"Activities_Detailed.tsv", sep=""), show_col_types = FALSE)
