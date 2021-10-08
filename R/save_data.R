# school_districts <- readr::read_csv("data_raw/school_districts.csv") %>%
# 	dplyr::mutate(sd_id = paste0(school_district_id, school_district_type))
#
# counties <- readr::read_csv("data_raw/counties.csv")
# disabilities <- readr::read_csv("data_raw/disabilities.csv")
# languages <- readr::read_csv("data_raw/languages.csv")
#
# usethis::use_data(school_districts, counties, disabilities, languages, internal = TRUE, overwrite = TRUE)
