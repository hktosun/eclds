#' Scrape ECLDS pages
#'
#' Scrape specific sections of the ECLDS.
#' @importFrom magrittr %>%
#'
#' @param section "kindergarten" or "birth to prek"
#' @param subsection "early care and education", "child demographics", "family demographics", "economic and food assistance", "kindergarten attendance", "kindergarten summary" for kindergarten; "early childhood screening", "scholarships", "parent aware", "community assessment" for "birth to prek"
#' @param geography "county" or "school district"
#' @param year Vector of years. 2014 to 2020 as of September 2021.
#' @param browser Browser in the remote driver
#' @param port Port for the remote driver

#' @export

scrape_eclds <- function(section, subsection, geography, year, browser = "firefox", port = 4445){


	if(!section %in% c("kindergarten", "birth to prek")){
		stop("`section` must be either 'kindergarten' to 'birth to prek'.")
	}

	if(!geography %in% c("county", "school district")){
		stop('`geography must be either "county" or "school district".')
	}

	if(section == "kindergarten" & !subsection %in% c("early care and education", "child demographics", "family demographics", "economic and food assistance", "kindergarten attendance", "kindergarten summary")){
		stop('subsection for kindergarten must be one of "early care and education", "child demographics", "family demographics", "economic and food assistance", "kindergarten attendance", "kindergarten summary".')
	} else if(section == "birth to prek" & !subsection %in% c("early childhood screening", "scholarships", "parent aware")){
		stop('`subsection for birth to prek must be one of "early childhood screening", "scholarships", "parent aware".')
	}

	remdr <- create_remotedriver(browser, port)

	if(geography == "county"){
		df <- tidyr::expand_grid(county_mn_id = counties$county_mn_id, year = year) %>%
			dplyr::mutate(data = purrr::map2(.data$county_mn_id, .data$year, ~get_tables(section, subsection, "county", .x, .y, remdr)))

		df <- df %>%
			dplyr::left_join(counties, by = 'county_mn_id') %>%
			dplyr::select(.data$county_id, .data$county, .data$year, .data$data) %>%
			clean_tables(section, subsection, geography)

	} else if(geography == "school district"){

		df <- tidyr::expand_grid(sd_id = school_districts$sd_id, year = year) %>%
			dplyr::mutate(data = purrr::map2(.data$sd_id, .data$year, ~get_tables(section, subsection, "school district", .x, .y, remdr))) %>%
			dplyr::left_join(school_districts, by = 'sd_id') %>%
			dplyr::select(.data$school_district_id, .data$school_district_name, .data$year, .data$data) %>%
			clean_tables(section, subsection, geography)
	}

	df


}



