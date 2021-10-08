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
	}

	else if(section == "birth to prek" & !subsection %in% c("early childhood screening", "scholarships", "parent aware")){
		stop('`subsection for birth to prek must be one of "early childhood screening", "scholarships", "parent aware".')
	}

	remdr <- create_remotedriver(browser, port)

	if(geography == "county"){
		df <- tidyr::expand_grid(county_id = counties$county_mn_id[1:2], year = year) %>%
			dplyr::mutate(data = purrr::map2(.data$county_id, .data$year, ~get_tables(section, subsection, "county", .x, .y, remdr)))
	}

	else if(geography == "school district"){

		df <- tidyr::expand_grid(school_district_id = school_districts$sd_id[1:2], year = year) %>%
			dplyr::mutate(data = purrr::map2(.data$school_district_id, .data$year, ~get_tables(section, subsection, "school district", .x, .y, remdr)))
	}

	df <- clean_tables(df, section, subsection, geography)

	df

}



