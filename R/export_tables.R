#' Export ECLDS data
#'
#' Export ECLDS data coming from scraping all (main) tables to data files
#'
#' @param data ECLDS data acquired using `scrape_eclds_all`.
#' @param path Path of the folder to write the files

export_eclds_all <- function(data, path){

	ff <- data %>%
		dplyr::mutate(table_names = dplyr::case_when(
			.data$section == 'birth to prek' & .data$subsection == 'early childhood screening' ~ list(c('ece_age_at_screening')),
			.data$section == 'birth to prek' & .data$subsection == 'scholarships' ~ list(c('ece_assistance_any_before_scholarship', 'ece_assistance_type_before_scholarship', 'ece_program_during_scholarship')),
			.data$section == 'birth to prek' & .data$subsection == 'parent aware' ~ list(c('ece_funding_by_rating', 'ece_els_by_rating_by_providertype', 'ece_ccap_by_rating_by_providertype')),
			.data$section == 'kindergarten' & .data$subsection == 'early care and education' ~ list(c('k_ece_any', 'k_by_ece_program', 'k_by_ece_program_by_year')),
			.data$section == 'kindergarten' & .data$subsection == 'child demographics' ~ list(c('k_by_race', 'k_by_ece_any_by_race', 'k_by_ece_program_by_race')),
			.data$section == 'kindergarten' & .data$subsection == 'family demographics' ~ list(c('k_mother_age', 'k_teenmother_age', 'k_mother_education')),
			.data$section == 'kindergarten' & .data$subsection == 'economic and food assistance' ~ list(c('k_by_assistance', 'k_by_assistance_by_ece_any', 'k_by_assistance_by_ece_program')),
			.data$section == 'kindergarten' & .data$subsection == 'kindergarten attendance' ~ list(c('k_attendance', 'k_attendance_by_age_at_screening', 'k_attendance_by_ece_program')),
		)) %>%
		tidyr::unnest(c(.data$data, .data$table_names))

	if(!dir.exists(paste0(path, '/county'))){
		dir.create(paste0(path, '/county'))
	}

	if(!dir.exists(paste0(path, '/school district'))){
		dir.create(paste0(path, '/school district'))
	}

	ff %>%
		dplyr::select(.data$table_names, .data$geography, .data$data) %>%
		purrr::pwalk(~readr::write_csv(x = ..3, file = paste0(path, "/" , ..2, "/" , ..1, ".csv")))

}
