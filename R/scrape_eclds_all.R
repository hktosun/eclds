#' Scrape all sections
#'
#' Scrape all sections and subsections in ECLDS.
#'
#' @export
#'
scrape_eclds_all <- function(){
	grid <- tibble::tribble(~section, ~subsection, ~geography,
					'birth to prek', 'early childhood screening', 'county',
					'birth to prek', 'early childhood screening', 'school district',
					'birth to prek', 'scholarships', 'county',
					'birth to prek', 'scholarships', 'school district',
					'birth to prek', 'parent aware', 'county',
					'kindergarten', 'early care and education', 'county',
					'kindergarten', 'early care and education', 'school district',
					'kindergarten', 'child demographics', 'county',
					'kindergarten', 'child demographics', 'school district',
					'kindergarten', 'family demographics', 'county',
					'kindergarten', 'family demographics', 'school district',
					'kindergarten', 'economic and food assistance', 'county',
					'kindergarten', 'economic and food assistance', 'school district',
					'kindergarten', 'kindergarten attendance', 'county',
					'kindergarten', 'kindergarten attendance', 'school district'
					)

	a <- grid %>%
		dplyr::mutate(data = purrr::pmap(list(.data$section, .data$subsection, .data$geography), ~scrape_eclds(section = ..1, subsection = ..2, geography = ..3, year = 2014:2020)))





}
