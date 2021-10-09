#' Clean tables
#'
#' Clean ECLDS tables
#'
#' @importFrom rlang .data
#'
#' @param df Data frame
#' @param section Section
#' @param subsection Subsection
#' @param geography Geography
#' @export

clean_tables <- function(df, section, subsection, geography){

	k <- df %>%
		tidyr::unnest(.data$data)

	if(geography == "county"){
		k <- k %>%
			dplyr::group_by(.data$county_id, .data$year)
	} else if(geography == "sd_id"){
		k <- k %>%
			dplyr::group_by(.data$school_district_id, .data$year)
	}

	if(section == "birth to prek" & subsection == "early childhood screening"){
		ntables <- 1
	} else {
		ntables <- 3
	}

	m <- k %>%
		dplyr::mutate(id = dplyr::row_number()) %>%
		dplyr::filter(.data$id %in% 1:ntables) %>%
		tidyr::pivot_wider(
			names_from = .data$id,
			values_from = .data$data,
			names_prefix = "data")


	if(section == "birth to prek" & subsection == "scholarships"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("scholarship_status", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~setNames(., c("program_prior", "count_frac")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~setNames(., c("program_concurrent", "count_frac"))))

	}

	else if(section == "birth to prek" & subsection == "parent aware"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("quality_rating", "CCAP Only", "ELS Only", "Both CCAP and ELS")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~setNames(., c("site_type", "four_star", "three_star", "two_star", "one_star")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~setNames(., c("site_type", "four_star", "three_star", "two_star", "one_star")))) %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~tidyr::pivot_longer(.x, `CCAP Only`:`Both CCAP and ELS`, names_to = "funding", values_to = "count_frac"))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~tidyr::pivot_longer(.x, four_star:one_star, names_to = "quality_rating", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~tidyr::pivot_longer(.x, four_star:one_star, names_to = "quality_rating", values_to = "count_frac"))) %>%
			dplyr::mutate_at(dplyr::vars(.data$data2, .data$data3), ~purrr::map(., ~dplyr::mutate(.x, quality_rating = dplyr::case_when(quality_rating == "four_star" ~ "Four Stars", quality_rating == "three_star" ~ "Three Stars", quality_rating == "two_star" ~ "Two Stars", quality_rating == "one_star" ~ "One Star"))))

	}

	else if(section == "birth to prek" & subsection == "early childhood screening"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("screening_age", "count_frac"))))

	}

	else if(section == "kindergarten" & subsection == "early care and education"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("ece_participation", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~setNames(., c("program", "count_frac")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~setNames(., c("program", "4", "3", "2", "1")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~tidyr::pivot_longer(.x, `4`:`1`, names_to = "year_prior_k", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~dplyr::mutate(.x, year_prior_k = as.numeric(year_prior_k))))

	}

	else if(section == "kindergarten" & subsection == "child demographics"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("race_ethnicity", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~setNames(., c("race_ethnicity", "with_ece_participation", "without_ece_participation")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~setNames(., c("race_ethnicity", "ccap", "ecfe", "ecse", "prek")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~tidyr::pivot_longer(.x, 2:3, names_to = "ece_participation", values_to = "count_frac"))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~dplyr::mutate(.x, ece_participation = dplyr::case_when(ece_participation == "with_ece_participation" ~ "Known public ECE participation", ece_participation == "without_ece_participation" ~ "No ECE data available")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~tidyr::pivot_longer(.x, 2:5, names_to = "program", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~dplyr::mutate(.x, program = dplyr::case_when(program == "ccap" ~ "Child Care Assistance Program (CCAP)", program == "ecfe" ~ "Early Childhood Family Education (ECFE)", program == "ecse" ~ "Early Childhood Special Education (ECSE)", program == "prek" ~ "MN District Preschool"))))


	}

	else if(section == "kindergarten" & subsection == "family demographics"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("mother_age_at_birth", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~setNames(., c("teen_mother_age_at_birth", "count_frac")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~setNames(., c("mother_education_at_birth", "count_frac"))))
	}

	else if(section == "kindergarten" & subsection == "economic and food assistance"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("assistance", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~setNames(., c("assistance", "with_ece", "without_ece", "drop")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~setNames(., c("program", "mfip", "food_only", "no_assistance")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~dplyr::select(., -drop))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~tidyr::pivot_longer(.x, with_ece:without_ece, names_to = "ece_participation", values_to = "count_frac"))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~dplyr::mutate(.x, ece_participation = dplyr::case_when(ece_participation == "with_ece" ~ "Known public ECE participation", ece_participation == "without_ece" ~ "No ECE data available")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~tidyr::pivot_longer(.x, c(mfip, food_only, no_assistance), names_to = "assistance", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~dplyr::mutate(.x, assistance = dplyr::case_when(assistance == "mfip" ~ "MFIP/DWP", assistance == "food_only" ~ "Food Assistance Only (No MFIP/DWP)", assistance == "no_assistance" ~ "No MFIP/DWP or Food Assistance"))))

	}

	else if(section == "kindergarten" & subsection == "kindergarten attendance"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(.data$data1, ~setNames(., c("attendance", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(.data$data2, ~setNames(., c("age_at_screening", "Below 90%", "90% - 95%", "96% - 100%")))) %>%
			dplyr::mutate(data3 = purrr::map(.data$data3, ~setNames(., c("program", "Below 90%", "90% - 95%", "96% - 100%")))) %>%
			dplyr::mutate_at(dplyr::vars(.data$data2, .data$data3), ~purrr::map(.x, ~tidyr::pivot_longer(.x, `Below 90%`:`96% - 100%`, names_to = "attendance", values_to = "count_frac")))

	}

	r <- r %>%
		dplyr::mutate_at(dplyr::vars(dplyr::one_of("data1", "data2", "data3")), ~purrr::map(.x, ~tidyr::separate(.x, count_frac, into = c("count", "frac"), sep = " ", fill = 'right'))) %>%
		dplyr::mutate_at(dplyr::vars(dplyr::one_of("data1", "data2", "data3")), ~purrr::map(.x, ~dplyr::mutate(.x, count = dplyr::case_when(count == "N/A" ~ "0", count == "CTSTR" ~ NA_character_, TRUE ~ count)))) %>%
		dplyr::mutate_at(dplyr::vars(dplyr::one_of("data1", "data2", "data3")), ~purrr::map(.x, ~dplyr::mutate(.x, count = readr::parse_number(count)))) %>%
		dplyr::mutate_at(dplyr::vars(dplyr::one_of("data1", "data2", "data3")), ~purrr::map(.x, ~dplyr::mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)", ""))))

	if(section == "birth to prek" & subsection == "early childhood screening"){

		table1 <- r %>%
			dplyr::filter(purrr::map_dbl(.data$data1, ~nrow(.x)) > 0) %>%
			tidyr::unnest(.data$data1) %>%
			dplyr::ungroup()

		output <- list(table1 = table1)

	} else {

		table1 <- r %>%
			dplyr::select(-.data$data2, -.data$data3) %>%
			dplyr::filter(purrr::map_dbl(.data$data1, ~nrow(.x)) > 0) %>%
			tidyr::unnest(.data$data1) %>%
			dplyr::ungroup()

		table2 <- r %>%
			dplyr::select(-.data$data1, -.data$data3) %>%
			dplyr::filter(purrr::map_dbl(.data$data2, ~nrow(.x)) > 0) %>%
			tidyr::unnest(.data$data2) %>%
			dplyr::ungroup()

		table3 <- r %>%
			dplyr::select(-.data$data1, -.data$data2) %>%
			dplyr::filter(purrr::map_dbl(.data$data3, ~nrow(.x)) > 0) %>%
			tidyr::unnest(.data$data3) %>%
			dplyr::ungroup()

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}

	output

}

