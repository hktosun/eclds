#' Clean tables
#'
#' Clean ECLDS tables
#'
#' @param df Data fram
#' @param section Section
#' @param subsection Subsection
#' @param geography Geography
#' @export

clean_tables <- function(df, section, subsection, geography){

	k <- df %>%
		tidyr::unnest(data)

	if(geography == "county"){
		k <- k %>%
			dplyr::group_by(county_id, year)
	} else if(geography == "sd_id"){
		k <- k %>%
			dplyr::group_by(sd_id, year)
	}

	if(section == "birth to prek" & subsection == "early childhood screening"){
		ntables <- 2
	} else {
		ntables <- 3
	}

	m <- k %>%
		dplyr::mutate(id = dplyr::row_number()) %>%
		dplyr::filter(id %in% 1:ntables) %>%
		tidyr::pivot_wider(
			names_from = id,
			values_from = data,
			names_prefix = "data")


	if(section == "birth to prek" & subsection == "scholarships"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(data1, ~setNames(., c("scholarship_status", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~setNames(., c("program_prior", "count_frac")))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~setNames(., c("program_concurrent", "count_frac"))))

	}

	else if(section == "birth to k" & subsection == "parent aware"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(data1, ~setNames(., c("pa_rating", "ccap", "els", "both")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~setNames(., c("site_type", "four_star", "three_star", "two_star", "one_star")))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~setNames(., c("site_type", "four_star", "three_star", "two_star", "one_star")))) %>%
			dplyr::mutate(data1 = purrr::map(data1, ~tidyr::pivot_longer(.x, ccap:both, names_to = "program", values_to = "count_frac"))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~tidyr::pivot_longer(.x, four_star:one_star, names_to = "pa_rating", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~tidyr::pivot_longer(.x, four_star:one_star, names_to = "pa_rating", values_to = "count_frac")))

	}

	else if(section == "birth to prek" & subsection == "early childhood screening"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(data1, ~setNames(., c("screening_age", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~setNames(., c("screening_year", "age_3", "age_4", "age_5_6", "drop")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~dplyr::select(.x, -drop))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~tidyr::pivot_longer(.x, age_3:age_5_6, names_to = "age", values_to = "count_frac")))

	}

	else if(section == "kindergarten" & subsection == "early care and education"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(data1, ~setNames(., c("ece_participation", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~setNames(., c("program", "count_frac")))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~setNames(., c("program", "4", "3", "2", "1")))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~tidyr::pivot_longer(.x, `4`:`1`, names_to = "year_prior_k", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~dplyr::mutate(.x, year_prior_k = as.numeric(year_prior_k))))

	}

	else if(section == "kindergarten" & subsection == "child demographics"){

		r <- m %>%
			dplyr::mutate(data1 = map(data1, ~setNames(., c("race_ethnicity", "count_frac")))) %>%
			dplyr::mutate(data2 = map(data2, ~setNames(., c("race_ethnicity", "with_ece_participation", "without_ece_participation")))) %>%
			dplyr::mutate(data3 = map(data3, ~setNames(., c("race_ethnicity", "ccap", "ecfe", "ecse", "prek")))) %>%
			dplyr::mutate(data2 = map(data2, ~tidyr::pivot_longer(.x, 2:3, names_to = "ece_participation", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = map(data3, ~tidyr::pivot_longer(.x, 2:5, names_to = "program", values_to = "count_frac")))

	}

	else if(section == "kindergarten" & subsection == "family demographics"){

		r <- m %>%
			dplyr::mutate(data1 = map(data1, ~setNames(., c("mother_age_at_birth", "count_frac")))) %>%
			dplyr::mutate(data2 = map(data2, ~setNames(., c("teen_mother_age_at_birth", "count_frac")))) %>%
			dplyr::mutate(data3 = map(data3, ~setNames(., c("mother_education_at_birth", "count_frac"))))
	}

	else if(section == "kindergarten" & subsection == "economic and food assistance"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(data1, ~setNames(., c("assistance", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~setNames(., c("assistance", "with_ece", "without_ece", "drop")))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~setNames(., c("program", "mfip", "food_only", "no_assistance")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~dplyr::select(., -drop))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~tidyr::pivot_longer(.x, with_ece:without_ece, names_to = "ece_participation", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~tidyr::pivot_longer(.x, c(mfip, food_only, no_assistance), names_to = "assistance", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = map(data3, ~dplyr::mutate(.x, program = dplyr::case_when(program == 'Child Care Assistance Program (CCAP)' ~ 'ccap', program == 'Early Childhood Family Education (ECFE)' ~ 'ecfe', program == 'Early Childhood Special Education (ECSE)' ~ 'ecse', program == 'MN District Preschool' ~ 'prek'))))

	}

	else if(section == "kindergarten" & subsection == "kindergarten attendance"){

		r <- m %>%
			dplyr::mutate(data1 = purrr::map(data1, ~setNames(., c("attendance", "count_frac")))) %>%
			dplyr::mutate(data2 = purrr::map(data2, ~setNames(., c("age_at_screening", "between_0_90", "between_90_95", "between_96_100")))) %>%
			dplyr::mutate(data3 = purrr::map(data3, ~setNames(., c("program", "between_0_90", "between_90_95", "between_96_100")))) %>%
			dplyr::mutate_at(dplyr::vars(data2, data3), ~purrr::map(.x, ~tidyr::pivot_longer(.x, between_0_90:between_96_100, names_to = "attendance", values_to = "count_frac"))) %>%
			dplyr::mutate(data3 = map(data3, ~dplyr::mutate(.x, program = dplyr::case_when(program == 'Child Care Assistance Program (CCAP)' ~ 'ccap', program == 'Early Childhood Family Education (ECFE)' ~ 'ecfe', program == 'Early Childhood Special Education (ECSE)' ~ 'ecse', program == 'MN District Preschool' ~ 'prek', program == 'No Reported Participation' ~ 'none'))))

	}

	r <- r %>%
		dplyr::mutate_at(dplyr::vars(data1, data2, data3), ~purrr::map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
		dplyr::mutate_at(dplyr::vars(data1, data2, data3), ~purrr::map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ readr::parse_number(count))))) %>%
		dplyr::mutate_at(dplyr::vars(data1, data2, data3), ~purrr::map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", ""))))

	if(section == "birth to prek" & subsection == "early childhood screening"){

		table1 <- r %>%
			dplyr::select(-data2) %>%
			tidyr::unnest(data1)

		table2 <- r %>%
			dplyr::select(-data1) %>%
			tidyr::unnest(data2)

		output <- list(table1 = table1, table2 = table2)

	} else {

		table1 <- r %>%
			dplyr::select(-data2, -data3) %>%
			tidyr::unnest(data1)

		table2 <- r %>%
			dplyr::select(-data1, -data3) %>%
			tidyr::unnest(data2)

		table3 <- r %>%
			dplyr::select(-data1, -data2) %>%
			tidyr::unnest(data3)

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}

	output

}

