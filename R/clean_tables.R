clean_tables <- function(df, section, subsection, geography){

	if(section == "birth to prek" & subsection == "scholarships"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			filter(id %in% 1:3) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")

		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("scholarship_status", "count_frac")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("program_prior", "count_frac")))) %>%
			mutate(data3 = map(data3, ~setNames(., c("program_concurrent", "count_frac")))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", ""))))



		outcome <- list(table1 = r$data1, table2 = r$data2, table3 = r$data3)
	}

	if(section == "birth to k" & subsection == "parent aware"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			filter(id %in% 1:3) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")


		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("pa_rating", "ccap", "els", "both")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("site_type", "four_star", "three_star", "two_star", "one_star")))) %>%
			mutate(data3 = map(data3, ~setNames(., c("site_type", "four_star", "three_star", "two_star", "one_star")))) %>%
			mutate(data1 = map(data1, ~pivot_longer(.x, ccap:both, names_to = "program", values_to = "count_frac"))) %>%
			mutate(data2 = map(data2, ~pivot_longer(.x, four_star:one_star, names_to = "pa_rating", values_to = "count_frac"))) %>%
			mutate(data3 = map(data3, ~pivot_longer(.x, four_star:one_star, names_to = "pa_rating", values_to = "count_frac"))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", ""))))

		table1 <- r %>%
			select(-data2, -data3) %>%
			unnest(data1)

		table2 <- r %>%
			select(-data1, -data3) %>%
			unnest(data2)

		table3 <- r %>%
			select(-data1, -data2) %>%
			unnest(data3)

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}

	if(section == "birth to prek" & subsection == "early childhood screening"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			filter(id %in% 1:2) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")

		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("screening_age", "count_frac")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("screening_year", "age_3", "age_4", "age_5_6", "drop")))) %>%
			mutate(data2 = map(data2, ~select(.x, -drop))) %>%
			mutate(data2 = map(data2, ~pivot_longer(.x, age_3:age_5_6, names_to = "age", values_to = "count_frac"))) %>%
			mutate_at(vars(data1, data2), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", ""))))

		table1 <- r %>%
			select(-data2) %>%
			unnest(data1)

		table2 <- r %>%
			select(-data1) %>%
			unnest(data2)


		output <- list(table1 = table1, table2 = table2)


	}

	if(section == "kindergarten" & subsection == "early care and education"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			ungroup() %>%
			filter(id %in% 1:3) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")


		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("ece_participation", "count_frac")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("program", "count_frac")))) %>%
			mutate(data3 = map(data3, ~setNames(., c("program", "4", "3", "2", "1")))) %>%
			mutate(data3 = map(data3, ~pivot_longer(.x, `4`:`1`, names_to = "year_prior_k", values_to = "count_frac"))) %>%
			mutate(data3 = map(data3, ~mutate(.x, year_prior_k = as.numeric(year_prior_k)))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", ""))))


		table1 <- r %>%
			select(-data2, -data3) %>%
			unnest(data1)

		table2 <- r %>%
			select(-data1, -data3) %>%
			unnest(data2)

		table3 <- r %>%
			select(-data1, -data2) %>%
			unnest(data3)

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}

	if(section == "kindergarten" & subsection == "child demographics"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			ungroup() %>%
			filter(id %in% 1:3) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")


		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("race_ethnicity", "count_frac")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("race_ethnicity", "with_ece_participation", "without_ece_participation")))) %>%
			mutate(data3 = map(data3, ~setNames(., c("race_ethnicity", "ccap", "ecfe", "ecse", "prek")))) %>%
			mutate(data2 = map(data2, ~pivot_longer(.x, 2:3, names_to = "ece_participation", values_to = "count_frac"))) %>%
			mutate(data3 = map(data3, ~pivot_longer(.x, 2:5, names_to = "program", values_to = "count_frac"))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", ""))))


		table1 <- r %>%
			select(-data2, -data3) %>%
			unnest(data1)

		table2 <- r %>%
			select(-data1, -data3) %>%
			unnest(data2)

		table3 <- r %>%
			select(-data1, -data2) %>%
			unnest(data3)

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}

	if(section == "kindergarten" & subsection == "family demographics"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			ungroup() %>%
			filter(id %in% 1:3) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")


		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("mother_age_at_birth", "count_frac")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("teen_mother_age_at_birth", "count_frac")))) %>%
			mutate(data3 = map(data3, ~setNames(., c("mother_education_at_birth", "count_frac")))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", ""))))


		table1 <- r %>%
			select(-data2, -data3) %>%
			unnest(data1)

		table2 <- r %>%
			select(-data1, -data3) %>%
			unnest(data2)

		table3 <- r %>%
			select(-data1, -data2) %>%
			unnest(data3)

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}

	if(section == "kindergarten" & subsection == "economic and food assistance"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			ungroup() %>%
			filter(id %in% 1:3) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")


		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("assistance", "count_frac")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("assistance", "with_ece", "without_ece", "drop")))) %>%
			mutate(data3 = map(data3, ~setNames(., c("program", "mfip", "food_only", "no_assistance")))) %>%
			mutate(data2 = map(data2, ~select(., -drop))) %>%
			mutate(data2 = map(data2, ~pivot_longer(.x, with_ece:without_ece, names_to = "ece_participation", values_to = "count_frac"))) %>%
			mutate(data3 = map(data3, ~pivot_longer(.x, c(mfip, food_only, no_assistance), names_to = "assistance", values_to = "count_frac"))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", "")))) %>%
			mutate(data3 = map(data3, ~mutate(.x, program = case_when(program == 'Child Care Assistance Program (CCAP)' ~ 'ccap', program == 'Early Childhood Family Education (ECFE)' ~ 'ecfe', program == 'Early Childhood Special Education (ECSE)' ~ 'ecse', program == 'MN District Preschool' ~ 'prek'))))


		table1 <- r %>%
			select(-data2, -data3) %>%
			unnest(data1)

		table2 <- r %>%
			select(-data1, -data3) %>%
			unnest(data2)

		table3 <- r %>%
			select(-data1, -data2) %>%
			unnest(data3)

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}

	if(section == "kindergarten" & subsection == "kindergarten attendance"){

		k <- df %>%
			unnest(data)

		if(geography == "county"){
			k <- k %>%
				group_by(county_id, year)
		} else if(geography == "sd_id"){
			k <- k %>%
				group_by(sd_id, year)
		}

		m <- k %>%
			mutate(id = row_number()) %>%
			ungroup() %>%
			filter(id %in% 1:3) %>%
			pivot_wider(
				names_from = id,
				values_from = data,
				names_prefix = "data")


		r <- m %>%
			mutate(data1 = map(data1, ~setNames(., c("attendance", "count_frac")))) %>%
			mutate(data2 = map(data2, ~setNames(., c("age_at_screening", "between_0_90", "between_90_95", "between_96_100")))) %>%
			mutate(data3 = map(data3, ~setNames(., c("program", "between_0_90", "between_90_95", "between_96_100")))) %>%
			mutate_at(vars(data2, data3), ~map(.x, ~pivot_longer(.x, between_0_90:between_96_100, names_to = "attendance", values_to = "count_frac"))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~separate(.x, count_frac, into = c("count", "frac"), sep = " "))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, count = case_when(count == "N/A" ~ 0, count == "CTSTR" ~ NA_real_, TRUE ~ parse_number(count))))) %>%
			mutate_at(vars(data1, data2, data3), ~map(.x, ~mutate(.x, frac = stringr::str_replace_all(frac, "\\(|%|\\)|>", "")))) %>%
			mutate(data3 = map(data3, ~mutate(.x, program = case_when(program == 'Child Care Assistance Program (CCAP)' ~ 'ccap', program == 'Early Childhood Family Education (ECFE)' ~ 'ecfe', program == 'Early Childhood Special Education (ECSE)' ~ 'ecse', program == 'MN District Preschool' ~ 'prek', program == 'No Reported Participation' ~ 'none'))))


		table1 <- r %>%
			select(-data2, -data3) %>%
			unnest(data1)

		table2 <- r %>%
			select(-data1, -data3) %>%
			unnest(data2)

		table3 <- r %>%
			select(-data1, -data2) %>%
			unnest(data3)

		output <- list(table1 = table1, table2 = table2, table3 = table3)

	}



	output

}

