#' @importFrom magrittr %>%

get_tables <- function(section, subsection, geography, i, year, remdr){

	df <- tryCatch(
		{

			if(geography == "county"){

				if(section == "birth to prek" & subsection == "early childhood screening"){
					link <- paste0("http://eclds.mn.gov/#earlyChildhoodScreening/orgId--cOrg",
								   i,
								   "__groupType--county__FISCAL_YEAR--",
								   year,
								   "__MOMS_EDLVL_AT_CB--ALL__PRENATAL_CARE--FOC_NONE__HOME_LANGUAGE--FOC_NONE__p--7")
				}

				else if(section == "birth to prek" & subsection == "scholarships"){
					link <- paste0("http://eclds.mn.gov/#scholarships/orgId--cOrg",
								   i,
								   "__groupType--county__ECO_DEV_REGION--FOC_NONE__FISCAL_YEAR--",
								   year,
								   "__HOME_LANGUAGE--FOC_NONE__MOMS_AGE_AT_CB--ALL__p--7")
				}

				else if(section == "birth to prek" & subsection == "parent aware"){
					link <- paste0("http://eclds.mn.gov/#parentAware/orgId--cOrg",
								   i,
								   "__groupType--county__ECO_DEV_REGION--FOC_NONE__FISCAL_YEAR--",
								   year,
								   "__HOME_LANGUAGE--FOC_NONE__MOMS_AGE_AT_CB--ALL__p--7")
				}

				else if(section == "kindergarten" & subsection == "early care and education"){
					link <- paste0("http://eclds.mn.gov/#programParticipation/orgId--cOrg",
								   i,
								   "__groupType--county__FISCAL_YEAR--",
								   year,
								   "__p--7")
				}

				else if(section == "kindergarten" & subsection == "child demographics"){
					link <- paste0("http://eclds.mn.gov/#childDemographics/orgId--cOrg",
					i,
					"__groupType--county__FISCAL_YEAR--",
					year,
					"__DISABILITY_TYPE--FOC_NONE__HOME_LANGUAGE--FOC_NONE__p--7")
				}

				else if(section == "kindergarten" & subsection == "family demographics"){
					link <- paste0("http://eclds.mn.gov/#familyDemographics/orgId--cOrg",
								   i,
								   "__groupType--county__FISCAL_YEAR--",
								   year,
								   "__p--7")
				}

				else if(section == "kindergarten" & subsection == "economic and food assistance"){
					link <- paste0("http://eclds.mn.gov/#economicFoodAssistance/orgId--cOrg",
								   i,
								   "__groupType--county__FISCAL_YEAR--",
								   year,
								   "__DISABILITY_TYPE--FOC_NONE__HOME_LANGUAGE--FOC_NONE__p--7")
				}

				else if(section == "kindergarten" & subsection == "kindergarten attendance"){
					link <- paste0("http://eclds.mn.gov/#kgAttend/orgId--cOrg",
								   i,
								   "__groupType--county__FISCAL_YEAR--",
								   year,
								   "__HOME_LANGUAGE--FOC_NONE__MOMS_EDLVL_AT_CB--ALL__p--7")
				}

				else if(section == "kindergarten" & subsection == "kindergarten summary"){
					link <- paste0("http://eclds.mn.gov/#summaryReport/orgId--cOrg",
								   i,
								   "__groupType--county__FISCAL_YEAR--",
								   year,
								   "__p--1")
				}

			} else if(geography == "school district"){

				if(section == "birth to prek" & subsection == "early childhood screening"){
					link <- paste0("http://eclds.mn.gov/#earlyChildhoodScreening/orgId--",
								   i,
								   "__FISCAL_YEAR--",
								   year,
								   "__MOMS_EDLVL_AT_CB--ALL__PRENATAL_CARE--FOC_NONE__HOME_LANGUAGE--FOC_NONE__p--7")
				}

				else if(section == "birth to prek" & subsection == "scholarships"){
					link <- paste0("http://eclds.mn.gov/#scholarships/orgId--",
								   i,
								   "__ECO_DEV_REGION--FOC_NONE__FISCAL_YEAR--",
								   year,
								   "__HOME_LANGUAGE--FOC_NONE__MOMS_AGE_AT_CB--ALL__p--7")
				}

				else if(section == "birth to prek" & subsection == "parent aware"){
					link <- paste0("http://eclds.mn.gov/#parentAware/orgId--",
								   i,
								   "__ECO_DEV_REGION--FOC_NONE__FISCAL_YEAR--",
								   year,
								   "__HOME_LANGUAGE--FOC_NONE__MOMS_AGE_AT_CB--ALL__p--7")
				}

				else if(section == "kindergarten" & subsection == "early care and education"){
					link <- paste0("http://eclds.mn.gov/#programParticipation/orgId--",
								   i,
								   "__FISCAL_YEAR--",
								   year,
								   "__p--7")
				}

				else if(section == "kindergarten" & subsection == "child demographics"){
					link <- paste0("http://eclds.mn.gov/#childDemographics/orgId--",
								   i,
								   "__FISCAL_YEAR--",
								   year,
								   "__DISABILITY_TYPE--FOC_NONE__HOME_LANGUAGE--FOC_NONE__p--7")
				}

				else if(section == "kindergarten" & subsection == "family demographics"){
					link <- paste0("http://eclds.mn.gov/#familyDemographics/orgId--",
								   i,
								   "__FISCAL_YEAR--",
								   year,
								   "__p--7")
				}

				else if(section == "kindergarten" & subsection == "economic and food assistance"){
					link <- paste0("http://eclds.mn.gov/#economicFoodAssistance/orgId--",
								   i,
								   "__FISCAL_YEAR--",
								   year,
								   "__DISABILITY_TYPE--FOC_NONE__HOME_LANGUAGE--FOC_NONE__p--7")
				}

				else if(section == "kindergarten" & subsection == "kindergarten attendance"){
					link <- paste0("http://eclds.mn.gov/#kgAttend/orgId--",
								   i,
								   "__FISCAL_YEAR--",
								   year,
								   "__HOME_LANGUAGE--FOC_NONE__MOMS_EDLVL_AT_CB--ALL__p--7")
				}

				else if(section == "kindergarten" & subsection == "kindergarten summary"){
					link <- paste0("http://eclds.mn.gov/#summaryReport/orgId--",
								   i,
								   "__FISCAL_YEAR--",
								   year,
								   "__p--1")
				}
			}


			remdr$open(silent = TRUE)
			remdr$navigate(link)
			print(paste(section, subsection, geography, i, year, sep = ", "))
			Sys.sleep(5)

			html <- remdr$getPageSource()[[1]] %>%
				rvest::read_html()

			remdr$closeWindow()

			tables <- html %>%
				rvest::html_table(fill = TRUE)

			tables
		},
		error = function(cond){
			return(NA)
		})
	df
}

