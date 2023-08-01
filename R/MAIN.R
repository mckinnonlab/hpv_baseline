

rm(list = ls())

library(tidyverse)


## get file names with directories
files <- list.files("data", full.names = TRUE)
files

## get variable labels
vars_labels <- readxl::read_excel(here::here("data", "vars_labels.xlsx"))
vars <- setNames(as.list(vars_labels$labels), vars_labels$vars)

## process geneXpert result
source(here::here("R", "01_gxp.R"), echo = FALSE)

## process screening data 
source(here::here("R", "02_screen.R"), echo = FALSE)



## combined screen and gxp 
combined <- screen |> 
	## picking variables of interest
	dplyr::select(
		pid, Gender, sex, Age, age_cat, hiv_self_report, 
		MaritalStatus, education_level, country, counties_lived, 
		own_house, payRent, income_total, current_living, 
		smoke_current, alcohol, alcohol_sex, 
		drug_use, hysterectomy, menopause, pregnant, 
		contraceptive, 
		hpv_test:douching_with
	) |> 
	dplyr::left_join(gxp, by = "pid") |> 
	mutate(
		across(num_partner_cas:drug_sex_all, ~ case_when(
			.x >= 3 ~ "3+", 
			TRUE ~ as.character(.x)
		)), 
		hpv_hiv = case_when(
			hiv_self_report == "Negative" & hpv_result == "Negative" ~ "Negative", 	
			hiv_self_report == "Positive" & hpv_result == "Positive" ~ "HIV+HPV+", 	
			hiv_self_report == "Negative" & hpv_result == "Positive" ~ "HIV-HPV+", 	
			hiv_self_report == "Positive" & hpv_result == "Negative" ~ "HIV+HPV-", 	
		), 
		response = as.numeric(hpv_result == "Positive"), 
	) |> 
	dplyr::select(-pid) |>
	labelled::set_variable_labels(.labels = vars, .strict = FALSE)

## save processed dataset for further analysis
save(combined, vars, file = here::here("data", "hpv_processed.rda"))


## exploratory data analysis
# source(here::here("R", "03_eda.R"), echo = FALSE)


