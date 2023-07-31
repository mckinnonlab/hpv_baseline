


raw_screen <- readxl::read_excel(grep("Screenform", files, value = TRUE))


screen <- raw_screen %>% 
	## remove identifiers
	select(-`Respodent Name`) %>%
	## pid
	rename(
		pid = `TSH#`, 
	) %>% 
	mutate(
		## fix pid 
		pid = case_when(
			nchar(pid) == 4 ~ paste0("TSH", pid),
			nchar(pid) == 3 ~ paste0("TSH0", pid),
			nchar(pid) == 2 ~ paste0("TSH00", pid),
			nchar(pid) == 1 ~ paste0("TSH000", pid),
			TRUE ~ pid
		),
		## screening date
		screen_date = lubridate::dmy(ScreenDate), 
		screen_mnth = lubridate::month(screen_date, label = TRUE), 
		screen_qtr = lubridate::quarter(screen_date), 
		screen_year = lubridate::year(screen_date), 
		screen_week = lubridate::week(screen_date), 
		
		## marital status 
		MaritalStatus = case_when(
			MaritalStatus %in% c(1) ~ "Single", 
			MaritalStatus %in% c(2, 3) ~ "Having partners", 
			MaritalStatus %in% c(4:7) ~ "Married", 
			TRUE ~ NA_character_
		),
		MaritalStatus = factor(MaritalStatus, levels = c("Single", "Having partners", "Married")), 
		education_level = case_when(
			education_level %in% c(1) ~ "No", 
			education_level %in% c(2, 3) ~ "Primary", 
			education_level %in% c(4, 5) ~ "Secondary", 
			education_level %in% c(6:10) ~ "Vocational and above", 
			TRUE ~ NA_character_
		), 
		
		## education level 
		education_level = factor(education_level, levels = c(
			"No", "Primary", "Secondary", "Vocational and above"
		)),
		schoolYears = gsub("[^0-9.-]", "", schoolYears),
		schoolYears = as.numeric(schoolYears),
		
		## county data 
		country = factor(country, levels = 1:2, labels = c("Kenya", "Other")), 
		counties_lived = ifelse(counties_lived > 1, "2+", counties_lived),
		
		## current living places 
		current_living = case_when(
			current_living_a == 1 ~ "Alone", 
			current_living_b == 1 ~ "family", 
			current_living_c == 1 ~ "friends", 
			current_living_d == 1 ~ "friends", 
			current_living_e == 1 ~ "Sex Partners", 
			current_living_f == 1 ~ "Sex Partners", 
			current_living_g == 1 ~ "family", 
			current_living_h == 1 ~ "family", 
			current_living_i == 1 ~ "Other", 
			TRUE ~ NA_character_
		),
		current_living = factor(current_living, levels = c(
			"Alone", "family", "friends", "Sex Partners", "Other"
		)),
		
		## own house
		own_house = recode(own_house, `1` = "Yes", `2` = "No"), 
		payRent = case_when(
			payRent == 1 ~ "Yes", 
			payRent == 2 ~ "No", 
			TRUE ~ NA_character_
		),
		
		## income 
		income_num = rowSums(across(income_a:income_m), na.rm = TRUE), 
		income_num = ifelse(income_num > 2, "3+", income_num), 
		income_other = rowSums(across(income_b:income_m), na.rm = TRUE), 
		income_source = case_when(
			income_a == 1 & income_other == 0 ~ "Sex work", 
			income_a == 1 & income_other > 0 ~ "Sex work + others", 
			is.na(income_a) & income_other > 0 ~ "Others", 
			TRUE ~ NA_character_
		), 
		income_source = factor(income_source, levels = c(
			"Sex work", "Sex work + others", "Others"
		)), 
		across(income_a:income_m, ~ ifelse(is.na(.x), "+", .x)),
		income_a = ifelse(income_a == 1, "Sex work", ""),
		income_b = ifelse(income_b == 1, "Hairdresser/beautician/masseuse", ""),
		income_c = ifelse(income_c == 1, "Shop worker", ""),
		income_d = ifelse(income_d == 1, "Street vendor/casual labourer", ""),
		income_e = ifelse(income_e == 1, "Business", ""),
		income_f = ifelse(income_f == 1, "Construction worker", ""),
		income_g = ifelse(income_g == 1, "Factory worker", ""),
		income_h = ifelse(income_h == 1, "Farming/agricultural worker", ""),
		income_i = ifelse(income_i == 1, "Government worker", ""),
		income_j = ifelse(income_j == 1, "Tourism/travel agent/tour guide", ""),
		income_k = ifelse(income_k == 1, "Waiter-waitress/bartender/hotel employee", ""),
		income_l = ifelse(income_l == 1, "Parental/spousal support", ""),
		income_m = ifelse(income_m == 1, "Other", ""),
		income_source2 = paste(income_a, income_b, income_c, income_d, income_e,
													 income_f, income_g, income_h, income_i, income_j,
													 income_k, income_l, income_m, sep = " + "),
		
		
		income_total = as.numeric(gsub("([0-9]+).*$", "\\1", income_total)), 
		lowest_rate = as.numeric(lowest_rate), 
		highest_rate = as.numeric(highest_rate), 
		
		## venues 
		venue_num = rowSums(across(venue_a:venue_q), na.rm = TRUE), 
		venue_num = ifelse(venue_num > 2, "3+", venue_num), 
		venue_other1 = rowSums(across(venue_a:venue_c), na.rm = TRUE), 
		venue_other2 = rowSums(across(venue_e:venue_q), na.rm = TRUE), 
		venue_other = venue_other1 + venue_other2, 
		venue_source = case_when(
			venue_d == 1 & venue_other == 0 ~ "Bars/nightclubs", 
			venue_d == 1 & venue_other > 0 ~ "Bars/nightclubs + others", 
			is.na(venue_d) & venue_other > 0 ~ "Others", 
			TRUE ~ NA_character_
		), 
		venue_other = ifelse(venue_other > 1, "2+", venue_other), 
		across(venue_a:venue_q, ~ ifelse(is.na(.x), "+", .x)),
		venue_a = ifelse(venue_a == 1, "Hotel/brothel/Guesthouse/lodging", ""), 
		venue_b = ifelse(venue_b == 1, "Massage parlours", ""), 
		venue_c = ifelse(venue_c == 1, "Home-based", ""), 
		venue_d = ifelse(venue_d == 1, "Bars/nightclubs", ""), 
		venue_e = ifelse(venue_e == 1, "Street-based", ""), 
		venue_f = ifelse(venue_f == 1, "Virutal platforms", ""), 
		venue_g = ifelse(venue_g == 1, "Injecting den", ""), 
		venue_h = ifelse(venue_h == 1, "Unihibitable building", ""), 
		venue_i = ifelse(venue_i == 1, "Parks", ""), 
		venue_j = ifelse(venue_j == 1, "Beach", ""), 
		venue_k = ifelse(venue_k == 1, "Casino", ""), 
		venue_l = ifelse(venue_l == 1, "Barbershop/Salon", ""), 
		venue_m = ifelse(venue_m == 1, "Sex den", ""), 
		venue_n = ifelse(venue_n == 1, "Strip club", ""), 
		venue_o = ifelse(venue_o == 1, "Highways", ""), 
		venue_p = ifelse(venue_p == 1, "Chang’aa den", ""), 
		venue_q = ifelse(venue_q == 1, "Other", ""), 
		venue_source2 = paste(venue_a, venue_b, venue_c, venue_d, venue_e, venue_f, 
													venue_g, venue_h, venue_i, venue_j, venue_k, venue_l, 
													venue_m, venue_n, venue_p, venue_q, sep = "+"), 
	) %>%
	rename(
	) %>% 
	mutate(
		## smoking history 
		smoke_ever = ifelse(Q18 == 2, 0, 1), 
		smoke_current = ifelse(Q19 == 2, 0, 1),   
		smoke_shisha = ifelse(Q20 == 2, 0, 1), 
		smoke_other = ifelse(Q21 == 2, 0, 1),
		
		## alcohol drinking in the last 2 months 
		alcohol = factor(Q22, labels = c(
			"Never", "Monthly or less", "weekly", "2-3 times per week", "4+ times per week"
		)), 
		alcohol_sex = factor(Q23, 4:1, c("Never", "Sometimes", "Often", "Always")), 
		alcohol_vomit = factor(Q24, 4:1, c("Never", "Sometimes", "Often", "Always")),
		
		## drug use 
		drug_use = ifelse(Q25 == 2, 0, 1),
		## list of drugs 
		across(Q26:Q26C, ~ ifelse(.x == 1, .x, 0)), 
		Q26C = ifelse(Q26C == 1, 5),
		drug_num = rowSums(across(Q26:Q26C), na.rm = TRUE),
		drug_num = ifelse(drug_num == 5, 1, drug_num), 
		drug_num = ifelse(drug_num > 1, "2+", drug_num),
		## inject drugs 
		inject = ifelse(Q27 == 2, 0, 1), 
		## share needle
		share_needle = ifelse(Q29 == 2, 0, 1),
		
		## long-term steriod 
		steriod = ifelse(Q30 == 2, 0, 1), 
		steriod = ifelse(Q31 == 2, 0, 1), 
		hysterectomy = ifelse(Q33 == 2, 0, 1), 
		
		## reproductive history 
		menopause = ifelse(Q34 == 2, 0, 1), 
		pregnant = ifelse(Q35 == 2, 0, 1), 
		child_alive = rowSums(across(c(Q37A, Q37B)), na.rm = TRUE), 
		child_dead = rowSums(across(c(Q38A, Q38B)), na.rm = TRUE), 
		abortion = rowSums(across(c(Q39A, Q39B)), na.rm = TRUE),  
		## TODO:: age last child has age < 14 ::: check later      <<< ========
		age_lastchild = as.numeric(Q40),
		## contraceptive history
		contraceptive = case_when(
			Q41 == 1 ~ "None", 
			Q41 == 2 ~ "Safe days", 
			Q41 == 3 ~ "Oral/Pills", 
			Q41 == 4 ~ "IUD", 
			Q41 == 5 ~ "Depo Provera Injection", 
			Q41 == 6 ~ "Tubal Ligation", 
			Q41 == 7 ~ "Male Condom", 
			Q41 == 8 ~ "Female Condom", 
			Q41 == 9 ~ "Diaphragm", 
			Q41 == 10 ~ "Spermicide", 
			Q41 == 11 ~ "Vasectomy", 
			Q41 == 12 ~ "Implant", 
			TRUE ~ NA_character_
		), 
		contraceptive = factor(contraceptive),
		contraceptive = relevel(contraceptive, "None"), 
		
		## HIV status
		hiv_test = ifelse(Q42 == 2, 0, 1),
		hiv_test_lastyr = ifelse(Q43 > 0 & Q43 <= 3, "1-3", ifelse(
			Q43 > 3, "4+", Q43
		)), 
		hiv = ifelse(Q44 == 2, 0, 1), 
		art = ifelse(Q45 == 2, 0, 1),
		art_place = factor(Q46, labels = c("SWOP", "Other")), 
		
		## HPV history
		hpv_test = ifelse(Q48 == 2, 0, 1), 
		hpv_vaccine = ifelse(Q49 == 2, 0, 1), 
		anal_cancer_test = ifelse(Q50 == 2, 0, 1), 
		Q51 = ifelse(Q51 == 5, NA, Q51), 
		anal_cancer_test_when = factor(Q51, labels = c(
			"<1 year", "1-2 years", "3-5 years", ">5 years"
		)), 
		
		## TODO:: do wordcloud vis for main reasons Q53 Q58
		hpv_last_test = ifelse(Q54 == 2, 1, 0), 
		
		## sex practices 
		condom_lastsex = ifelse(Q60 == 2, 0, 1), 
		condom_access = ifelse(Q61 == 2, 0, 1),
		## age or year at sex debut: use year
		Age = as.numeric(Age),
		first_sex_when = case_when(
			Q62 == 20004 ~ 2004,
			nchar(Q62) == 4 ~ 2022 - Q62,
			nchar(Q62) <= 2 ~ Age - Q62, 
			TRUE ~ Q62 
		), 
		first_sex_when = ifelse(first_sex_when < 0, NA, first_sex_when), 
		sex_work_yr = case_when(
			Q63 == 20004 ~ 2004,
			nchar(Q63) == 4 ~ 2022 - Q63,
			nchar(Q63) <= 2 ~ Age - Q63, 
			TRUE ~ Q63 
		), 
		num_partner_cas = Q65A,
		num_partner_rep = Q65B,
		num_partner_reg = Q65C,
		num_partner_hiv_dk = Q66, 
		num_partner_hiv_cas = Q66A, 
		num_partner_hiv_rep = Q66B, 
		num_partner_hiv_reg = Q66C, 
		drug_sex_cas = ifelse(Q67A == 2, 0, 1),
		drug_sex_rep = ifelse(Q67B == 2, 0, 1),
		drug_sex_reg = ifelse(Q67C == 2, 0, 1),
		drug_sex_all = as.numeric(Q67A == 1 & Q67B == 1 & Q67C == 1),   
		douching = ifelse(Q69 == 2, 0, 1),
		douching_with = ifelse(Q70 == 1, "water", "other"), 
		douching_with = factor(douching_with, labels = c("water", "other")), 
		
		vio_none = ifelse(is.na(Q71A), 0, Q71A), 
		vio_physical = ifelse(is.na(Q71B), 0, Q71B), 
		vio_emotional = ifelse(is.na(Q71C), 0, Q71C), 
		vio_psychological = ifelse(is.na(Q71D), 0, Q71D), 
		vio_spiritual = ifelse(is.na(Q71E), 0, Q71E), 
		vio_cultural = ifelse(is.na(Q71F), 0, Q71F), 
		vio_verbal = ifelse(is.na(Q71G), 0, Q71G), 
		vio_financial = ifelse(is.na(Q71H), 0, Q71H), 
		vio_neglect = ifelse(is.na(Q71I), 0, Q71I), 
		vio_person_client = Q72A, 
		vio_person_husband = Q72B, 
		vio_person_sexworker = Q72C, 
		vio_person_police = Q72D, 
		vio_person_venue = Q72E, 
		vio_person_neighbor = Q72F, 
		vio_person_other = Q72G, 
		vio_sexwork = as.numeric(Q73 == 1), 
		vio_when = factor(Q74, labels = c(
			"<1 month", "1-6 months", "6-12 months", ">12 months" 
		)), 
		vio_sexual = as.numeric(Q75 == 1), 
		
		vio_sexual_client = Q76A, 
		vio_sexual_husband = Q76B, 
		vio_sexual_sexworker = Q76C, 
		vio_sexual_police = Q76D, 
		vio_sexual_venue = Q76E, 
		vio_sexual_neighbor = Q76F, 
		vio_sexual_other = Q76G, 
		vio_sexual_sexwork = as.numeric(Q77 == 1), 
		vio_sexual_when = factor(Q78, labels = c(
			"<1 month", "1-6 months", "6-12 months", ">12 months" 
		)), 
		vio_counsel = as.numeric(Q79 == 1), 
		arrested = as.numeric(Q80 == 1), 
		arrested_sexwork = as.numeric(Q81 == 1), 
		
		
		age_cat = case_when(
			Age < 30 ~ "<30", 
			Age >= 30 ~ "30+", 
			TRUE ~ NA_character_
		), 
		sex = case_when(
			Gender %in% c(2, 4) ~ "FSW", 
			Gender %in% c(1, 3) ~ "MSM", 
			TRUE ~ NA_character_
		),
		Gender = case_when(
			Gender == 1 ~ "cis-Male", 
			Gender == 2 ~ "cis-Female", 
			Gender == 3 ~ "trans-Male", 
			Gender == 4 ~ "trans-Female", 
			Gender == 5 ~ "Prefer not to answer", 
			TRUE ~ NA_character_
		), 
		Gender = factor(Gender, c(
			"cis-Female", "trans-Female", "cis-Male", "trans-Male", "Prefer not to answer"
		)), 
		hiv_sym = case_when(
			Q44 == 1 ~ "HIV+", 
			Q44 == 2 ~ "HIV-", 
			TRUE ~ NA_character_
		), 
		hiv_self_report = case_when(
			Q44 == 1 ~ "Positive", 
			Q44 == 2 ~ "Negative", 
			TRUE ~ NA_character_
		), 
		combined = paste(sex, age_cat, hiv_sym), 
		combined = ifelse(grepl("NA", combined), NA, combined),
	) 



