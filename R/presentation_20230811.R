
rm(list = ls())
library(tidyverse)

load(here::here("data", "hpv_processed.rda"))


combined <- 
	combined |> 
	mutate(
		age_cat = case_when(
			Age <= 20 ~ "≤20", 
			Age > 20 & Age <= 25 ~ "21-25", 
			Age > 25 & Age <= 30 ~ "25-30", 
			Age > 30 & Age <= 35 ~ "31-35", 
			Age > 35 & Age <= 40 ~ "36-40", 
			Age > 40 & Age <= 45 ~ "41-45", 
			Age > 45 ~ "46+",
			# Age > 45 & Age <= 50 ~ "46-50", 
			# Age > 50 ~ "51+"
		), 
		age_cat = factor(age_cat), 
		across(hr_hpv_16:hr_p5, ~ 1/.x), 
		across(hr_hpv_16:hr_p5, ~ if_else(is.infinite(.x), NA, .x)), 
		sex_work_yr = case_when(
			sex_work_yr >= 0 & sex_work_yr <= 3 ~ "0-3", 
			sex_work_yr > 3 & sex_work_yr <= 6 ~ "3-6", 
			sex_work_yr > 6 ~ "7+", 
		)
	) |> 
	labelled::set_variable_labels(.labels = vars, .strict = FALSE)

# total particpants
combined |> count(sex)
combined |> 
	filter(is.na(sex) | is.na(hpv_result)) |> 
	select(pid, hpv_result, sex) 
	
	

## theme management
theme_set(ggprism::theme_prism(base_size = 12))

theme_update(
	legend.position = "none", 
	strip.placement = "outside", 
	strip.text = element_text(size = 14)
)


# hpv prevalence by participant type --------------------------------------

combined |> 
	select(hpv_result, sex) |> 
	filter(!is.na(hpv_result), !is.na(sex)) |> 
	count(hpv_result, sex) |> 
	group_by(sex) |> 
	mutate(pct = n / sum(n), 
				 label = paste0(
				 	n, " (", scales::label_percent(accuracy = 0.1)(pct), ")"
				 )) |> 
	ggplot(aes(x = hpv_result, y = n)) + 
	geom_col(
		aes(fill = hpv_result)
	) + 
	scale_y_continuous(
		expand = c(.01, .01), 
		limits = c(0, 1000)
	) +
	scale_fill_manual(values = c("blue", "red")) + 
	geom_text(aes(label = label), vjust = -1, size = 4.5, fontface = "bold") +
	facet_wrap(~ sex, strip.position = "bottom") + 
	labs(
		x = "HPV Result", 
		y = "Frequency"
	) 
ggsave(here::here("output", "presentation", "plot_hpv_prevalence_by_type.png"), 
			 width = 8, height = 6)



# hpv genotype by cohort type ---------------------------------------------

combined |> 
	select(sex, hpv_result, hr_hpv_16:hr_p5) |> 
	filter(hpv_result == "Positive") |> 
	filter(!is.na(hpv_result), !is.na(sex)) |> 
	mutate(
		across(hr_hpv_16:hr_p5, ~ if_else(.x > 0, "Positive", "Negative"))
	) |> 
	pivot_longer(cols = hr_hpv_16:hr_p5, names_to = "genotype") |> 
	filter(value == "Positive") |> 
	count(sex, genotype) |> 
	mutate(N = if_else(sex == "FSW", 615, 32), 
				 pct = n / N, 
				 label = paste0(
				 	n, " (", scales::label_percent(accuracy = 0.1)(pct), ")"
				 )) |> 
	ggplot(aes(x = genotype, y = n)) + 
	geom_col(
		aes(fill = genotype), 
		# width = .5
	) + 
	ggrepel::geom_text_repel(
		aes(label = label), 
		vjust = -2, size = 4.5, fontface = "bold"
	) +
	scale_x_discrete(
		labels = paste0("P", 1:5)
	) + 
	scale_y_continuous(
		expand = c(.01, .01), 
		limits = c(0, 400)
	) + 
	facet_wrap(~ sex, strip.position = "bottom") + 
	labs(
		x = "HPV Genotype", 
		y = "Frequency"
	) 
ggsave(here::here("output", "presentation", "plot_hpv_genotype_by_type.png"), 
			 width = 10, height = 8)





# Age pyramid -------------------------------------------------------------

combined |> 
	mutate(
		HIV = hiv_self_report, 
		age_group = age_cat
	) |> 
	count(HIV, age_group, sex, hpv_result) |> 
	drop_na() |> 
	group_by(HIV, age_group, sex) |> 
	mutate(
		pct = n / sum(n), 
		label = paste0(
			scales::label_percent(accuracy = 0.1)(pct)
		), 
		label = if_else(hpv_result == "Positive" & sex == "FSW", label, NA)
	) |> 
	apyramid::age_pyramid(stack_by = hpv_result, count = n, show_midpoint = FALSE) + 
	scale_fill_manual(values = c("blue", "red")) + 
	labs(
		x = "", 
		fill = "HPV Result"
	) + 
	geom_label(aes(label = label), hjust = 3, fontface = "bold") + 
	facet_wrap(~ HIV, ncol = 2, 
						 labeller = label_both) + 
	ggprism::theme_prism(base_size = 12) + 
	theme(
		legend.title = element_text(face = "bold"), 
		legend.position = c(.9, .5)
	) 
ggsave(here::here("output", "presentation", "plot_age_pyramid.png"), 
			 width = 12, height = 6)




# genotype - ct values ----------------------------------------------------

fsw <- combined |> 
	filter(sex == "FSW") |> 
	mutate(
		Gender = forcats::fct_drop(Gender)
	) |> 
	labelled::set_variable_labels(.labels = vars, .strict = FALSE)


d <- combined |> 
	dplyr::filter(hpv_result == "Positive") |> 
	dplyr::select(hr_hpv_16:hr_p5, sex) |> 
	setNames(c(paste0("P", 1:5), "sex")) |> 
	tidyr::pivot_longer(cols = -sex) |> 
	dplyr::filter(!is.na(value)) |> 
	dplyr::filter(!is.na(sex)) |> 
	dplyr::mutate(value = if_else(value == 0, NA, value))

d |> 
	ggplot(aes(x = name, y = value)) + 
	geom_boxplot(
		aes(
			fill = name, 
			fill = after_scale(colorspace::lighten(fill, .5)),
			color = after_scale(colorspace::darken(fill, .5)),
		), 
		width = .5, 
		show.legend = FALSE
	) + 
	geom_point(
		aes(
			fill = name, 
			fill = after_scale(colorspace::lighten(fill, .5)),
			color = after_scale(colorspace::darken(fill, .5)),
		), 
		position = position_jitter(.15, seed = 123), 
		shape = 21, 
		size = 2.5, 
		stroke = .5,
		alpha = .6
	) + 
	geom_text(
		data =
			d |>
			dplyr::filter(!is.na(value)) |>
			dplyr::count(sex, name) |>
			dplyr::mutate(
				pct = n / sum(n),
				n = paste0("n=", n, " \n(", scales::label_percent(accuracy = .1)(pct), ")"),
				value = 0.07
			),
		aes(label = n), 
		fontface = "bold"
	) +
	labs(
		x = NULL, 
		y = "1/Ct value", 
	) + 
	facet_grid(~ sex)
ggsave(here::here("output", "presentation", "plot_ct_sex.png"), 
			 width = 12, height = 8)




# genotypes: 1/ct values by age and hiv status ----------------------------

d <- fsw |> 
	dplyr::filter(hpv_result == "Positive") |> 
	dplyr::select(hr_hpv_16:hr_p5, hiv_self_report, age_cat) |> 
	setNames(c(paste0("P", 1:5), "HIV", "Age")) |> 
	tidyr::pivot_longer(cols = -c(HIV, Age), names_to = "Genotype") |> 
	dplyr::filter(!is.na(value), !is.na(HIV)) |> 
	dplyr::mutate(value = if_else(value == 0, NA, value))

d |> 
	ggplot(aes(x = Age, y = value)) + 
	geom_boxplot(
		aes(
			fill = Age, 
			fill = after_scale(colorspace::lighten(fill, .5)),
			color = after_scale(colorspace::darken(fill, .5)),
		), 
		width = .5, 
		show.legend = FALSE
	) + 
	geom_point(
		aes(
			fill = Age, 
			fill = after_scale(colorspace::lighten(fill, .5)),
			color = after_scale(colorspace::darken(fill, .5)),
		), 
		position = position_jitter(.15, seed = 123), 
		shape = 21, 
		size = 2.5, 
		stroke = .5,
		alpha = .6
	) + 
	# geom_text(
	# 	data = 
	# 		d |> 
	# 		dplyr::filter(!is.na(value)) |> 
	# 		dplyr::count(HIV, Age, Genotype) |> 
	# 		dplyr::group_by(HIV, Age) |> 
	# 		dplyr::mutate(
	# 			pct = n / sum(n), 
	# 			n = paste0("(n=", n, ", ", scales::label_percent(accuracy = .1)(pct), ")"),
	# 			value = 0.06
	# 		), 
	# 	aes(label = n), 
	# 	fontface = "bold", 
	# 	size = 2,
	# 	angle = 90
	# ) + 
	geom_smooth(
		aes(group = Genotype, color = Genotype), 
		se = FALSE
	) + 
	labs(
		x = "Age", 
		y = "1/ct value", 
	) + 
	theme_bw() + 
	theme(
		legend.position = "none", 
		plot.title = element_text(face = "bold"), 
		axis.title = element_text(face = "bold")
	) + 
	facet_grid(HIV ~ Genotype, labeller = label_both)

ggsave(here::here("output", "presentation", "plot_ct_age_hiv.png"), 
			 width = 14, height = 8)



# genotype channel combined + HIV -----------------------------------------

fsw2 <- 
	fsw |> 
	dplyr::mutate(
		across(hr_hpv_16:hr_p5, 
					 ~ if_else(
					 	hpv_result == "Positive" & !is.na(.x), "Yes", "No" 
					 )), 
		hpv_combined = (hr_hpv_16 == "Yes") + (hr_hpv_18_45 == "Yes") + 
			(hr_p3 == "Yes") + (hr_p4 == "Yes") + (hr_p5 == "Yes"), 
		hpv_combined = if_else(hpv_combined == 0, NA, hpv_combined)
	) |> 
	labelled::set_variable_labels(.labels = vars, .strict = FALSE)



fsw2 |> 
	mutate(HIV = hiv_self_report, 
				 hpv_combined = factor(hpv_combined)) |> 
	count(hpv_combined, HIV) |> 
	drop_na() |> 
	group_by(HIV) |> 
	mutate(
		pct = n / sum(n), 
		label = paste0(n, "\n(", scales::label_percent(accuracy = 0.1)(pct), ")")
	) |> 
	ggplot(aes(x = hpv_combined, y = pct)) + 
	geom_col(
		aes(
			fill = hpv_combined, 
			fill = after_scale(colorspace::lighten(fill, .5)),
			color = after_scale(colorspace::darken(fill, .5)),
		), 
		width = .5, 
		show.legend = FALSE
	) + 
	scale_y_continuous(
		expand = c(.01, .01), 
		limits = c(0, .8),
		labels = scales::label_percent()
	) + 
	geom_text(
		aes(label = label), 
		fontface = "bold", 
		vjust = -.5
	) + 
	labs(
		x = NULL, 
		y = "Percentage"
	) + 
	facet_wrap(~ HIV, nrow = 1,  
						 strip.position = "bottom", 
						 labeller = label_both)

ggsave(here::here("output", "presentation", "plot_hpv_combined_HIV.png"), 
			 width = 14, height = 8)



d <- fsw2 |> 
	select(pid, hiv_self_report, hr_hpv_16:hr_p5) |> 
	pivot_longer(cols = -c(pid, hiv_self_report)) |> 
	filter(value == "Yes") |> 
	mutate(
		genotype = case_when(
			name == "hr_hpv_16" ~ "P1",
			name == "hr_hpv_18_45" ~ "P2",
			name == "hr_p3" ~ "P3",
			name == "hr_p4" ~ "P4",
			name == "hr_p5" ~ "P5",
		)
	) 

d |> 
	select(pid, hiv_self_report, genotype) |> 
	pivot_wider(id_cols = c(pid, hiv_self_report), 
							names_from = "genotype", values_from = "genotype") |> 
	unite(genotype, P1, P2, P3, P4, P5, na.rm = TRUE, sep=",") |> 
	mutate(genotype = fct_infreq(genotype), 
				 genotype = fct_lump(genotype, 10)) |> 
	gtsummary::tbl_summary(
		include = genotype,
		by = hiv_self_report, 
		label = list(
			genotype ~ "Genotype combination"
		)
	) |> 
	gtsummary::add_overall() |> 
	gtsummary::bold_labels() |> 
	gtsummary::modify_header(
		"stat_2" ~ "**HIV: Positive**, N = {n}"
	) |> 
	gtsummary::modify_column_hide("stat_1")



# Tables ------------------------------------------------------------------


source(here::here("R", "utils.R"), echo = FALSE)
gtsummary::tbl_merge(
	tbls = list(
		fsw |> 
			dplyr::select(
				Gender, Age, age_cat, hiv_self_report
			) |> 
			table1_total(missing = "no", add_n = FALSE), 
		fsw |> 
			dplyr::select(
				Gender, Age, age_cat, 
				hpv_result, hiv_self_report
			) |> 
			table1_by(by = hpv_result, byname = "HPV Result", 
								missing = "no",
								percent = "row", 
								add_N = FALSE, 
								add_total = FALSE)
	), 
	tab_spanner = c(NA, "**HPV Result**")
)

				
gtsummary::tbl_merge(
	tbls = list(
		fsw |> 
			dplyr::select(
				education_level, hpv_test, condom_access, first_sex_when, sex_work_yr
			) |> 
			table1_total(missing = "no", add_n = FALSE), 
		fsw |> 
			dplyr::select(
				education_level, hpv_test, condom_access, first_sex_when, sex_work_yr,
				hpv_result
			) |> 
			table1_by(by = hpv_result, byname = "HPV Result", 
								missing = "no",
								percent = "row", 
								add_N = FALSE, 
								add_total = FALSE)
	), 
	tab_spanner = c(NA, "**HPV Result**")
)



mod_lm <- glm(
	response ~ Gender + Age + hiv_self_report +
		hpv_test + condom_access + sex_work_yr,
	family = binomial, 
	data = fsw
) 

mod_lm |> 
	gtsummary::tbl_regression(
		exponentiate = TRUE
	)

mod_lm |> 
	broom::tidy(conf.int = TRUE, exponentiate = TRUE) |> 
	slice(-1) |> 
	ggplot(aes(x = estimate, y = term)) +
	geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
	geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = 
								 	.2, color = "gray50") +
	geom_point(aes(size = estimate), shape = 15) +
	scale_x_continuous(
		breaks = seq(-1, 6)
	) + 
	labs(
		y = NULL, 
		x = "Odds Ratio"
	)







combined |> 
	select(hpv_result, hiv_self_report) |> 
	filter(!is.na(hpv_result), !is.na(hiv_self_report)) |> 
	count(hpv_result, hiv_self_report) |> 
	group_by(hiv_self_report) |> 
	mutate(pct = n / sum(n), 
				 hiv_self_report = paste("HIV: ", hiv_self_report), 
				 label = paste0(
				 	n, " (", scales::label_percent(accuracy = 0.1)(pct), ")"
				 )) |> 
	ggplot(aes(x = hpv_result, y = pct)) + 
	geom_col(
		aes(fill = hpv_result), 
	) + 
	scale_y_continuous(
		expand = c(.01, .01), 
		limits = c(0, 1), 
		labels = scales::label_percent()
	) +
	scale_fill_manual(values = c("blue", "red")) + 
	geom_text(aes(label = label), vjust = -1, size = 4.5, fontface = "bold") +
	facet_wrap(~ hiv_self_report) + 
	labs(
		x = "HPV Result", 
		y = "Percentage"
	) 
ggsave(here::here("output", "presentation", "plot_hpv_prevalence_by_hiv.png"), 
			 width = 8, height = 6)
