


library(gtsummary)


# combined |> 
# 	head() |> 
# 	rio::export(here::here("output", "variables.xlsx"))

library(officer)
sect_properties <- prop_section(
	page_size = page_size(
		orient = "portrait",
		width = 11, height = 20
	),
	type = "continuous",
	page_margins = page_mar()
)

combined |> 
	dplyr::select(-pid) |> 
	mutate(
		across(num_partner_cas:drug_sex_all, ~ case_when(
			.x >= 3 ~ "3+", 
			TRUE ~ as.character(.x)
		))
	) |> 
	labelled::set_variable_labels(.labels = vars, .strict = FALSE) |> 
	gtsummary::tbl_summary(
		by = hpv_result, 
		percent = "row", 
		type = list(
			num_partner_cas:drug_sex_all ~ "categorical"
		)
	) |> 
	gtsummary::add_p() |> 
	gtsummary::add_overall() |> 
	gtsummary::add_n() |> 
	gtsummary::as_flex_table() |> 
	flextable::save_as_docx(
		path = here::here("output", "table1.docx"), pr_section = sect_properties
	)


