

table1_total <- function(data, 
												 missing = c("no", "ifany", "always"),
												 percent = c("column", "row"), 
												 add_n = TRUE, ...) {
	tbl <- data |> 
		gtsummary::tbl_summary(
			statistic = list(
				gtsummary::all_continuous2() ~ "{median} ({p25} - {p75})",
				gtsummary::all_categorical() ~ "{n} ({p})"
			),
			digits = list(
				gtsummary::all_continuous() ~ 1,
				gtsummary::all_categorical() ~ c(0, 1)
			),
			missing = missing[1],
			missing_text = "Missing",
			percent = percent[1],
			...
		) %>%
		gtsummary::modify_header(
			all_stat_cols() ~ "**{level}**, N = {n}"
		) |> 
		gtsummary::bold_labels()
	
	if (add_n) {
		tbl <- tbl %>% 
			gtsummary::add_n()
	} else {
		tbl	
	}
	return(tbl)
} 



table1_by <- function(data, by,
														byname = "Example",
														missing = c("no", "ifany", "always"),
														percent = c("column", "row"),
														add_N = TRUE,
														add_total = TRUE,
														add_pvalue = TRUE, ...) {
	tbl <- data %>%
		gtsummary::tbl_summary(
			by = {{by}},
			statistic = list(
				gtsummary::all_continuous2() ~ "{median} ({p25} - {p75})",
				gtsummary::all_categorical() ~ "{n} ({p})"
			),
			digits = list(
				gtsummary::all_continuous() ~ 1,
				gtsummary::all_categorical() ~ c(0, 1)
			),
			# type = type,
			missing = missing[1],
			missing_text = "Missing",
			percent = percent[1], ...
		) %>%
		gtsummary::modify_header(
			all_stat_cols() ~ "**{level}**, N = {n} ({style_percent(p, digits = 1)}%)"
		) %>%
		gtsummary::bold_labels()
	
	header <- 
		gtsummary::show_header_names(tbl, quiet = TRUE) |>  
		dplyr::pull("column")
	header <- header[grepl("stat_", header) & header != "stat_0"]
	
	tbl <- tbl %>%
		gtsummary::modify_spanning_header(header ~ sprintf("**%s**", byname))
	
	if (add_N) tbl <- tbl %>% gtsummary::add_n()
	if (add_total) {
		tbl <- tbl %>%
			gtsummary::add_overall(
				last = FALSE,
				statistic = list(
					gtsummary::all_continuous() ~ "{median} ({p25} - {p75})",
					gtsummary::all_categorical() ~ "{n} ({p})"
				),
				digits = list(
					gtsummary::all_continuous() ~ 1,
					gtsummary::all_categorical() ~ c(0, 1)
				)
			)
	}
	if (add_pvalue) {
		tbl <- tbl %>%
			# rounding p-values to 3 decimal places
			gtsummary::add_p(
				pvalue_fun = function(x) gtsummary::style_number(x, digits = 3)
			)
	}
	
	return(tbl)
}