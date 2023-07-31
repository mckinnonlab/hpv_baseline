


sheets <- c(
	"April_May", 
	"Freq Vl", 
	"Counts"
)


ds <- combined |> 
	arrange(pid, desc(hpv_result)) |> 
	distinct(pid, .keep_all = TRUE)
	


raw <- readxl::read_excel(grep("HPVFlow", files, value = TRUE), 
									 sheet = "April_May")
raw |> 
	mutate(
		pid = `TUBE NAME`, 
	) |> 
	left_join(ds, by = "pid") |> 
	rio::export(here::here("output", "combined_flow_baseline_SHEET_April_May.xlsx"))


raw <- readxl::read_excel(grep("HPVFlow", files, value = TRUE), 
													sheet = "Freq Vl")
raw |> 
	mutate(
		pid = `...1`, 
	) |> 
	left_join(ds, by = "pid") |> 
	rio::export(here::here("output", "combined_flow_baseline_SHEET_Freq_Vl.xlsx"))


raw <- readxl::read_excel(grep("HPVFlow", files, value = TRUE), 
													sheet = "Counts")
raw |> 
	mutate(
		pid = `PID`, 
		pid = gsub(" ", "", pid)
	) |> 
	# select(pid, PID)
	left_join(ds, by = "pid") |> 
	rio::export(here::here("output", "combined_flow_baseline_SHEET_Counts.xlsx"))



combined |> 
	rio::export(here::here("output", "combined_baseline_demographics.xlsx"))
