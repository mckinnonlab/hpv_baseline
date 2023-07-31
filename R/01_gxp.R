



## regular expression 
grep("Screenform", files, value = TRUE)

raw_gxp <- readxl::read_excel(grep("GeneXpert", files, value = TRUE))


message("Processing geneXpert dataset ... ")

## |> pipe 
gxp <- raw_gxp |> 
	janitor::clean_names() |> 
	dplyr::select(pid:hr_p5) |> 
	## remove missing values in hpv result
	dplyr::filter(!is.na(hpv_result))


gxp |> 
	dplyr::filter(is.na(hpv_result)) 

