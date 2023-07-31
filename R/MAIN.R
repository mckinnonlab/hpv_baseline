

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

## exploratory data analysis
source(here::here("R", "03_eda.R"), echo = FALSE)


