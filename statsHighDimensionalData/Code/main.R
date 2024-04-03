# Set working directories -------------------------------------------------
remove(list=ls())

if (Sys.getenv("USERNAME") == "..."){
  main_dir <- "..."
} else if (Sys.getenv("USERNAME") == "..."){
  main_dir <- "..."
} 

setwd(main_dir)
data_dir <- paste0(main_dir, "/Data")
code_dir <- paste0(main_dir, "/Code")
results_dir <- paste0(main_dir, "/Results")


# Load packages -----------------------------------------------------------
library(tidyverse)
library(gt)
librarr(reshape2)

# Run other files ---------------------------------------------------------
source(paste0(code_dir, "/analysis.R"))
source(paste0(code_dir, "/tables_and_figures.R"))
