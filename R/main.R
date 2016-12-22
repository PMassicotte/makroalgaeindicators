library(tidyverse)
library(haven)
library(lme4)

library(extrafont)
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

# source("R/extract_modis.R") # ATTENTION: takes few minutes

# Clean the workspace
rm(list = ls())

source("R/read_params.R")
source("R/process_data.R")
source("R/indicator.R")

df <- read_sas("data/alsfjord.sas7bdat")

df <- process_data(df)

indicator <- MacroAlgaeIndicator_CumulativeCover(df)

# write_csv(indicator, "/home/pmassicotte/Desktop/data_ready.csv")
