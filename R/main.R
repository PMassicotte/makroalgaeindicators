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
source("R/MacroAlgaeIndicator_CumulativeCover.R")

df <- read_sas("data/alsfjord.sas7bdat")

indicator <- MacroAlgaeIndicator_CumulativeCover(df)
