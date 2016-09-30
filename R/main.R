library(tidyverse)
library(haven)
library(lme4)

# Clean the workspace
rm(list = ls())

source("R/read_params.R")
source("R/indicator.R")
source("R/process_data.R")

# params <- read_params()

# View(params[[2]])

df <- read_sas("data/alsfjord.sas7bdat")

indicator <- MacroAlgaeIndicator_CumulativeCover(df)
