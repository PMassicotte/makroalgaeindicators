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
source("R/modified_lmer.R")
source("R/MacroAlgaeIndicator_CumulativeCover.R")

df <- read_sas("data/alsfjord_2013_2016.sas7bdat")
df <- read_sas("data/alsfjord_2007_2012.sas7bdat")
df <- read_sas("data/alsfjord_2001_2006.sas7bdat")
df <- read_sas("data/dybsoefjord_2001_2006.sas7bdat")

#Examples
indicator <- MacroAlgaeIndicator("CumulativeCover",df,boundaries=c(74.4,40.8,20.4,13.8),depth_cutoff=1)
indicator <- MacroAlgaeIndicator("PropOpportunist",df,boundaries=c(0.25,0.40,0.60,0.80),depth_cutoff=1)
indicator <- MacroAlgaeIndicator("NPerennials",df,boundaries=c(10,8,5,2),depth_cutoff=1)
