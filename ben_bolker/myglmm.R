
source("R/MacroAlgaeIndicator_CumulativeCover.R")
source("R/read_params.R")

res <- read_sas("data/alsfjord.sas7bdat") %>% 
  CumulativeCover_process_data() 

mod1 <- lmer(log_cumcover_mod ~ 
               (1 | kildestationsnavn) + 
               (1 | year) + 
               (1 | kildestationsnavn:year) + 
               (1 | proevetager), 
             data = res,
             REML = TRUE)

summary(mod1)
coef(mod1)

p <- read_params()[[1]]

no_year <- length(unique(res$year))

p$Estimate[p$CovParm == "year(vandomr*period)"] <- 
  p$Estimate[p$CovParm == "year(vandomr*period)"] * (1 - no_year / 6)

dd <- as.function(mod1)
(ff <- dd(sqrt(p$Estimate[1:4])))  ## new REML: 1704.708
environment(dd)$pp$beta(1)  ## new parameters

# ***************************************************************************
# The estimate of the intercept from the GLMM is 5.4589 with a variance of 0.1461.
# The indicator (L*beta) has an expectation value of 4.9868829217 and variance 0.149426779
# Using L_vector= {1 0.2 0.2 0.2 0.2 0.2 10 50 0};
# ***************************************************************************

