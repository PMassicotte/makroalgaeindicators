#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Function used to calculate indicators. Only takes 1 parameter
#               which specify which indicator should be calculated.
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

get_indicator <- function(which = c("cumcover", "fraction", "succession")) {

  # ***************************************************************************
  # glmm models
  # 
  # Find which package to use
  # 1. Gaussian distribution
  # 2. Allow fixed parameter value
  # ***************************************************************************
  
  # Look at glmer from lme4
  # https://mixedpsychophysics.wordpress.com/r-code-a-test/fitting-a-glmm-with-lme4-basic-syntax/
  
}