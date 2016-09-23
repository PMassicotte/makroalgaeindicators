read_params <- function() {
  
  covparams <- haven::read_sas("data/covparms_cumcover.sas7bdat")
  parmest <- haven::read_sas("data/parmest_cumcover.sas7bdat")
  
  res <- list(
    covparams = covparams,
    parmest = parmest
  )
  
  return(res)
  
}