read_params <- function() {
  
  # covparams_CumCover <- haven::read_sas("data/covparms_cumcover.sas7bdat")
  # parmest_CumCover <- haven::read_sas("data/parmest_cumcover.sas7bdat")
  # covparams_PropOpportunist <- haven::read_sas("data/covparms_filaandel.sas7bdat")
  # parmest_PropOpportunist <- haven::read_sas("data/parmest_filaandel.sas7bdat")
  # covparams_NPerennials <- haven::read_sas("data/covparms_narter_perennial.sas7bdat")
  # parmest_NPerennials <- haven::read_sas("data/parmest_narter_perennial.sas7bdat")
  
  data("covparams_CumCover")
  data("parmest_CumCover")
  data("covparams_PropOpportunist")
  data("parmest_PropOpportunist")
  data("covparams_NPerennials")
  data("parmest_NPerennials")
   
  res <- list(
    covparams_CumCover = covparams_CumCover,
    parmest_CumCover = parmest_CumCover,
    covparams_PropOpportunist = covparams_PropOpportunist,
    parmest_PropOpportunist = parmest_PropOpportunist,
    covparams_NPerennials = covparams_NPerennials,
    parmest_NPerennials = parmest_NPerennials
  )
  
  return(res)
  
}