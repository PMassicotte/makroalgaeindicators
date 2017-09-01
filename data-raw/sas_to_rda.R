library(tidyverse)

CumulativeCover <- haven::read_sas("inst/extdata/parmest_cumcover_in.sas7bdat")
devtools::use_data(CumulativeCover, overwrite = TRUE)

PropOpportunist <- haven::read_sas("inst/extdata/parmest_filaandel_in.sas7bdat")
devtools::use_data(PropOpportunist, overwrite = TRUE)

NPerennials <- haven::read_sas("inst/extdata/parmest_narter_perennial_in.sas7bdat")
devtools::use_data(NPerennials, overwrite = TRUE)


# covb --------------------------------------------------------------------

covb_cumcover_in <- as.matrix(read_sas("inst/extdata/covb_cumcover_in.sas7bdat"))
devtools::use_data(covb_cumcover_in, overwrite = TRUE)

covb_filaandel_in <- as.matrix(read_sas("inst/extdata/covb_filaandel_in.sas7bdat"))
devtools::use_data(covb_filaandel_in, overwrite = TRUE)

covb_narter_perennial_in <- as.matrix(read_sas("inst/extdata/covb_narter_perennial_in.sas7bdat"))
devtools::use_data(covb_narter_perennial_in, overwrite = TRUE)


# Params ------------------------------------------------------------------

covparams_CumCover <- haven::read_sas("inst/extdata/covparms_cumcover.sas7bdat")
devtools::use_data(covparams_CumCover, overwrite = TRUE)

parmest_CumCover <- haven::read_sas("inst/extdata/parmest_cumcover.sas7bdat")
devtools::use_data(parmest_CumCover, overwrite = TRUE)

covparams_PropOpportunist <- haven::read_sas("inst/extdata/covparms_filaandel.sas7bdat")
devtools::use_data(covparams_PropOpportunist, overwrite = TRUE)

parmest_PropOpportunist <- haven::read_sas("inst/extdata/parmest_filaandel.sas7bdat")
devtools::use_data(parmest_PropOpportunist, overwrite = TRUE)

covparams_NPerennials <- haven::read_sas("inst/extdata/covparms_narter_perennial.sas7bdat")
devtools::use_data(covparams_NPerennials, overwrite = TRUE)

parmest_NPerennials <- haven::read_sas("inst/extdata/parmest_narter_perennial.sas7bdat")
devtools::use_data(parmest_NPerennials, overwrite = TRUE)


# Test datasets -----------------------------------------------------------

alsfjord_2013_2016 <- read_sas("inst/extdata/alsfjord_2013_2016.sas7bdat")
devtools::use_data(alsfjord_2013_2016, overwrite = TRUE)
