

#' Cumulative cover indicator
#' 
#' @param df A datafram with monitoring data for macroalgae according to the 
#'   Danish National Marine Monitoring Program. The dataframe should contain the
#'   following variables:
#'   
#'   \describe{ 
#'   \item{kildestationnavn}{An identifier for the monitoring
#'   transect name.} 
#'   \item{dato}{Date of the sampling.} 
#'   \item{dybde}{Depth of
#'   the observation in meter.} 
#'   \item{hardbund_daekpct}{Percentage cover of
#'   suitable substrate for macroalgae.} 
#'   \item{totcover_daekpct}{Total cover of
#'   macroalgae relative to the suitable substrate.} 
#'   \item{proevetager}{Identifier of the person carrying out the monitoring
#'   transect.} 
#'   \item{lat_art}{Specie names of the macroalgae.} 
#'   \item{steneck}{The Steneck number of the species.} 
#'   \item{growth_strategy}{Either "P" for perennial, "O" for opportunist or "C"
#'   for crustforming algae.} }
#'   
#' @param depth_cutoff The cut-off depth for physical exposure. Observations 
#'   whith depths shallower than this value are not used in the indicator
#'   calculation.
#' @param std_depth The depth for which  the indication is calculated. Default
#'   is 7 meters.
#' @param std_haardsub The suitable substrate percentage for which the indicator
#'   if calculated. Default it 0.5 (50%).
#' @param boundaries A vector of lenght 4 giving the boundary values of the
#'   standardized indicator for cumulative cover. The boundary values should be
#'   given in the order high-good, good-moderate, moderate-poor, poor-bad.
#' @param n_iter Maximum number of iteration for Montecarlo simulation.
#'   
#' @return
#' @export
#' 
#' @examples
MacroAlgaeIndicator_CumulativeCover <-
  function(df,
           depth_cutoff,
           std_depth = 7,
           std_haardsub = 50,
           boundaries,
           n_iter = 10000) {
    
    # ***************************************************************************
    # glmm models
    #
    # Find which package to use
    # 1. Gaussian distribution
    # 2. Allow fixed parameter value
    # ***************************************************************************
    
    # Look at glmer from lme4
    # https://mixedpsychophysics.wordpress.com/r-code-a-test/fitting-a-glmm-with-lme4-basic-syntax/
    
   
    res <- process_data(df)
    
    return(res)
    
  }