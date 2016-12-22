

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
           boundaries = c(13.4, 20.4, 40.8, 74.2), # Remove default
           n_iter = 10000) {
    
    boundaries <- c(-1e6, boundaries, 1e6)
    
    # *************************************************************************
    # Step 1: Process the "raw" data.
    # *************************************************************************
    res <- process_data(df)
    
    # *************************************************************************
    # Step 2: Do the GLMM on the processed data.
    # *************************************************************************
    
    estimate_glmm <- 4.9868829217 # use "fake" number until the glmm problem is solved
    variance_glmm <- 0.149426779 # use "fake" number until the glmm problem is solved
    
    # *************************************************************************
    # Step 3: Read Jacob's data and replace values with those from the GLMMM
    # *************************************************************************
    
    ## Open Jacob's vector and replace the first element at position [1]
    b_vector <- as.vector(read_sas("data/parmest_cumcover_in.sas7bdat")$Estimate)
    b_vector[1] <- estimate_glmm
    
    ## Open Jacob's matrix and replace 1 value at [1,1] with the estimate from the GLMM
    v_b_matrix <- as.matrix(read_sas("data/covb_cumcover_in.sas7bdat"))
    v_b_matrix[1, 1] <- variance_glmm
    
    l_vector <- c(1, 0.2, 0.2, 0.2, 0.2, 0.2, std_depth, 50, 0)
    
    estimate <- l_vector %*% b_vector
    
    variance <- l_vector %*% v_b_matrix %*% l_vector
    
    labels <- c("bad", "poor", "moderate", "good", "high")
    
    # *************************************************************************
    # Step 4: Do the simulations.
    # *************************************************************************
    
    cumcover <- mat.or.vec(n_iter, 1)
    
    for (i in 1:n_iter) {
      cumcover[i] <- exp(estimate + rnorm(1) * sqrt(variance)) + 1
    }
    
    res <- data_frame(
      cumcover = cumcover,
      status = cut(cumcover, breaks = boundaries, labels = labels)
    )
    
    return(res)
    
  }