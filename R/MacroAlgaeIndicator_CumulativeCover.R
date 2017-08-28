#' Macroalgae indicators
#' 
#' @param indicator Name of the indicator to be calculated. Possible values are
#'   "CumulativeCover", "PropOpportunist", and "NPerennials"
#' 
#' @param df A dataframe with monitoring data for macroalgae according to the 
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
#'   \item{steneck}{The Steneck number of the species.} 
#'   \item{growth_strategy}{Either "P" for perennial, "O" for opportunist or "C"
#'   for crustforming algae.} }
#'   
#' @param boundaries A vector of length 4 giving the status class boundary values
#'   of the standardized indicator. The boundary values should be
#'   given in the order high-good, good-moderate, moderate-poor, poor-bad.
#' @param depth_cutoff The cut-off depth for physical exposure. Observations 
#'   whith depths shallower than this value are not used in the indicator
#'   calculation.
#' @param std_depth The depth for which  the indication is calculated. Default
#'   is 7 meters.
#' @param std_haardsub The suitable substrate percentage for which the indicator
#'   if calculated. Default it 0.5 (50%).
#' @param n_iter Maximum number of iteration for Montecarlo simulation.
#' @param conf_lvl Percentile number
#'   
#' @return
#' @export
#' 
#' @examples
MacroAlgaeIndicator <-
  function(indicator=c("CumulativeCover", "PropOpportunist" ,"NPerennials"),
           df,
           boundaries, 
           depth_cutoff,
           std_depth = 7,
           std_haardsub = 50,
           n_iter = 10000,
           Indicator_pred_pct = 0.5) {

# Check that there is only one indicator listed from the list    
    match.arg(indicator,c("CumulativeCover", "PropOpportunist" ,"NPerennials"),several.ok=FALSE)
    
# Check if indicator is in the list of accepted indicators    
    if (length(boundaries)!=4) {stop("Der skal angives 4 graensevaerdier i funktionskaldet!")}
   
# Add the lowest/highest value the indicator can take, corresponding to EQR=0. Reverse boundaries for those with decreasing numerical quality
    boundaries <- switch(indicator,
                         CumulativeCover = rev(c(1000,boundaries,0)),
                         PropOpportunist = c(0.0,boundaries,1.0),
                         NPerennials = rev(c(100,boundaries,0)))
    if(!(all(diff(boundaries) > 0)|all(diff(boundaries) < 0))) {stop("Graensevaerdierne er ikke angivet korrekt!")}
    
    # *************************************************************************
    # Step 1: Process the "raw" data.
    # *************************************************************************
    df <- Macroalgae_process_data(indicator,df,depth_cutoff)
    
    # Check if there are any observations remaining after preprocessing    
    if (length(df$residual)==0) {stop("Antallet af observationer i datasaettet er 0!")}
     
    # *************************************************************************
    # Step 2: Do the GLMM on the processed data.
    # *************************************************************************
    
    glmm_parmest <- modified_lmer(indicator,df)
 
    # *************************************************************************
    # Step 3: Read Jacob's data and replace values with those from the GLMMM
    # *************************************************************************
    
    ## Open Jacob's vector and replace the first element at position [1]
    b_vector <- switch(indicator,
                       CumulativeCover = as.vector(read_sas("data/parmest_cumcover_in.sas7bdat")$Estimate),
                       PropOpportunist = as.vector(read_sas("data/parmest_filaandel_in.sas7bdat")$Estimate),
                       NPerennials     = as.vector(read_sas("data/parmest_narter_perennial_in.sas7bdat")$Estimate))
    b_vector[1] <- glmm_parmest$estimate_glmm
    
    ## Open Jacob's matrix and replace 1 value at [1,1] with the estimate from the GLMM
    v_b_matrix <- switch(indicator,
                         CumulativeCover = as.matrix(read_sas("data/covb_cumcover_in.sas7bdat")),
                         PropOpportunist = as.matrix(read_sas("data/covb_filaandel_in.sas7bdat")),
                         NPerennials     = as.matrix(read_sas("data/covb_narter_perennial_in.sas7bdat")))
    v_b_matrix[1, 1] <- glmm_parmest$variance_glmm
    
    l_vector <- switch(indicator,
                       CumulativeCover = c(1, 0.2, 0.2, 0.2, 0.2, 0.2, std_depth, 50, 0),
                       PropOpportunist = c(1, 0.2, 0.2, 0.2, 0.2, 0.2, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 50, 0),
                       NPerennials     = c(1, 0.2, 0.2, 0.2, 0.2, 0.2, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 50, 0))
    estimate <- l_vector %*% b_vector
    
    variance <- l_vector %*% v_b_matrix %*% l_vector
    
    #different ranking of classes when going from low to high
    switch(indicator,
           CumulativeCover = labels <- c("bad", "poor", "moderate", "good", "high"),
           PropOpportunist = labels <- c("high", "good", "moderate", "poor", "bad"),
           NPerennials     = labels <- c("bad", "poor", "moderate", "good", "high"))
    
    # *************************************************************************
    # Step 4: Do the simulations.
    # *************************************************************************
    
    simulvector <- mat.or.vec(n_iter, 1)
    
    switch(indicator,
           CumulativeCover = for (i in 1:n_iter) {simulvector[i] <- exp(estimate + rnorm(1) * sqrt(variance)) - 1},
           PropOpportunist = for (i in 1:n_iter) {simulvector[i] <- sin(estimate + rnorm(1) * sqrt(variance))*sin(estimate + rnorm(1) * sqrt(variance))},
           NPerennials     = for (i in 1:n_iter) {simulvector[i] <- exp(estimate + rnorm(1) * sqrt(variance)) - 1})

    simulations <- data_frame(
      simulvector = simulvector,
      status = cut(simulvector, breaks = boundaries, labels = labels)
    )
    
    # *************************************************************************
    # Step 5: Proportion of each class (between 0 and 1).
    # *************************************************************************
    
    #Organise table so that it always starts with "High", "Good", etc.
    switch(indicator,
           CumulativeCover = proportions <- data.frame(rev(table(simulations$status)/nrow(simulations))),
           PropOpportunist = proportions <- data.frame(table(simulations$status)/nrow(simulations)),
           NPerennials     = proportions <- data.frame(rev(table(simulations$status)/nrow(simulations))))
    names(proportions) <- c("class", "proportion")
    
    # *************************************************************************
    # Step 6: Calculate the requested percentil number and standard error
    # *************************************************************************
    
    pred_indicator <- quantile(simulations$simulvector, Indicator_pred_pct)
    stderr_indicator <- sd(simulations$simulvector)
    
    # *************************************************************************
    # Step 7: Wrap-up results
    # *************************************************************************
    
    res <- list(proportions, pred_indicator, stderr_indicator)
    
    return(res)
    
  }

Macroalgae_process_data <- function(indicator,df,depth_cutoff) {
  
  
  # How to break the depth into "classes"
  mybreaks <- c(seq(1, 21, by = 2), 100)
  
  res <- df %>%
    mutate(dato = as.Date(as.character(dato), format = "%Y%m%d")) %>% 
    group_by(
      vandomraade,
      ObservationsstedID,
      dato,
      UndersoegelseTypeKode,
      kildestationsnavn,
      proevetager,
      proeveID,
      hardbund_daekpct, 
      totcover_daekpct,
      dybde
    ) %>%
    summarise(
      cumcov = sum(art_daekpct[Steneck < 7]),
      opportunist = sum(art_daekpct[Steneck <= 3]),
      n_perenial = sum(Steneck < 7 & art_daekpct >= 1 & Growth_strategy == "P")
    ) %>% 
    ungroup() %>% 
    mutate(hardbund_daekpct = ifelse(hardbund_daekpct < 0, NA, hardbund_daekpct)) %>% 
    mutate(opportunist = ifelse(cumcov > 0 & is.na(opportunist), 0, opportunist)) %>% 
    mutate(n_perenial = ifelse(is.na(cumcov) & is.na(n_perenial), 0, n_perenial)) %>% 
    
    mutate(prop_opportunist = ifelse(cumcov>0,opportunist / cumcov,NA)) %>% 
    mutate(arsin_prop_opportunist = asin(sqrt(prop_opportunist))) %>% 
    mutate(log_n_perenial = log(n_perenial + 1)) %>% 
    mutate(log_cumcov = log(cumcov + 1)) %>% 
    mutate(log_cumcov = ifelse(cumcov < (totcover_daekpct - 20), NA, log_cumcov)) %>% 
    
    mutate(depth_class = cut(dybde, mybreaks, right = FALSE, include.lowest = TRUE)) %>% 
    mutate(haard_ind1 = ifelse(hardbund_daekpct < 50, hardbund_daekpct, 50)) %>% 
    mutate(haard_ind2 = ifelse(hardbund_daekpct < 50, 0, hardbund_daekpct - 50)) %>% 
    mutate(month = lubridate::month(dato)) %>% 
    mutate(year = lubridate::year(dato)) %>% 
    
    filter(UndersoegelseTypeKode %in% c(1,5)) %>%  # use only transects with macroalgae data
    filter(month %in% 5:9) %>%   # use only months with regular monitoring data
    filter(dybde < 15) %>%       # include only depths less than 15 m
    filter(dybde > depth_cutoff) # include depths below the limit of physical exposure
  
  
  # ***************************************************************************
  # At this point we have the data correctly processed at the first level.
  # Now, lets calculate macroalgae observations adjusted for depth, hard substrate and month effects
  # ***************************************************************************
  
  params <- switch(indicator,CumulativeCover = read_params()[[2]] %>%  select(Effect, month, Estimate),
                   PropOpportunist = read_params()[[4]] %>% select(Effect, month, interval, Estimate),
                   NPerennials = read_params()[[6]] %>% select(Effect, month, interval, Estimate))
  
  parm_depth <- params$Estimate[params$Effect == "depth"]
  parm_H1 <- params$Estimate[params$Effect == "haard_ind1"]
  parm_H2 <- params$Estimate[params$Effect == "haard_ind2"]
  parm_month <- params$Estimate[params$Effect == "month"]
  parm_interval <- params$Estimate[params$Effect == "interval"]
  
  res <- switch(indicator,
           CumulativeCover  = mutate(res,residual = log_cumcov - parm_month[res$month-4] - parm_depth * dybde - haard_ind1 * parm_H1 - haard_ind2 * parm_H2),
           PropOpportunist  = mutate(res,residual = arsin_prop_opportunist - parm_month[res$month-4] - parm_interval[depth_class] - haard_ind1 * parm_H1 - haard_ind2 * parm_H2),
           NPerennials      = mutate(res,residual = log_n_perenial - parm_month[res$month-4] - parm_interval[depth_class] - haard_ind1 * parm_H1 - haard_ind2 * parm_H2))
 
  res <-drop_na(res,residual)  
 
  return(res)
  
}

