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
#' @param conf_lvl Percentil number
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
           n_iter = 10000,
           conf_lvl = 0.5) {
    
    boundaries <- c(-1e6, boundaries, 1e6)
    
    # *************************************************************************
    # Step 1: Process the "raw" data.
    # *************************************************************************
    df <- CumulativeCover_process_data(df)
    
    # *************************************************************************
    # Step 2: Do the GLMM on the processed data.
    # *************************************************************************
    
    glmm_res <- CumulativeCover_glmm(df)
    estimate_glmm <- glmm_res$estimate_glmm
    variance_glmm <- glmm_res$variance_glmm
    
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
    
    simulations <- data_frame(
      cumcover = cumcover,
      status = cut(cumcover, breaks = boundaries, labels = labels)
    )
    
    # *************************************************************************
    # Step 5: Proportion of each class (between 0 and 1).
    # *************************************************************************
    
    proportions <- data.frame(table(simulations$status)/nrow(simulations))
    names(proportions) <- c("class", "proportion")
    
    # *************************************************************************
    # Step 6: Calculate the requested percentil number
    # *************************************************************************
    
    myquantile <- quantile(simulations$cumcover, conf_lvl)
    
    # *************************************************************************
    # Step 7: Wrap-up results
    # *************************************************************************
    
    res <- list(proportions, myquantile)
    
    return(res)
    
  }


#' GLMM for the cumulative cover indicator
#'
#' @param df A data frame containing processed data ready for the GLMM.
#'
#' @return A list wiht both estimate and variance from the GLMM model.
#' @export
#'
#' @examples
CumulativeCover_glmm <- function(df) {
  
  estimate_glmm <- 4.9868829217 # use "fake" number until the glmm problem is solved
  variance_glmm <- 0.149426779 # use "fake" number until the glmm problem is solved
  
  return(list(estimate_glmm = estimate_glmm, variance_glmm = variance_glmm))
  
}

CumulativeCover_process_data <- function(df) {
  
  
  # How to break the depth into "classes"
  mybreaks <- c(0, seq(1, 21, by = 2), 100)
  
  res <- df %>%
    mutate(dato = as.Date(as.character(dato), format = "%Y%m%d")) %>% 
    group_by(
      vandomraade,
      ObservationsstedID,
      dato,
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
    
    mutate(prop_opportunist = opportunist / cumcov) %>% 
    mutate(arsin_prop_opportunist = asin(sqrt(prop_opportunist))) %>% 
    mutate(log_n_perenial = log(n_perenial + 1)) %>% 
    mutate(log_cumcov = log(cumcov + 1)) %>% 
    mutate(log_cumcov = ifelse(cumcov < (totcover_daekpct - 20), NA, log_cumcov)) %>% 
    
    mutate(depth_class = cut(dybde, mybreaks, right = FALSE, include.lowest = TRUE)) %>% 
    mutate(haard_ind1 = ifelse(hardbund_daekpct < 50, hardbund_daekpct, 50)) %>% 
    mutate(haard_ind2 = ifelse(hardbund_daekpct < 50, 0, hardbund_daekpct - 50)) %>% 
    mutate(month = lubridate::month(dato)) %>% 
    mutate(year = lubridate::year(dato)) %>% 
    
    filter(month %in% 5:9) %>% 
    filter(dybde < 17) %>% 
    filter(dybde > 3) # to change
  
  
  # ***************************************************************************
  # At this point we have the data correctly processed at the first level.
  # Now, lets calculate log_cumcover_mod.
  # ***************************************************************************
  
  params <- read_params()[[2]] %>% 
    select(Effect, month, Estimate)
  
  parm_depth <- params$Estimate[params$Effect == "depth"]
  parm_H1 <- params$Estimate[params$Effect == "haard_ind1"]
  parm_H2 <- params$Estimate[params$Effect == "haard_ind2"]
  
  res <- filter(params, Effect == "month") %>% 
    full_join(res, ., by = "month") %>% 
    mutate(log_cumcover_mod = log_cumcov - Estimate - parm_depth * dybde - haard_ind1 * parm_H1 - haard_ind2 * parm_H2) %>% 
    drop_na(log_cumcover_mod)
  
  return(res)
  
}