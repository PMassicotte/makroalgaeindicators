buildMM <- function(theta,m1,lmod) {
  
  dd <- as.function(m1)
  
  ff <- dd(theta)
  
  opt <- list(par = c(0, 0), fval = ff, conv = 0)
  
  mm <-
    mkMerMod(environment(dd),
             opt,
             lmod$reTrms,
             fr = lmod$fr,
             mc = quote(hacked_lmer()))
  
  return(mm)
}

objfun <- function(x, target,m1,lmod) {
  
  mm <- buildMM(sqrt(x),m1,lmod)
  
  return(sum((unlist(VarCorr(mm)) - target)^2))
}

modified_lmer <- function(indicator,df) {
  
  m1 <- 
    lmer(
    residual ~
      (1 | kildestationsnavn) +
      (1 | year) +
      (1 | kildestationsnavn:year) +
      (1 | proevetager),
    data = df,
    REML = TRUE,
    control = lmerControl(check.nlev.gtr.1 = "ignore"))
  
  
  dd <- as.function(m1)
  
  ff <- dd(c(0, 0, 0, 0))
  
  opt <- list(par = c(0, 0), fval = ff, conv = 0)
  
  lmod <- lFormula(residual ~
                     (1 | kildestationsnavn) +
                     (1 | year) +
                     (1 | kildestationsnavn:year) +
                     (1 | proevetager),
                   data = df, 
                   control = lmerControl(check.nlev.gtr.1 = "ignore"))
  
  m1X <- mkMerMod(environment(dd),
                  opt,
                  lmod$reTrms,
                  fr = lmod$fr,
                  mc = quote(hacked_lmer()))
  
  # Remove residuals estimate for now
  
  no_year <- length(unique(df$year))
  covparm <- switch(indicator,CumulativeCover = read_params()[[1]],
                              PropOpportunist = read_params()[[3]],
                              NPerennials     = read_params()[[5]])
  
  covparm$Estimate[covparm$CovParm == "year(vandomr*period)"] <- 
    covparm$Estimate[covparm$CovParm == "year(vandomr*period)"] * (1 - no_year / 6)
  
  covparm <- as.vector(covparm$Estimate)
  
  ## Change order to match the order provided in SAS by Jacob

  s0 <- as.vector(covparm)[c(3, 2, 1, 4)] / sigma(m1)^2
  
  opt <- optim(fn = objfun, par = s0, target = as.vector(covparm)[c(3, 2, 1, 4)],m1=m1,lmod=lmod)
  
  mm_final <- buildMM(sqrt(opt$par),m1,lmod)
  
  fixef(mm_final)
  
  # estimate is
  
  estimate <- fixef(mm_final)
  variance <- vcov(mm_final)@x
 
  sigma <- getME(mm_final,"sigma")
  
  n <- getME(mm_final,"n")
  R <- diag(covparm[5], n, n)
  
  g_matrix <- as.matrix(getME(mm_final,"Lambda")) * as.matrix(getME(mm_final,"Lambdat")) * sigma^2
  z_matrix <- as.matrix(getME(mm_final,"Z"))
  
  V <- z_matrix %*% g_matrix %*% t(z_matrix) + R
  
  X <- as.vector(getME(mm_final,"X"))
  y_vector <- as.vector(getME(mm_final,"y"))
  
  Vinv <- solve(V)
  Beta <- solve(X %*% Vinv %*% (X)) %*% X %*% Vinv %*% y_vector
  
  V_Beta <- solve(X %*% Vinv %*% X)
  return(list(estimate_glmm = Beta, variance_glmm = V_Beta))
  
}


