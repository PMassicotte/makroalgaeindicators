buildMM <- function(theta) {
  
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

objfun <- function(x, target) {
  
  mm <- buildMM(sqrt(x))
  
  return(sum((unlist(VarCorr(mm)) - target)^2))
}

modified_lmer <- function(df) {
  
  m1 <- lmer(
    log_cumcover_mod ~
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
  
  lmod <- lFormula(log_cumcover_mod ~
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
  
  # Remove resisuals estimate for now
  
  no_year <- length(unique(df$year))
  p <- read_params()[[1]]
  p$Estimate[p$CovParm == "year(vandomr*period)"] <- 
    p$Estimate[p$CovParm == "year(vandomr*period)"] * (1 - no_year / 6)
  
  p <- as.vector(p$Estimate)
  
  ## Change order to match the order provided in SAS by Jacob
  target <- as.vector(p)[c(3, 2, 1, 4)] 

  s0 <- target / sigma(m1)^2
  
  opt <- optim(fn = objfun, par = s0, target = target)
  
  mm_final <- buildMM(sqrt(opt$par))
  
  fixef(mm_final)
  
  # estimate is
  
  estimate <- fixef(mm_final)
  variance <- vcov(mm_final)@x
 
  getME(mm_final, "y")
  getME(mm_final, "X")
  # return(list(estimate_glmm = estimate_glmm, variance_glmm = variance_glmm))
  
  getME(mm_final, "beta")
  
  # as.matrix(getME(mm_final, "Lambdat")) * getME(mm_final,"sigma")
  
  # sigma <- getME(mm_final,"sigma")
  
  sigma <- sqrt(p[5])

  n <- getME(mm_final,"n")
  R <- diag(sigma^2, n, n)
  
  g_matrix <- as.matrix(getME(mm_final,"Lambda")) * as.matrix(getME(mm_final,"Lambdat")) * sigma^2
  z_matrix <- as.matrix(getME(mm_final,"Z"))
  
  V <- z_matrix %*% g_matrix %*% t(z_matrix) + R
  
  X <- as.vector(getME(mm_final,"X"))
  y_vector <- as.vector(getME(mm_final,"y"))
  
  Vinv <- solve(V)
  Beta <- solve(X %*% Vinv %*% (X)) %*% X %*% Vinv %*% y_vector
  
  V_Beta <- solve(X %*% Vinv %*% X)
  
}

