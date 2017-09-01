
reprex::reprex({
  
  #' Dear Ben. This is a follow up regarding the discussion we had over 
  #' stackoverflow 
  #' (http://stackoverflow.com/questions/39718754/fixing-variance-values-in-lme4).
  #' If you remember, you helped us (my collegue @JacobCarstensen and I) to fit 
  #' a `lme()` model where we wanted to fix in advance the variance values. You
  #' proposed a solution that worked fine (see bellow). 
  
  library(lme4)
  set.seed(101)
  ss <- sleepstudy[sample(nrow(sleepstudy), size = round(0.9 * nrow(sleepstudy))), ] 
  m1 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), ss)
  
  summary(m1)
  
  fixef(m1)
  
  dd <- as.function(m1)
  
  ff <- dd(c(0, 0))
  
  opt <- list(par = c(0, 0), fval = ff, conv = 0)
  
  lmod <- lFormula(Reaction ~ Days + (1|Subject) + (0+Days|Subject), ss)
  
  m1X <- mkMerMod(environment(dd),
                  opt,
                  lmod$reTrms,
                  fr = lmod$fr,
                  mc = quote(hacked_lmer()))
  
  
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
  
  objfun <- function(x, target = c(700, 30)) {
    
    mm <- buildMM(sqrt(x))
    
    return(sum((unlist(VarCorr(mm)) - target)^2))
  }
  
  #' Fix variances to 700 and 30.
  s0 <- c(700, 30) / sigma(m1)^2
  
  opt <- optim(fn = objfun, par = s0)
  
  mm_final <- buildMM(sqrt(opt$par))
  
  #' This works fine!
  summary(mm_final)
  
  #' Now, we are wondering if it possible to force a lmer model with random 
  #' effect to be fitted on data with only one level? We want to do this to keep
  #' the same model structure in rare case where our data only contains 1 
  #' grouping level. In a previous step, we manually fix variance of the random 
  #' effects, hence we do not need to estimate it. To be clearer, the objective 
  #' is not to estimate the variance components of the mixed model, but to 
  #' estimate how pre-specified variances for a number of random factors affect 
  #' the standard error of a mean value. For example, if Y = μ + A + B + C + ε 
  #' and we know (from other analyses) `V[A]`, `V[B]`, `V[C]` and `V[ε]` then 
  #' what is the variance of our estimate of μ when estimated from a data set 
  #' that also contains information on the levels of the random factors A, B and
  #' C? Hopefully, there is an easy way with `lmer()` or at least getting the 
  #' design matrix for the random factors out so that the covariance structure 
  #' for all the random components can be calculated. For information, this can
  #' be done easily in SAS with the hold-option to the parms statement in PROC
  #' MIXED, but apparently this is not as easily done in lmer().
  
  ss2 <- subset(ss, Subject == 308)
  m2 <- lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), ss2)
  
})

