Hi Ben.

Following your answer on
[Stackoverflow](http://stackoverflow.com/questions/39718754/fixing-variance-values-in-lme4),
I tried to fix parameter as you described.

    library(lme4)

    ## Loading required package: Matrix

    rm(list = ls())

    set.seed(101)
    ss <- sleepstudy[sample(nrow(sleepstudy),size=round(0.9*nrow(sleepstudy))),]

    m1 <- lmer(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), ss)

    summary(m1)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Reaction ~ Days + (1 | Subject) + (0 + Days | Subject)
    ##    Data: ss
    ## 
    ## REML criterion at convergence: 1580.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8827 -0.4678  0.0175  0.4966  4.9724 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  Subject   (Intercept) 703.38   26.521  
    ##  Subject.1 Days         35.34    5.945  
    ##  Residual              687.02   26.211  
    ## Number of obs: 162, groups:  Subject, 18
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  251.552      7.324   34.35
    ## Days          10.379      1.573    6.60
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Days -0.199

This model gives me the following estimates on fixed effects:

    coef(summary(m1))

    ##              Estimate Std. Error   t value
    ## (Intercept) 251.55172   7.324088 34.345808
    ## Days         10.37874   1.572872  6.598593

To test your answer, I tried to fix the estimated variances of `m1`.
Doing this, I was expecting to find exactly the same estimates on the
fixed effects.

    dd <- as.function(m1)

    # These values are those originally estimated
    ff <- dd(c(sqrt(703.38),sqrt(35.34)))

Now, let's compare original estimates with the new ones.

    coef(summary(m1))

    ##              Estimate Std. Error   t value
    ## (Intercept) 251.55172   7.324088 34.345808
    ## Days         10.37874   1.572872  6.598593

    environment(dd)$pp$beta(1)  ## new parameters

    ## [1] 251.37021  10.37071

As we can see, these are slightly different. What I am missing?

Thank you for your help, Philippe

    opt <- list(par = c(0, 0),
                fval = ff,
                conv = 0)
                lmod <- lFormula(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), ss)
                m1X <- mkMerMod(environment(dd),
                opt,
                lmod$reTrms,
                fr = lmod$fr,
                mc = quote(hacked_lmer()))

                buildMM <- function(theta) {
                  dd <- as.function(m1)
                  ff <- dd(theta)
                  opt <- list(par = c(0, 0),
                  fval = ff,
                  conv = 0)
                  mm <- mkMerMod(environment(dd),
                  opt,
                  lmod$reTrms,
                  fr = lmod$fr,
                  mc = quote(hacked_lmer()))
                  return(mm)
                }

    objfun <- function(x,target=c(700,30)) {
       mm <- buildMM(sqrt(x))
       return(sum((unlist(VarCorr(mm))-target)^2))
    }
    s0 <- c(700,30)/sigma(m1)^2
    opt <- optim(fn=objfun,par=s0)

    ## Warning in sqrt(x): NaNs produced

    mm_final <- buildMM(sqrt(opt$par))
    summary(mm_final)

    ## Linear mixed model fit by REML ['lmerMod']
    ## 
    ## REML criterion at convergence: 1581
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8453 -0.4798  0.0206  0.4994  4.9268 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  Subject   (Intercept) 700      26.458  
    ##  Subject.1 Days         30       5.477  
    ##  Residual              700      26.458  
    ## Number of obs: 162, groups:  Subject, 18
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  251.580      7.330   34.32
    ## Days          10.378      1.479    7.02
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Days -0.215
