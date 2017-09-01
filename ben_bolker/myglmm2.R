m1 <- lmer(log_cumcover_mod ~ 
       (1 | kildestationsnavn) + 
       (1 | year) + 
       (1 | kildestationsnavn:year), 
       # (1 | proevetager), 
     data = res)

lmod <- lFormula(log_cumcover_mod ~ 
                   (1 | kildestationsnavn) + 
                   (1 | year) + 
                   (1 | kildestationsnavn:year), 
                 # (1 | proevetager), 
                 data = res)

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


objfun <- function(x, target = c(0.09902947, 0.01640309, 0.05848691)) {
  mm <- buildMM(sqrt(x))
  return(sum((unlist(VarCorr(
    mm
  )) - target) ^ 2))
}

target = c(0.09902947, 0.01640309, 0.05848691)

s0 <- target / sigma(m1) ^ 2
opt <- optim(fn = objfun, par = s0) 
mm_final <- buildMM(sqrt(opt$par))

summary(mm_final)
