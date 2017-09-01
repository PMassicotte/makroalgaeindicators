library(lme4)

rm(list = ls())

set.seed(101)
ss <- sleepstudy[sample(nrow(sleepstudy),size=round(0.9*nrow(sleepstudy))),]

m1 <- lmer(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), ss)

summary(m1)

dd <- as.function(m1)

ff <- dd(c(sqrt(703.38),sqrt(35.34)))
environment(dd)$pp$beta(1)  ## new parameters

# These are the Std. Error
environment(dd)$pp$beta0

## New model

opt <- list(par = (c(0, 0)),
            fval = ff,
            conv = 0)

lmod <- lFormula(Reaction~Days+(1|Subject)+(0+Days|Subject),ss)

m1X <- mkMerMod(environment(dd),
                opt,
                lmod$reTrms,
                fr = lmod$fr,
                mc = quote(hacked_lmer()))


m1
m1X

summary(m1)
summary(m1X)
