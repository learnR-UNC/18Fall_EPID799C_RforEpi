##############################################################################
# Purpose:
# Author: R Epi Team
# Input:
# Output:
# Remarks:
###############################################################################

# TO DO
# can make this a lot cleaner by having a function that saves model object and does est and 95% CI for me

lazyglm <- function(...){
  glmobj <- glm(...)
  if(glmobj$family$link == "identity"){
    est <- coef(glmobj)
    confint <- confint.default(glmobj) # assuming asymptotic normality for ease of convergence
  } else {
    est <- exp(coef(glmobj))
    confint <- exp(confint.default(glmobj)) # assuming asymptotic normality for ease of convergence
  }

  sum <- round(cbind(est, confint),2)

  ret <- list(glmobj=glmobj, est=est, confint=confint, sum=sum)
  return(ret)
}

########################################
#########      Imports      ############
########################################
library(tidyverse)
library(lmtest)
########################################
######    Acknowledgements       #######
########################################
# https://sper.org/wp-content/uploads/sites/2/2015/10/AMW-2014_Fox_Simulation-Studies.pdf
# https://github.com/avonholle

########################################
#####        Confounding           #####
########################################
# Criterion that C is associated with E and D
# E and D are associated independent of C
# DAG is classic confounder

n <- 1000 # sample size

df <- data.frame(dz=rep(NA, n),
                 exp=rep(NA,n),
                 c=rbinom(n,1,0.5)

)

# set baselines
p1 <- 0.5 # p(D = 1 | E = 1) but this is crude so this may be a fake prevalence of disease given the exposure
p0 <- 0.1 # p(D = 1 | E = 0)
e <- pc <- 0.3 # this is the "error" introduced by confounding or the increased association given differning levels of the confounder
pe <- 0.5

df$exp[df$c == 1] <- rbinom(n=sum(df$c == 1), 1, min(pe+pc, 0.8))
df$exp[df$c == 0] <- rbinom(n=sum(df$c == 0), 1, pe)


df$dz[df$exp == 1 & df$c == 1] <- rbinom(sum(df$exp == 1 & df$c == 1),1, min(p1 + e, 0.99))
df$dz[df$exp == 0 & df$c == 1] <- rbinom(sum(df$exp == 0 & df$c == 1),1, min(p0 + e, 0.99))
df$dz[df$exp == 1 & df$c == 0] <- rbinom(sum(df$exp == 1 & df$c == 0),1,p1)
df$dz[df$exp == 0 & df$c == 0] <- rbinom(sum(df$exp == 0 & df$c == 0),1,p0)
crude <- prop.table(xtabs(~df$exp + df$dz), margin=1); crude
crude[2,2] - crude[1,2]

adj1 <- xtabs(~df$exp + df$dz + df$c)[,,1]
adj2 <- xtabs(~df$exp + df$dz + df$c)[,,2]
prop.table(adj1, margin = 1)[2,2] - prop.table(adj1, margin = 1)[1,2]
prop.table(adj2, margin = 1)[2,2] - prop.table(adj2, margin = 1)[1,2]


### RISK DIFFERENCE
mod0 <- glm(dz ~ exp, data=df, family = binomial(link = "identity"))
broom::tidy(mod0) # crude
mod1 <- glm(dz ~ exp + c, data=df, family = binomial(link = "identity"))
broom::tidy(mod1) # adj






########################################
#####              EMM             #####
########################################
# Rules of EMM --> absence of EMM on one scale means it must be present on the other scale
#           Note, can have EMM on multiple or all scales (i.e. RR & RD but not OR...)

set.seed(44)
n <- 10000 # sample size
df <- data.frame(dz=rep(NA, n),
                 exp=rep(NA, n),
                 m=rbinom(n = n, size = 1, 0.5)
)


p1 <- 0.5 # p(D = 1 | E = 1)
p0 <- 0.1 # p(D = 1 | E = 0)
pe <- 0.5 # exposure prev
e <- 0.1 # some error introduced by confounding -- hacky
emm <- 0.1 # prob that emm is assoc w/ outcome

df$exp <- rbinom(n = n, size = 1, pe)
df$dz[df$exp == 1 & df$m == 1] <- rbinom(sum(df$exp == 1 & df$m == 1), 1, emm+p1) # hacky but introduce some error
df$dz[df$exp == 0 & df$m == 1] <- rbinom(sum(df$exp == 0 & df$m == 1),1, emm+p0)
df$dz[df$exp == 1 & df$m == 0] <- rbinom(sum(df$exp == 1 & df$m == 0),1,p1)
df$dz[df$exp == 0 & df$m == 0] <- rbinom(sum(df$exp == 0 & df$m == 0),1,p0)

# crude table
xtabs(~df$exp + df$dz)
crude <- prop.table(xtabs(~df$exp + df$dz), margin = 1)
crude[2,2] - crude[1,2]
crude[2,2]/crude[1,2]
# stratified table
xtabs(~df$exp + df$dz + df$m)
strata <- prop.table(xtabs(~df$exp + df$dz + df$m), margin = 1) # store for strata estimates
# EMM on the Risk Difference Scale
strata[2,2,1] - strata[1,2,1]
strata[2,2,2] - strata[1,2,2]

# EMM on the Risk Ratio Scale
strata[2,2,1] / strata[1,2,1]
strata[2,2,2] / strata[1,2,2]



# Risk difference scale
mod1 <- glm(dz ~ exp, data=df, family = binomial(link = "identity"))
summary(mod1)
round(cbind(coef(mod1), confint.default(mod1)),2)

mod2 <- glm(dz ~ exp + m, data=df, family = binomial(link = "identity"))
summary(mod2) # just add in extra risk factor, EMM without allowing it to departure from homogeneity
lmtest::lrtest(mod2, mod1)
round(cbind(coef(mod2), confint.default(mod2)),2)

mod3 <- glm(dz ~ exp + m + exp*m, data=df, family = binomial(link = "identity"))
summary(mod3) # account for EMM and look for departure from homogeneity with RD model
round(cbind(coef(mod3), confint.default(mod3)),2)
lmtest::lrtest(mod3, mod2)


# risk ratio scale
mod4 <- glm(dz ~ exp, data=df, family = binomial(link = "log"))
summary(mod4)
round(cbind(exp(coef(mod4)), exp(confint.default(mod4))),2)

mod5 <- glm(dz ~ exp + m, data=df, family = binomial(link = "log"))
summary(mod5) # just add in extra risk factor, EMM without allowing it to departure from homogeneity
round(cbind(exp(coef(mod5)), exp(confint.default(mod5))),2)
lmtest::lrtest(mod5,mod4)

mod6 <- glm(dz ~ exp + m + exp*m, data=df, family = binomial(link = "log"))
summary(mod6) # account for EMM and look for departure from homogeneity with RD model
round(cbind(exp(coef(mod6)), exp(exp(confint.default(mod6)))),2)
lmtest::lrtest(mod6, mod5)

rr1 <- contrast::contrast(mod6,
                          a=list(exp = 1, m=1),
                          b=list(exp = 0, m=1))
rr0 <- contrast::contrast(mod6,
                          a=list(exp = 1, m=0),
                          b=list(exp = 0, m=0))



num <- exp(rr1$Contrast)*(1/sum(df$m==1)) + exp(rr0$Contrast)*(1/sum(df$m==0))
denom <- (1/sum(df$m==1)) + (1/sum(df$m==0))
num/denom

inverseprobweight <- as.data.frame(t(prop.table(table(df$m))), stringsAsFactors = F)
inverseprobweight <- inverseprobweight %>%
  dplyr::mutate(m=as.numeric(Var2), inverseprobweight=1/Freq) %>%
  dplyr::select(m,inverseprobweight)

dfw <- left_join(df, inverseprobweight, by=c("m"))

mod7 <- glm(dz ~ exp + m + m*exp, data=dfw, family = quasipoisson(link = "log"),
            weights = inverseprobweight)
summary(mod7)
round(cbind(exp(coef(mod7)), exp(exp(confint(mod7)))),2)
rr1 <- contrast::contrast(mod7,
                          a=list(exp = 1, m=1),
                          b=list(exp = 0, m=1))
rr0 <- contrast::contrast(mod7,
                          a=list(exp = 1, m=0),
                          b=list(exp = 0, m=0))








# odds ratio scale
mod7 <- glm(dz ~ exp + m, data=df, family = binomial(link = "logit"))
summary(mod7) # just add in extra risk factor, EMM without allowing it to departure from homogeneity
mod8 <- glm(dz ~ exp + m + exp*m, data=df, family = binomial(link = "logit"))
summary(mod8) # account for EMM and look for departure from homogeneity with RD model
lmtest::lrtest(mod8, mod7)
