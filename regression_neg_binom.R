#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Negative binomial regression
# AUTHOR  : Jesper W. Schneider
# EMAIL   : jws@ps.au.dk
# UPDATED : 2019-04-25
#----------------------------------------------------------
# CONTRIBUTORS : Jens Peter Andersen <jpa@ps.au.dk>
#----------------------------------------------------------
# PURPOSE : Produce supplementary negative binomial regression
#----------------------------------------------------------

require(MASS)
source("dataload.R")

## negative binominal regression - model 1
nbmod1 <- glm.nb(tc ~ factor(case) + n_authors + factor(int_collab) + selfcit + mncs_journal, data = t_first)
nbs1 <- summary(nbmod1)
alpha <- 1/nbmod1$theta
est.nbmod1 <- cbind(Estimate = coef(nbmod1), confint(nbmod1))
nbr1 <- exp(est.nbmod1)

nb.rt1 <- tibble(Outcome = "CS",Model = "Sample 1",
              Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
              Estimate = nbs1$coefficients[,1],
              StdError = nbs1$coefficients[,2],
              ExpEstimate = nbr1[,1],
              ExpLCL = nbr1[,2],
              EXPUCL = nbr1[,3])

## negative binominal regression - model 2
nbmod2 <- glm.nb(tc ~ factor(case) + n_authors + factor(int_collab) + selfcit + mncs_journal, data = t_last)
nbs2 <- summary(nbmod2)
alpha <- 1/nbmod2$theta
modelfit(nbmod2)
est.nbmod2 <- cbind(Estimate = coef(nbmod2), confint(nbmod2))
nbr2 <- exp(est.nbmod2)

nb.rt2 <- tibble(Outcome = "CS",Model = "Sample 2",
                 Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
                 Estimate = nbs2$coefficients[,1],
                 StdError = nbs2$coefficients[,2],
                 ExpEstimate = nbr2[,1],
                 ExpLCL = nbr2[,2],
                 EXPUCL = nbr2[,3])

## negative binominal regression - model 3
nbmod3 <- glm.nb(tc ~ factor(case) + n_authors + factor(int_collab) + selfcit + mncs_journal, data = t_both)
nbs3 <- summary(nbmod3)
alpha <- 1/nbmod3$theta
modelfit(nbmod3)
est.nbmod3 <- cbind(Estimate = coef(nbmod3), confint(nbmod3))
nbr3 <- exp(est.nbmod3)

nb.rt3 <- tibble(Outcome = "CS",Model = "Sample 3",
                 Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
                 Estimate = nbs3$coefficients[,1],
                 StdError = nbs3$coefficients[,2],
                 ExpEstimate = nbr3[,1],
                 ExpLCL = nbr3[,2],
                 EXPUCL = nbr3[,3])

nb.rt.final <- rbind(nb.rt1,nb.rt2,nb.rt3)
save(nb.rt.final,file="regression_table_nb.Rdata")