#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Tweedie regressions
# AUTHOR  : Jesper W. Schneider
# EMAIL   : jws@ps.au.dk
# UPDATED : 2019-04-25
#----------------------------------------------------------
# CONTRIBUTORS : Jens Peter Andersen <jpa@ps.au.dk>
#----------------------------------------------------------
# PURPOSE : Standardize input variables	in line with Gelman (2009) and
#           run tweedie-model. Subsequently we will check predictors 
#           using logistic regression on the case variable
#----------------------------------------------------------

#----------------------------------------------------------
# LOAD PACKAGES 

require(statmod)
require(tweedie)
require(cplm)
require(MASS)
require(HDtweedie)
require(car)
require(arm)

source("dataload.R")

#----------------------------------------------------------
# Tweedie regression

# Sample 1, Female first author
# normal coefficients, as supplementary
twmod1 <- glm(ncs ~ case + n_authors + int_collab + selfcit + mncs_journal, data = t_first, family = tweedie(var.power = 1.65, link.power = power.link))
s1 <- summary(twmod1)
est1 <- cbind(Estimate = coef(twmod1), confint(twmod1))
irr1 <- exp(est1)

vif(twmod1)
sum(residuals(twmod1, type = "pearson")^2)

#regression table for supplementary materials
rt1 <- tibble(Outcome = "NCS",Model = "Sample 1",
             Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
             Estimate = s1$coefficients[,1],
             StdError = s1$coefficients[,2],
             ExpEstimate = irr1[,1],
             ExpLCL = irr1[,2],
             EXPUCL = irr1[,3])

# standardized coefficients
stand.twmod1 <- standardize(twmod1) 
s.s1 <- summary(stand.twmod1)
est.stand1 <- cbind(Estimate = coef(stand.twmod1), confint(stand.twmod1))
irr.stand1 <- exp(est.stand1)

vif(stand.twmod1)
sum(residuals(stand.twmod1, type = "pearson")^2)

#regression table for supplementary materials
rt.s1 <- tibble(Outcome = "NCS",Model = "Sample 1",
                Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
                Estimate = s.s1$coefficients[,1],
                StdError = s.s1$coefficients[,2],
                ExpEstimate = irr.stand1[,1],
                ExpLCL = irr.stand1[,2],
                EXPUCL = irr.stand1[,3])

rt1.c <- rt1 %>% inner_join(rt.s1,by=c("Outcome","Model","Predictor"))

# Sample 2, Female last author
# normal coefficients, as supplementary
twmod2 <- glm(ncs ~ case + n_authors + int_collab + selfcit + mncs_journal, data = t_last, family = tweedie(var.power = 1.72, link.power = power.link))
s2 <- summary(twmod2)
est2 <- cbind(Estimate = coef(twmod2), confint(twmod2))
irr2 <- exp(est2)

vif(twmod2)
sum(residuals(twmod2, type = "pearson")^2)

#regression table for supplementary materials
rt2 <- tibble(Outcome = "NCS",Model = "Sample 2",
              Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
              Estimate = s2$coefficients[,1],
              StdError = s2$coefficients[,2],
              ExpEstimate = irr2[,1],
              ExpLCL = irr2[,2],
              EXPUCL = irr2[,3])

# standardized coefficients
stand.twmod2 <- standardize(twmod2) 
s.s2 <- summary(stand.twmod2)
est.stand2 <- cbind(Estimate = coef(stand.twmod2), confint(stand.twmod2))
irr.stand2 <- exp(est.stand2)

vif(stand.twmod2)
sum(residuals(stand.twmod2, type = "pearson")^2)

#regression table for supplementary materials
rt.s2 <- tibble(Outcome = "NCS",Model = "Sample 2",
                Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
                Estimate = s.s2$coefficients[,1],
                StdError = s.s2$coefficients[,2],
                ExpEstimate = irr.stand2[,1],
                ExpLCL = irr.stand2[,2],
                EXPUCL = irr.stand2[,3])

rt2.c <- rt2 %>% inner_join(rt.s2,by=c("Outcome","Model","Predictor"))

# Sample 3, Female first & last author
# normal coefficients, as supplementary
twmod3 <- glm(ncs ~ case + n_authors + int_collab + selfcit + mncs_journal, data = t_both, family = tweedie(var.power = 1.6, link.power = power.link))
s3 <- summary(twmod3)
est3 <- cbind(Estimate = coef(twmod3), confint(twmod3))
irr3 <- exp(est3)

vif(twmod3)
sqrt(vif(twmod3))
sum(residuals(twmod3, type = "pearson")^2)

#regression table for supplementary materials
rt3 <- tibble(Outcome = "NCS",Model = "Sample 3",
              Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
              Estimate = s3$coefficients[,1],
              StdError = s3$coefficients[,2],
              ExpEstimate = irr3[,1],
              ExpLCL = irr3[,2],
              EXPUCL = irr3[,3])

# standardized coefficients
stand.twmod3 <- standardize(twmod3) 
s.s3 <- summary(stand.twmod3)
est.stand3 <- cbind(Estimate = coef(stand.twmod3), confint(stand.twmod3))
irr.stand3 <- exp(est.stand3)

vif(stand.twmod3)
sum(residuals(stand.twmod3, type = "pearson")^2)


#regression table for supplementary materials
rt.s3 <- tibble(Outcome = "NCS",Model = "Sample 3",
                Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_journal"),
                Estimate = s.s3$coefficients[,1],
                StdError = s.s3$coefficients[,2],
                ExpEstimate = irr.stand3[,1],
                ExpLCL = irr.stand3[,2],
                EXPUCL = irr.stand3[,3])

rt3.c <- rt3 %>% inner_join(rt.s3,by=c("Outcome","Model","Predictor"))

save(est1,irr1,est.stand1,irr.stand1,
     est2,irr2,est.stand2,irr.stand2,
     est3,irr3,est.stand3,irr.stand3,
     file="regression_output.Rdata")

rt.final <- rbind(rt1.c,rt2.c,rt3.c)
save(rt.final,file="regression_table_main.Rdata")

#----------------------------------------------------------
# logistic regression

# Sample 1, Female first author
# normal coefficients, as supplementary
logitmod1 <- glm(case ~ n_authors + int_collab + selfcit + mncs_journal, data = t_first, family = binomial)
ls1 <- summary(logitmod1)
estlog1 <- cbind(Estimate = coef(logitmod1), confint(logitmod1))
slr1 <- exp(estlog1)

# standardized coefficients
stand.logitmod1 <- standardize(logitmod1) 
ls.s1 <- summary(stand.logitmod1)
est.standlogit1 <- cbind(Estimate = coef(stand.logitmod1), confint(stand.logitmod1))
slr.stand1 <- exp(est.standlogit1)

lrt1 <- tibble(Outcome = "case",Model = "Sample 1",
                Predictor = c("(Intercept)","n_authors","int_collab","selfcit","mncs_journal"),
                Estimate = ls1$coefficients[,1],
                StdError = ls1$coefficients[,2],
                ExpEstimate = slr1[,1],
                ExpLCL = slr1[,2],
                EXPUCL = slr1[,3])

lrt.s1 <- tibble(Outcome = "case",Model = "Sample 1",
                 Predictor = c("(Intercept)","n_authors","int_collab","selfcit","mncs_journal"),
                Estimate = ls.s1$coefficients[,1],
                StdError = ls.s1$coefficients[,2],
                ExpEstimate = slr.stand1[,1],
                ExpLCL = slr.stand1[,2],
                EXPUCL = slr.stand1[,3])

# Sample 2, Female last author
# normal coefficients, as supplementary
logitmod2 <- glm(case ~ n_authors + int_collab + selfcit + mncs_journal, data = t_last, family = binomial)
ls2 <- summary(logitmod2)
estlog2 <- cbind(Estimate = coef(logitmod2), confint(logitmod2))
slr2 <- exp(estlog2)

# standardized coefficients
stand.logitmod2 <- standardize(logitmod2) 
ls.s2 <- summary(stand.logitmod2)
est.standlogit2 <- cbind(Estimate = coef(stand.logitmod2), confint(stand.logitmod2))
slr.stand2 <- exp(est.standlogit2)


lrt2 <- tibble(Outcome = "case",Model = "Sample 2",
               Predictor = c("(Intercept)","n_authors","int_collab","selfcit","mncs_journal"),
               Estimate = ls2$coefficients[,1],
               StdError = ls2$coefficients[,2],
               ExpEstimate = slr2[,1],
               ExpLCL = slr2[,2],
               EXPUCL = slr2[,3])

lrt.s2 <- tibble(Outcome = "case",Model = "Sample 2",
                 Predictor = c("(Intercept)","n_authors","int_collab","selfcit","mncs_journal"),
                 Estimate = ls.s2$coefficients[,1],
                 StdError = ls.s2$coefficients[,2],
                 ExpEstimate = slr.stand2[,1],
                 ExpLCL = slr.stand2[,2],
                 EXPUCL = slr.stand2[,3])

# Sample 3, Female first & last author
# normal coefficients, as supplementary
logitmod3 <- glm(case ~ n_authors + int_collab + selfcit + mncs_journal, data = t_both, family = binomial)
ls3 <- summary(logitmod3)
estlog3 <- cbind(Estimate = coef(logitmod3), confint(logitmod3))
slr3 <- exp(estlog3)

# standardized coefficients
stand.logitmod3 <- standardize(logitmod3) 
ls.s3 <- summary(stand.logitmod3)
est.standlogit3 <- cbind(Estimate = coef(stand.logitmod3), confint(stand.logitmod3))
slr.stand3 <- exp(est.standlogit3)

lrt3 <- tibble(Outcome = "case",Model = "Sample 3",
               Predictor = c("(Intercept)","n_authors","int_collab","selfcit","mncs_journal"),
               Estimate = ls3$coefficients[,1],
               StdError = ls3$coefficients[,2],
               ExpEstimate = slr3[,1],
               ExpLCL = slr3[,2],
               EXPUCL = slr3[,3])

lrt.s3 <- tibble(Outcome = "case",Model = "Sample 3",
                 Predictor = c("(Intercept)","n_authors","int_collab","selfcit","mncs_journal"),
                 Estimate = ls.s3$coefficients[,1],
                 StdError = ls.s3$coefficients[,2],
                 ExpEstimate = slr.stand3[,1],
                 ExpLCL = slr.stand3[,2],
                 EXPUCL = slr.stand3[,3])

save(slr1,slr.stand1,
     slr2,slr.stand2,
     slr3,slr.stand3,
     file="ols_regression_output.Rdata")

lrt1.c <- lrt1 %>% inner_join(lrt.s1,by=c("Outcome","Model","Predictor"))
lrt2.c <- lrt2 %>% inner_join(lrt.s2,by=c("Outcome","Model","Predictor"))
lrt3.c <- lrt3 %>% inner_join(lrt.s3,by=c("Outcome","Model","Predictor"))

lrt.final <- rbind(lrt1.c,lrt2.c,lrt3.c)
save(lrt.final,file="regression_table_logit.Rdata")
