#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Tweedie full sample
# AUTHOR  : Jesper W. Schneider
# EMAIL   : jws@ps.au.dk
# UPDATED : 2019-04-25
#----------------------------------------------------------
# CONTRIBUTORS : Jens Peter Andersen <jpa@ps.au.dk>
#----------------------------------------------------------
# PURPOSE : Produce supplementary Tweedie regression of the
#           full, unmatched sample.
#----------------------------------------------------------

load("full_data_prepped.Rdata")
require(statmod)
require(tweedie)
require(cplm)
require(MASS)
require(HDtweedie)
require(car)
require(arm)

twmod1 <- glm(ncs ~ factor(f_first) + n_authors + factor(int_collab) + selfcit + mncs_journal, data = fd, 
              family = tweedie(var.power = 1.65, link.power = 0))
s1 <- summary(twmod1)

# standradized coefficients
stand.twmod1 <- standardize(twmod1) 
s.s1 <- summary(stand.twmod1)
est.stand1 <- cbind(Estimate = coef(stand.twmod1), confint(stand.twmod1))
sr.s1 <- exp(est.stand1)

rt1 <- tibble(Outcome = "NCS",Model = "F_First",
              Predictor = c("(Intercept)","f_first","n_authors","int_collab","selfcit","mncs_journal"),
              Estimate = s.s1$coefficients[,1],
              StdError = s.s1$coefficients[,2],
              ExpEstimate = sr.s1[,1],
              ExpLCL = sr.s1[,2],
              EXPUCL = sr.s1[,3])

#####################################
twmod2 <- glm(ncs ~ factor(f_last) + n_authors + factor(int_collab) + selfcit + mncs_journal, data = fd, 
              family = tweedie(var.power = 1.65, link.power = 0))
s2 <- summary(twmod2)

# standradized coefficients
stand.twmod2 <- standardize(twmod2) 
s.s2 <- summary(stand.twmod2)
est.stand2 <- cbind(Estimate = coef(stand.twmod2), confint(stand.twmod2))
sr.s2 <- exp(est.stand2)

rt2 <- tibble(Outcome = "NCS",Model = "F_Last",
              Predictor = c("(Intercept)","f_last","n_authors","int_collab","selfcit","mncs_journal"),
              Estimate = s.s2$coefficients[,1],
              StdError = s.s2$coefficients[,2],
              ExpEstimate = sr.s2[,1],
              ExpLCL = sr.s2[,2],
              EXPUCL = sr.s2[,3])

####################################
twmod3 <- glm(ncs ~ factor(f_both) + n_authors + factor(int_collab) + selfcit + mncs_journal, data = fd, 
              family = tweedie(var.power = 1.65, link.power = 0))
s3 <- summary(twmod3)

# standradized coefficients
stand.twmod3 <- standardize(twmod3) 
s.s3 <- summary(stand.twmod3)
est.stand3 <- cbind(Estimate = coef(stand.twmod3), confint(stand.twmod3))
sr.s3 <- exp(est.stand3)

rt3 <- tibble(Outcome = "NCS",Model = "F_Both",
              Predictor = c("(Intercept)","f_both","n_authors","int_collab","selfcit","mncs_journal"),
              Estimate = s.s3$coefficients[,1],
              StdError = s.s3$coefficients[,2],
              ExpEstimate = sr.s3[,1],
              ExpLCL = sr.s3[,2],
              EXPUCL = sr.s3[,3])

ft.rt.final <- rbind(rt1,rt2,rt3)
save(ft.rt.final,file="regression_table_tw_full.Rdata")
