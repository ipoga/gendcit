#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Tweedie with MNCS Journal quantiles
# AUTHOR  : Jesper W. Schneider
# EMAIL   : jws@ps.au.dk
# UPDATED : 2019-04-25
#----------------------------------------------------------
# CONTRIBUTORS : Jens Peter Andersen <jpa@ps.au.dk>
#----------------------------------------------------------
# PURPOSE : Produce supplementary Tweedie regression using
#           quantiles of MNCS Journal
#----------------------------------------------------------

require(statmod)
require(tweedie)
require(cplm)
require(MASS)
require(HDtweedie)
require(car)
require(arm)
source("dataload.R")

### Tweedie model 1

### Create three new binary variables
# high.jif <- >= 95 percentile
# medium.jif <- >= 50 percentile
# low.jif <- < 50 percentile

t_first <- t_first %>%
  mutate(pct = ntile(mncs_journal,100))
t_first$high.jif <- 0
t_first$medium.jif <- 0
t_first$low.jif <- 0
t_first$high.jif[t_first$pct >= 95] <- 1
t_first$medium.jif[t_first$pct >= 50 & t_first$pct < 95] <- 1
t_first$low.jif[t_first$pct < 50] <- 1

# tweedie model 1 - link.power = 0
twmod1 <- glm(ncs ~ factor(case) + n_authors + factor(int_collab) + selfcit + factor(high.jif) + factor(medium.jif), data = t_first, 
              family = tweedie(var.power = 1.65, link.power = 0)) # low.jif is the reference category
s1 <- summary(twmod1)

# standardized coefficients
stand.twmod1 <- standardize(twmod1) 
s.s1 <- summary(stand.twmod1)
est.stand1 <- cbind(Estimate = coef(stand.twmod1), confint(stand.twmod1))
sr.s1 <- exp(est.stand1)

rt1 <- tibble(Outcome = "NCS",Model = "Sample 1",
              Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_j_high","mncs_j_med"),
              Estimate = s.s1$coefficients[,1],
              StdError = s.s1$coefficients[,2],
              ExpEstimate = sr.s1[,1],
              ExpLCL = sr.s1[,2],
              EXPUCL = sr.s1[,3])

# marginal means
#twmod1.emm <- emmeans(twmod1, "case")
#e1 <- summary(twmod1.emm)
#exp(e1)


### Tweedie model 2

### Create three new binary variables
# high.jif <- >= 95 percentile
# medium.jif <- >= 50 percentile
# low.jif <- < 50 percentile

t_last <- t_last %>%
  mutate(pct = ntile(mncs_journal,100))
t_last$high.jif <- 0
t_last$medium.jif <- 0
t_last$low.jif <- 0
t_last$high.jif[t_last$pct >= 95] <- 1
t_last$medium.jif[t_last$pct >= 50 & t_last$pct < 95] <- 1
t_last$low.jif[t_last$pct < 50] <- 1

# tweedie model 2
twmod2 <- glm(ncs ~ factor(case) + n_authors + factor(int_collab) + selfcit + factor(high.jif) + factor(medium.jif), data = t_last, 
              family = tweedie(var.power = 1.72, link.power = 0))  # low.jif is the reference category
s2 <- summary(twmod2)

# standradized coefficients
stand.twmod2 <- standardize(twmod2) 
s.s2 <- summary(stand.twmod2)
est.stand2 <- cbind(Estimate = coef(stand.twmod2), confint(stand.twmod2))
sr.s2 <- exp(est.stand2)

rt2 <- tibble(Outcome = "NCS",Model = "Sample 2",
              Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_j_high","mncs_j_med"),
              Estimate = s.s2$coefficients[,1],
              StdError = s.s2$coefficients[,2],
              ExpEstimate = sr.s2[,1],
              ExpLCL = sr.s2[,2],
              EXPUCL = sr.s2[,3])

### Tweedie model 3

### Create three new binary variables
# high.jif <- >= 95 percentile
# medium.jif <- >= 50 percentile
# low.jif <- < 50 percentile

t_both <- t_both %>%
  mutate(pct = ntile(mncs_journal,100))
t_both$high.jif <- 0
t_both$medium.jif <- 0
t_both$low.jif <- 0
t_both$high.jif[t_both$pct >= 95] <- 1
t_both$medium.jif[t_both$pct >= 50 & t_both$pct < 95] <- 1
t_both$low.jif[t_both$pct < 50] <- 1

# tweedie model 3
twmod3 <- glm(ncs ~ factor(case) + n_authors + factor(int_collab) + selfcit + factor(high.jif) + factor(medium.jif), data = t_both, 
              family = tweedie(var.power = 1.6, link.power = 0)) # low.jif is the reference category
s3 <- summary(twmod3)

# standradized coefficients
stand.twmod3 <- standardize(twmod3) 
s.s3 <- summary(stand.twmod3)
est.stand3 <- cbind(Estimate = coef(stand.twmod3), confint(stand.twmod3))
sr.s3 <- exp(est.stand3)

rt3 <- tibble(Outcome = "NCS",Model = "Sample 3",
              Predictor = c("(Intercept)","case","n_authors","int_collab","selfcit","mncs_j_high","mncs_j_med"),
              Estimate = s.s3$coefficients[,1],
              StdError = s.s3$coefficients[,2],
              ExpEstimate = sr.s3[,1],
              ExpLCL = sr.s3[,2],
              EXPUCL = sr.s3[,3])

mq.rt.final <- rbind(rt1,rt2,rt3)
save(mq.rt.final,file="regression_table_mncs_quantile.Rdata")