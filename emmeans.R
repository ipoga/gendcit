#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Estimated Marginal Means
# AUTHOR  : Jesper W. Schneider
# EMAIL   : jws@ps.au.dk
# UPDATED : 2019-04-29
#----------------------------------------------------------
# CONTRIBUTORS : Jens Peter Andersen <jpa@ps.au.dk>
#----------------------------------------------------------
# PURPOSE : Calculating and plotting estimated marginal 
#           means on NCS
#----------------------------------------------------------

require(emmeans)
require(statmod)
require(tweedie)
source("dataload.R")

#----------------------------------------------------------

# tweedie model
twmod1 <- glm(ncs ~ factor(case) + n_authors + int_collab + selfcit + mncs_journal, data = t_first, family = tweedie(var.power = 1.65, link.power = power.link))

# marginal means
twmod1.emm <- emmeans(twmod1, "case")
e1 <- summary(twmod1.emm)

# tweedie model
twmod2 <- glm(ncs ~ factor(case) + n_authors + int_collab + selfcit + mncs_journal, data = t_last, family = tweedie(var.power = 1.72, link.power = power.link))

# marginal means
twmod2.emm <- emmeans(twmod2, "case")
e2 <- summary(twmod2.emm)

# tweedie model
twmod3 <- glm(ncs ~ factor(case) + n_authors + int_collab + selfcit + mncs_journal, data = t_both, family = tweedie(var.power = 1.60, link.power = power.link))

# marginal means
twmod3.emm <- emmeans(twmod3, "case")
e3 <- summary(twmod3.emm)

e1$sample <- "Sample 1"
e2$sample <- "Sample 2"
e3$sample <- "Sample 3"
emd <- rbind(e1,e2,e3)
emd$emmean <- exp(emd$emmean)
emd$asymp.LCL <- exp(emd$asymp.LCL)
emd$asymp.UCL <- exp(emd$asymp.UCL)
emd <- emd %>% arrange(sample,desc(case))
emd$seq <- as.factor(seq(1,6,1))

emd %>%
  ggplot(aes(x = seq, y = emmean, group = sample)) + geom_point() + 
  geom_line(lty = 2) + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax =asymp.UCL), width = .25) +
  scale_x_discrete("",breaks=c(1,2,3,4,5,6),labels=c("Female first","Male first","Female last","Male last","Female first & last","Other constellations")) + 
  scale_y_continuous("Predicted NCS",limits=c(0.95,1.15)) +
  theme_bw()
ggsave("emmeans.png",dpi=300,width=8,height=3)
