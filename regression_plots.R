#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Regression plots
# AUTHOR  : Jens Peter Andersen
# EMAIL   : jpa@ps.au.dk
# UPDATED : 2019-04-26
#----------------------------------------------------------
# CONTRIBUTORS : name <email>
#----------------------------------------------------------
# PURPOSE : Generating plots from Tweedie regressions for
#           use in paper.
#----------------------------------------------------------

require(tidyverse)
load("regression_output.Rdata")
load("log_regression_output.Rdata")

#----------------------------------------------------------
# IRR Tweedie plot

lbl <- c ("Intercept","Case","N authors","Int. Collab.","Self-citations","MNCS Journal")

reg.stand1 <- tibble(v = seq(1,6,1),labels=lbl,irr=irr.stand1[,1],lo=irr.stand1[,2],hi=irr.stand1[,3],sample="Sample 1")
reg.stand2 <- tibble(v = seq(1,6,1),labels=lbl,irr=irr.stand2[,1],lo=irr.stand2[,2],hi=irr.stand2[,3],sample="Sample 2")
reg.stand3 <- tibble(v = seq(1,6,1),labels=lbl,irr=irr.stand3[,1],lo=irr.stand3[,2],hi=irr.stand3[,3],sample="Sample 3")
reg.stand <- rbind(reg.stand1,reg.stand2,reg.stand3)
reg.stand$labels[reg.stand$v == 2] <- c("F_First","F_Last","F_both")

reg.stand %>%
  filter(v > 1) %>%
  ggplot(aes(x=as.factor(labels),y=irr)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lo,ymax=hi),size=.25,width=.5) + 
  facet_grid(sample~., scales = "free", space = "free") + 
  scale_y_continuous("Exponentiated Tweedie coefficients on NCS, 95% confidence intervals",limits=c(.5,2),trans="log2",breaks=seq(.5,2,.1)) + 
  scale_x_discrete("Predictors (x)") + 
  geom_hline(yintercept=1,color="red",lty=2) +
  theme_bw() + 
  coord_flip()
ggsave("tweedie_fig_standard.png",width=7,height=6,dpi=600)
#----------------------------------------------------------
# Standardized OLS regression plot

slbl <- c ("Intercept","N authors","Int. Collab.","Self-citations","MNCS Journal")

sreg.stand1 <- tibble(v = seq(1,5,1),labels=slbl,irr=slr.stand1[,1],lo=slr.stand1[,2],hi=slr.stand1[,3],sample="Sample 1")
sreg.stand2 <- tibble(v = seq(1,5,1),labels=slbl,irr=slr.stand2[,1],lo=slr.stand2[,2],hi=slr.stand2[,3],sample="Sample 2")
sreg.stand3 <- tibble(v = seq(1,5,1),labels=slbl,irr=slr.stand3[,1],lo=slr.stand3[,2],hi=slr.stand3[,3],sample="Sample 3")
sreg.stand <- rbind(sreg.stand1,sreg.stand2,sreg.stand3)

sreg.stand %>%
  filter(v > 1) %>%
  ggplot(aes(x=as.factor(labels),y=irr)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lo,ymax=hi),size=.1,width=.5) + 
  facet_grid(sample~., scales = "free", space = "free") + 
  scale_y_continuous("Odds ratios on NCS, 95% confidence intervals",limits=c(.5,2),trans="log2",breaks=seq(.5,2,.1)) + 
  scale_x_discrete("Predictors (x)") + 
  geom_hline(yintercept=1,color="red",lty=2) +
  theme_bw() + 
  coord_flip()
ggsave("logreg_fig_standard.png",width=7,height=6,dpi=600)
