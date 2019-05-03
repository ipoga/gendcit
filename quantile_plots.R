#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Quantile plots
# AUTHOR  : Jens Peter Andersen
# EMAIL   : jpa@ps.au.dk
# UPDATED : 2019-04-29
#----------------------------------------------------------
# CONTRIBUTORS : name <email>
#----------------------------------------------------------
# PURPOSE : Generate plot of quantile distributions of self-
#           citations vs NCS and case cs MNCS Journal.
#----------------------------------------------------------

require(tidyverse)
require(patchwork)
source("dataload.R")

#----------------------------------------------------------
# Self-citations as function of NCS

quants <- seq(.05,1,.05)

q1 <- quantile(t_first$ncs,quants)
q2 <- quantile(t_last$ncs,quants)
q3 <- quantile(t_both$ncs,quants)
qout <- tibble(quant = quants,p1=0,p2=0,p3=0)
for (i in 1:nrow(qout)) {
  if (i == 1) {
    qout$p1[i] <- mean(t_first$selfcit_prop[t_first$ncs <= q1[i]])
    qout$p2[i] <- mean(t_last$selfcit_prop[t_last$ncs <= q2[i]])
    qout$p3[i] <- mean(t_both$selfcit_prop[t_both$ncs <= q3[i]])
  } else {
    qout$p1[i] <- mean(t_first$selfcit_prop[t_first$ncs <= q1[i] & t_first$ncs > q1[i-1]])
    qout$p2[i] <- mean(t_last$selfcit_prop[t_last$ncs <= q2[i] & t_last$ncs > q2[i-1]])
    qout$p3[i] <- mean(t_both$selfcit_prop[t_both$ncs <= q3[i] & t_both$ncs > q3[i-1]])
  }
}

qb <- tibble(x = quants,p1=q1,p2=q2,p3=q3)

p1 <- qout %>%
  gather(key="Sample",value="Indicator",p1,p2,p3) %>%
  ggplot(aes(x=quant,y=Indicator,color=Sample)) + geom_line(lty=2) + geom_point(shape=1) +
  scale_y_continuous("Mean proportion of self-citations per paper") +
  scale_x_continuous("Quantiles, NCS",minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .1)) + 
  theme_bw()

p2 <- qb %>%
  gather(key="Sample",value="Indicator",p1,p2,p3) %>%
  ggplot(aes(x=x,y=Indicator,color=Sample)) + geom_line(lty=2) + geom_point(shape=1) +
  scale_y_log10("Upper bounds of NCS per quantile, log-scale") + 
  scale_x_continuous("Quantiles, NCS",minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .1)) + 
  scale_color_discrete(labels=c("Sample 1","Sample 2","Sample 3")) +
  theme_bw() + 
  annotation_logticks(base = 10, sides = "l")

#----------------------------------------------------------
# Case as function of MNCS Journal

quants <- seq(.05,1,.05)

q1 <- quantile(t_first$mncs_journal,quants)
q2 <- quantile(t_last$mncs_journal,quants)
q3 <- quantile(t_both$mncs_journal,quants)
qout <- tibble(quant = quants,p1=0,p2=0,p3=0,m1=0,m2=0,m3=0)
for (i in 1:nrow(qout)) {
  if (i == 1) {
    qout$p1[i] <- mean(as.numeric(t_first$case[t_first$mncs_journal <= q1[i]]) - 1)
    qout$p2[i] <- mean(as.numeric(t_last$case[t_last$mncs_journal <= q2[i]]) - 1)
    qout$p3[i] <- mean(as.numeric(t_both$case[t_both$mncs_journal <= q3[i]]) - 1)
    qout$m1[i] <- mean(t_first$ncs[t_first$mncs_journal <= q1[i]])
    qout$m2[i] <- mean(t_last$ncs[t_last$mncs_journal <= q2[i]])
    qout$m3[i] <- mean(t_both$ncs[t_both$mncs_journal <= q3[i]])
  } else {
    qout$p1[i] <- mean(as.numeric(t_first$case[t_first$mncs_journal <= q1[i] & t_first$mncs_journal > q1[i-1]]) - 1)
    qout$p2[i] <- mean(as.numeric(t_last$case[t_last$mncs_journal <= q2[i] & t_last$mncs_journal > q2[i-1]]) - 1)
    qout$p3[i] <- mean(as.numeric(t_both$case[t_both$mncs_journal <= q3[i] & t_both$mncs_journal > q3[i-1]]) - 1)
    qout$m1[i] <- mean(t_first$ncs[t_first$mncs_journal <= q1[i] & t_first$mncs_journal > q1[i-1]])
    qout$m2[i] <- mean(t_last$ncs[t_last$mncs_journal <= q2[i] & t_last$mncs_journal > q2[i-1]])
    qout$m3[i] <- mean(t_both$ncs[t_both$mncs_journal <= q3[i] & t_both$mncs_journal > q3[i-1]])
  }
}

qb <- tibble(x = quants,p1=q1,p2=q2,p3=q3)

p3 <- qout %>%
  gather(key="Sample",value="Indicator",p1,p2,p3) %>%
  ggplot(aes(x=quant,y=Indicator,color=Sample)) + geom_line(lty=2) + geom_point(shape=1) +
  scale_y_continuous("Percentage of case") + 
  scale_x_continuous("Quantiles, MNCS_Journal",minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .1)) + 
  scale_color_discrete(labels=c("Sample 1","Sample 2","Sample 3")) +
  theme_bw()

p4 <- qb %>%
  gather(key="Sample",value="Indicator",p1,p2,p3) %>%
  ggplot(aes(x=x,y=Indicator,color=Sample)) + geom_line(lty=2) + geom_point(shape=1) +
  scale_y_continuous("Upper bounds of MNCS Journal per quantile, log-scale",trans="log10") + 
  scale_x_continuous("Quantiles, MNCS_journal",minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .1)) + 
  theme_bw() + 
  annotation_logticks(base = 10, sides = "l")

p5 <- qout %>%
  gather(key="Sample",value="Indicator",m1,m2,m3) %>%
  ggplot(aes(x=quant,y=Indicator,color=Sample)) + geom_line(lty=2) + geom_point(shape=1) +
  scale_y_continuous("Mean NCS per quantile, log-scale",trans="log10") + 
  scale_x_continuous("Quantiles, MNCS_journal",minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .1)) + 
  theme_bw() + 
  annotation_logticks(base = 10, sides = "l")

p1 * theme(legend.position = "none") + p2 * theme(legend.position = "bottom") + plot_layout(ncol=1)
ggsave("quantile_plots_a.png",dpi=300,height=5,width=7)

p3 * theme(legend.position = "bottom") + (p4 * theme(legend.position = "none") + p5 * theme(legend.position = "none")) + plot_layout(ncol=1)
ggsave("quantile_plots_b.png",dpi=300,height=5,width=7)
