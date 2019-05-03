#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Overlap
# AUTHOR  : Jens Peter Andersen
# EMAIL   : jpa@ps.au.dk
# UPDATED : 2019-04-29
#----------------------------------------------------------
# CONTRIBUTORS : name <email>
#----------------------------------------------------------
# PURPOSE : Generate plot of overlapping NCS densities, 
#           calculate overlap scores.
#----------------------------------------------------------

require(tidyverse)
require(overlapping)
require(scales)
src("dataload.R")

cd <- rbind(t_first,t_last,t_both)

mcd <- cd %>%
  group_by(sample,case) %>%
  summarize(mncs = mean(ncs),se_ncs = sd(ncs)/sqrt(length(ncs)), n = n(), sigma = sd(ncs),p_uncited=mean(as.numeric(uncited)-1))

cd %>%
  ggplot(aes(x=(ncs+0.001),fill=as.factor(case))) + geom_density(alpha=.7,lty = 0) +
  facet_grid(sample ~ .) +
  scale_x_log10("NCS, log-scale",breaks=c(.001,.01,.1,0,1,10,100)) + 
  scale_y_continuous("Empirical density function") + 
  scale_fill_manual("Gender",labels=c("Men","Women"),values=c("#007fc4","#f8766d")) +
  theme_bw() + 
  annotation_logticks(size=.2,sides="b")+ theme(legend.position="bottom") + 
  geom_vline(data = filter(mcd, sample == "Sample 1: F_First"), aes(xintercept = mncs,color=case), lty = 2, size=.8) +
  geom_vline(data = filter(mcd, sample == "Sample 2: F_Last"), aes(xintercept = mncs,color=case), lty = 2, size=.8) +
  geom_vline(data = filter(mcd, sample == "Sample 3: F_Both"), aes(xintercept = mncs,color=case), lty = 2, size=.8) + 
  scale_color_manual("Gender",labels=c("Men","Women"),values=c("#007fc4","#f8766d"))
ggsave("density_log_facet.png",dpi=300,width=7,height=5)


#----------------------------------------------------------
# Table 2

d1 <- t_first %>% filter(case == 1) %>% select(selfcit)
d2 <- t_first %>% filter(case == 0) %>% select(selfcit)
o <- overlap(list(d1$selfcit,d2$selfcit))
o$OV
d1 <- t_first %>% filter(case == 1) %>% select(mncs_journal)
d2 <- t_first %>% filter(case == 0) %>% select(mncs_journal)
o <- overlap(list(d1$mncs_journal,d2$mncs_journal))
o$OV
d1 <- t_last %>% filter(case == 1) %>% select(selfcit)
d2 <- t_last %>% filter(case == 0) %>% select(selfcit)
o <- overlap(list(d1$selfcit,d2$selfcit),n=100000)
o$OV
d1 <- t_last %>% filter(case == 1) %>% select(mncs_journal)
d2 <- t_last %>% filter(case == 0) %>% select(mncs_journal)
o <- overlap(list(d1$mncs_journal,d2$mncs_journal))
o$OV
d1 <- t_both %>% filter(case == 1) %>% select(selfcit)
d2 <- t_both %>% filter(case == 0) %>% select(selfcit)
o <- overlap(list(d1$selfcit,d2$selfcit))
o$OV
d1 <- t_both %>% filter(case == 1) %>% select(mncs_journal)
d2 <- t_both %>% filter(case == 0) %>% select(mncs_journal)
o <- overlap(list(d1$mncs_journal,d2$mncs_journal))
o$OV

t_last %>% ggplot(aes(x=selfcit,color=case)) + geom_density() + scale_x_log10()

#-------- OLD STUFF _ REVISE!!!

m2 <- mean(fd$ncs[fd$f_first==2])
m1 <- mean(fd$ncs[fd$f_first==1])
d1 <- fd$ncs[fd$f_first==2]
d2 <- fd$ncs[fd$f_first==1]
ovl <- overlapEst(d1,d2,type="Dhat4")
ggplot(fd,aes(x=ncs,color=as.factor(f_first))) + geom_density(size=.3) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.3) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.3) +
  scale_x_log10("NCS, log10-scale",limits=c(0.01,100)) + 
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + annotation_logticks(size=.2,sides="b")+ theme(legend.position="bottom") + ggtitle("Unmatched, case: female first-author")
ggsave("density_all_ffirst.png", width=7,height=5)

m2 <- mean(fd$ncs[fd$f_last==2])
m1 <- mean(fd$ncs[fd$f_last==1])
ggplot(fd,aes(x=ncs,color=as.factor(f_last))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_log10("NCS, log10-scale",limits=c(0.01,100)) + 
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + annotation_logticks(size=.2)+ theme(legend.position="bottom") + ggtitle("Unmatched, case: female last-author")
ggsave("density_all_flast.png", width=7,height=5)

m2 <- mean(fd$ncs[fd$f_both==2])
m1 <- mean(fd$ncs[fd$f_both==1])
ggplot(fd,aes(x=ncs,color=as.factor(f_both))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_log10("NCS, log10-scale",limits=c(0.01,100)) + 
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + annotation_logticks(size=.2)+ theme(legend.position="bottom") + ggtitle("Unmatched, case: female first-last-author")
ggsave("density_all_fboth.png", width=7,height=5)

m1 <- mean(t_first$ncs[t_first$case==0])
m2 <- mean(t_first$ncs[t_first$case==1])
ggplot(t_first,aes(x=ncs,color=as.factor(case))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_log10("NCS, log10-scale",limits=c(0.01,100)) + 
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + annotation_logticks(size=.2)+ theme(legend.position="bottom") + ggtitle("Matched, case: female first-author")
ggsave("density_match_ffirst_log.png", width=7,height=5)

m1 <- mean(t_last$ncs[t_last$case==0])
m2 <- mean(t_last$ncs[t_last$case==1])
ggplot(t_last,aes(x=ncs,color=as.factor(case))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_log10("NCS, log10-scale",limits=c(0.01,100)) + 
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + annotation_logticks(size=.2)+ theme(legend.position="bottom") + ggtitle("Matched, case: female last-author")
ggsave("density_match_flast_log.png", width=7,height=5)

m1 <- mean(t_last$ncs[t_last$case==0])
m2 <- mean(t_last$ncs[t_last$case==1])
ggplot(t_last,aes(x=ncs,color=as.factor(case))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_log10("NCS, log10-scale",limits=c(0.01,100)) + 
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + annotation_logticks(size=.2)+ theme(legend.position="bottom") + ggtitle("Matched, case: female last-author")
ggsave("density_match_flast_log.png", width=7,height=5)

m1 <- mean(t_both$ncs[t_both$case==0])
m2 <- mean(t_both$ncs[t_both$case==1])
ggplot(t_both,aes(x=ncs,color=as.factor(case))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_log10("NCS, log10-scale",limits=c(0.01,100)) + 
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + annotation_logticks(size=.2)+ theme(legend.position="bottom") + ggtitle("Matched, case: female first- and last-author")
ggsave("density_match_fboth_log.png", width=7,height=5)

m1 <- mean(t_first$ncs[t_first$case==0])
m2 <- mean(t_first$ncs[t_first$case==1])
ggplot(t_first,aes(x=ncs,color=as.factor(case))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_continuous("NCS, linear scale",limits=c(0,10)) +
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + theme(legend.position="bottom") + ggtitle("Matched, case: female first-author")
ggsave("density_match_ffirst_lin.png", width=7,height=5)

m1 <- mean(t_last$ncs[t_last$case==0])
m2 <- mean(t_last$ncs[t_last$case==1])
p5 <- ggplot(t_last,aes(x=ncs,color=as.factor(case))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_continuous("NCS, linear scale",limits=c(0,10)) +
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw()+ theme(legend.position="bottom") + ggtitle("Matched, case: female last-author")
ggsave("density_match_flast_lin.png", width=7,height=5)

m1 <- mean(t_both$ncs[t_both$case==0])
m2 <- mean(t_both$ncs[t_both$case==1])
p6 <- ggplot(t_both,aes(x=ncs,color=as.factor(case))) + geom_density(size=.2) + 
  geom_vline(xintercept=1) +
  geom_vline(xintercept=m1,linetype="longdash",color="#f8766d",size=.2) +
  geom_vline(xintercept=m2,linetype="longdash",color="#007fc4",size=.2) +
  scale_x_continuous("NCS, linear scale",limits=c(0,10)) +
  scale_y_continuous("Density",limits=c(0,1)) +
  scale_color_manual("Gender",labels=c("Case","Control"),values=c("#f8766d","#007fc4")) +
  theme_bw() + theme(legend.position="bottom") + ggtitle("Matched, case: female first- and last-author")
ggsave("density_match_fboth_lin.png", width=7,height=5)





fd$f_first[fd$f_first==2] <- 0
fd$f_last[fd$f_last==2] <- 0
fd$f_both[fd$f_both==2] <- 0

tab1 <- data.frame(spec = c("Surgical","Medical","Hospital","Basic","Pediatric"),f_w=0,f_first=0,f_last=0,f_both =0)
tmp <- subset(fd,ms_surgical == 1)
tab1$f_w[tab1$spec == "Surgical"] <- mean(tmp$f_w)
tab1$f_first[tab1$spec == "Surgical"] <- mean(tmp$f_first)
tab1$f_last[tab1$spec == "Surgical"] <- mean(tmp$f_last)
tab1$f_both[tab1$spec == "Surgical"] <- mean(tmp$f_both)

tmp <- subset(fd,ms_medical == 1)
tab1$f_w[tab1$spec == "Medical"] <- mean(tmp$f_w)
tab1$f_first[tab1$spec == "Medical"] <- mean(tmp$f_first)
tab1$f_last[tab1$spec == "Medical"] <- mean(tmp$f_last)
tab1$f_both[tab1$spec == "Medical"] <- mean(tmp$f_both)

tmp <- subset(fd,ms_hospital == 1)
tab1$f_w[tab1$spec == "Hospital"] <- mean(tmp$f_w)
tab1$f_first[tab1$spec == "Hospital"] <- mean(tmp$f_first)
tab1$f_last[tab1$spec == "Hospital"] <- mean(tmp$f_last)
tab1$f_both[tab1$spec == "Hospital"] <- mean(tmp$f_both)

tmp <- subset(fd,ms_basic == 1)
tab1$f_w[tab1$spec == "Basic"] <- mean(tmp$f_w)
tab1$f_first[tab1$spec == "Basic"] <- mean(tmp$f_first)
tab1$f_last[tab1$spec == "Basic"] <- mean(tmp$f_last)
tab1$f_both[tab1$spec == "Basic"] <- mean(tmp$f_both)

tmp <- subset(fd,ms_pediatric == 1)
tab1$f_w[tab1$spec == "Pediatric"] <- mean(tmp$f_w)
tab1$f_first[tab1$spec == "Pediatric"] <- mean(tmp$f_first)
tab1$f_last[tab1$spec == "Pediatric"] <- mean(tmp$f_last)
tab1$f_both[tab1$spec == "Pediatric"] <- mean(tmp$f_both)

tab1
