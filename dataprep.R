#----------------------------------------------------------
# PROJECT : Gender differences in citation impact
# FILE    : Data preparation
# AUTHOR  : Jens Peter Andersen
# EMAIL   : jpa@ps.au.dk
# UPDATED : 2019-04-26
#----------------------------------------------------------
# CONTRIBUTORS : name <email>
#----------------------------------------------------------
# PURPOSE : Reading raw data, creating matched samples
#----------------------------------------------------------

require(Matching)
require(tidyverse)

# Read main dataset (fd)
load("full_data_prepped.Rdata")

# f_first matching
Tr <- fd$f_first
m <- glm(Tr~prestige+geo_first+geo_last+specialty+f_last,data=fd,na.action = na.exclude)
m <- Match(X=m$fitted,Tr=Tr,M=1,exact=T,ties=F)

t_first_treated <- fd[m$index.treated,]
t_first_control <- fd[m$index.control,]

# f_last matching
Tr <- fd$f_last
m <- glm(Tr~prestige+geo_first+geo_last+specialty+f_first,data=fd)
m <- Match(X=m$fitted,Tr=Tr,M=1,exact=T,ties=F)

t_last_treated <- fd[m$index.treated,]
t_last_control <- fd[m$index.control,]

# f_both matching
Tr <- fd$f_both
m <- glm(Tr~prestige+geo_first+geo_last+specialty,data=fd)
m <- Match(X=m$fitted,Tr=Tr,M=1,exact=T,ties=F)

t_both_treated <- fd[m$index.treated,]
t_both_control <- fd[m$index.control,]

# Combine datasets

t_first_treated$case <- 1
t_first_control$case <- 0
t_last_treated$case <- 1
t_last_control$case <- 0
t_both_treated$case <- 1
t_both_control$case <- 0

t_first <- rbind(t_first_treated,t_first_control)
t_last <- rbind(t_last_treated,t_last_control)
t_both <- rbind(t_both_treated,t_both_control)

t_first$sample <- "Sample 1: F_First"
t_last$sample <- "Sample 2: F_Last"
t_both$sample <- "Sample 3: F_Both"

save(t_first,file="t_first.Rdata")
save(t_last,file="t_last.Rdata")
save(t_both,file="t_both.Rdata")
