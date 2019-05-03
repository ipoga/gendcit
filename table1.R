require(tidyverse)

# TABLE 1 DATA

# Overall
fd %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))

# Specialty

# Prestige

fd %>%
  group_by(prestige) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))

# Geo

fd %>%
  filter(arab == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(commonwealth_is == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(e_asia == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(lat_am == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(north_am == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(oceania == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(sce_europe == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(ss_africa == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(sw_asia == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))
fd %>%
  filter(w_europe == 1) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both))

# Gender specifications over time

fd %>%
  group_by(py) %>%
  summarize(n = n())
# A number of papers (n = 76) have publication dates back in 2007 despite sampling from 2018. 
# There is no reason to exclude these from the overall analyses, but they are not meaningful
# in the time series below.

fd %>%
  filter(py > 2007) %>%
  group_by(py) %>%
  summarize(mfw = mean(f_w), mff = mean(f_first),mfl = mean(f_last), mfb = mean(f_both)) %>%
  gather("variable","value",c("mfw","mff","mfl","mfb")) %>%
  ggplot(aes(x = py, y = value, group = variable, color = variable)) + 
  geom_point() + geom_line(lty = 3) + 
  theme_bw() + 
  scale_y_continuous("Proportion of women authors", limits=c(0,.5)) + 
  scale_x_continuous("Publication year") +
  scale_color_hue("Author position",labels=c("First & Last","First","Last","Total weight"))
